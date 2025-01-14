module ScenarioViewer


#nowarn "9"

open System
open System.IO
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open FSharp.Data.Adaptive
open Aardvark.Rendering.Text

open PRo3D.Extensions
open PRo3D.Extensions.FSharp

open CommandLine
open CommandLine.Text

open Aardvark.FontProvider

open PRo3D.SPICE

do Aardvark.Base.Aardvark.UnpackNativeDependencies(typeof<CooTransformation.RelState>.Assembly)

type Font = GoogleFontProvider<"Roboto Mono">

module Shaders =

    open FShade
    open Aardvark.Rendering.Effects



type AdaptiveLine() =
    let data = ResizeArray()
    let arr = cval [||]
    let mutable last = None

    member x.AddPos(p : V3d) =
        last <- Some p
        data.Add(p)
        arr.Value <- data.ToArray()
        
    member x.Positions = arr :> aval<_>

    member x.Last = last

    member x.Clear() = arr.Value <- [||]; data.Clear()
   
type BodyState = 
    {
        name : string
        pos : cval<V3d>
        ndcSize : cval<V2d>
        history : AdaptiveLine
        radius : float<m>
    }

type Arguments = 
    {
        // start time given as UTC string (parsable by System.DateTime)
        // e.g. "2025-03-10 19:08:12.60"
        [<Option('t', "time", Required = true, HelpText = "Simulation start time.")>]
        startTime : string

        // how much is 1 second screen time in the simulation?
        // e.g. 1 means, it takes you 1 day observing one earth rotation ;)
        [<Option('f', "timefactor", Required = true, HelpText = "how much is 1 second screen time in the simulation?, e.g. 1 means, it takes you 1 day observing one earth rotation ;)")>]
        timeToSimluationTime : double

        // camera speed in m/s
        [<Option('s', "speed", Required = true, HelpText = "Camera movement speed in m/s")>]
        cameraSpeed : double<m / s>

        // observer body (will be center of the world)
        [<Option('o', "observer", Required = true, HelpText = "observer body (will be center of the world), e.g. earth, moon or mars.")>]
        observerBody : string 

        // SPICE name of reference frame to base the world on.
        [<Option('r', "referenceframe", Required = true, HelpText = "SPICE reference frame, e.g. ECLIPJ2000")>]
        referenceFrame : string

        [<Option('k', "spiceKernel", Required = false, Default = "", HelpText = "Path to SPICE metakernel")>]
        spiceKernel : string
    }

let scenarios = 
    [|
        { 
            startTime = "2025-03-10 19:08:12.60"; 
            timeToSimluationTime = (31.0 * 86400.0) / 150.0
            cameraSpeed = 100000000.0<m/s>
            observerBody = "mars"; 
            referenceFrame = "ECLIPJ2000" 
            spiceKernel = ""
        }
    |]



type CameraMode =
    | FreeFly
    | Orbit


let main (argv : array<string>) = 
    
    let args = 
        if argv.Length = 0 then
            scenarios[0]
        else
          let result = CommandLine.Parser.Default.ParseArguments<Arguments>(argv)
          match result with
          | :? Parsed<Arguments> as parsed -> parsed.Value
          | :? NotParsed<Arguments> as notParsed -> failwithf "could not parse arguments: %A" notParsed.Errors
          | _ -> failwithf "%A" result


    let observerBody = 
        match getBodySource args.observerBody with
        | Some s -> cval s
        | None -> failwith "observer not found list of available body sources"

    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(4)


    use _ = SPICE.initializeAndLoadKernels args.spiceKernel
                 
    let time = cval (DateTime.Parse args.startTime)

    let observer = observerBody |> AVal.map (fun o -> o.name)  
    let referenceFrame = args.referenceFrame

    let getLookAt (o : BodyDesc) = 
        match CooTransformation.getRelState o.goodObserver "SUN" o.name time.Value referenceFrame with
        | None -> failwith "could not get initial camera view"
        | Some targetState -> 
             CameraView.lookAt targetState.pos V3d.Zero V3d.OOI 

    let initialView = 
        getLookAt observerBody.Value |> cval

    let speed = cval (float args.cameraSpeed)
    let cameraMode = cval CameraMode.Orbit

    let view = 
        adaptive {
            let! mode = cameraMode
            let! initialView = initialView
            match mode with
            | CameraMode.FreeFly -> 
                let! currentSpeed = speed
                return! 
                    DefaultCameraController.controlExt (float currentSpeed) win.Mouse win.Keyboard win.Time initialView
            | CameraMode.Orbit -> 
                return!
                    AVal.integrate 
                        initialView win.Time [
                            DefaultCameraController.controllScrollWithSpeed speed win.Mouse win.Time
                            DefaultCameraController.controlOrbitAround win.Mouse (AVal.constant <| V3d.Zero)
                        ]
        }


    let distanceSunPluto = 5906380000.0 * 1000.0
    let frustum = win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 1000.0 distanceSunPluto (float s.X / float s.Y))
    let aspect = win.Sizes |> AVal.map (fun s -> float s.X / float s.Y)
    let scale = aspect |> AVal.map (fun aspect -> Trafo3d.Scale(V3d(1.0, aspect, 1.0)))


    let projTrafo = frustum |> AVal.map Frustum.projTrafo
    let viewProj = 
        (view, projTrafo) ||> AVal.map2 (fun view projTrafo -> 
            CameraView.viewTrafo view * projTrafo
        )

    let font = Font.Font


    let info = 
        let content = time |> AVal.map (fun t -> sprintf "%s" (CooTransformation.Time.toUtcFormat t))
        Sg.text font C4b.Gray content 
        |> Sg.trafo (scale |> AVal.map (fun s -> Trafo3d.Scale(0.1) * s * Trafo3d.Translation(-0.95,-0.95,0.0)))
     
    let help = 
        let content =
            String.concat Environment.NewLine [
                "<c>    : switch camera mode"
                "<t>    : reset time"
                "<n>    : switch observer"
            ]
        Sg.text font C4b.Gray (AVal.constant content)
        |> Sg.trafo (scale |> AVal.map (fun s -> Trafo3d.Scale(0.05) * s * Trafo3d.Translation(-0.95, 0.90,0.0)))


    let coordinateCross (size : float) (modelTrafo : aval<Trafo3d>)=  
        let lines (lines : array<Line3d*C4f>) =
            lines |> Array.collect(fun (l,c) -> [|l.P0; l.P1|]) |> Seq.toArray

        let cross = [|
                Line3d(V3d.Zero, V3d.XAxis * size), C4f.Red
                Line3d(V3d.Zero, V3d.YAxis * size), C4f.Green
                Line3d(V3d.Zero, V3d.ZAxis * size), C4f.Blue
            |] 

        let vertices =
            (modelTrafo, viewProj) ||> AVal.map2 (fun m vp -> 
                let mvp = m * vp
                lines cross |> Array.map (fun v -> 
                    let p = mvp.Forward.TransformPosProjFull v 
                    p |> V4f
                )
            )

        Sg.draw IndexedGeometryMode.LineList
        |> Sg.vertexAttribute DefaultSemantic.Positions vertices
        |> Sg.vertexArray     DefaultSemantic.Colors    (cross |> Array.collect (fun (l, c) -> [| c; c|]))
        |> Sg.shader { 
            do! DefaultSurfaces.vertexColor
        }

    let heraCoordinateCross = 
        coordinateCross 1000000000.0 (AVal.constant Trafo3d.Identity)

    let sg =
        Sg.ofList [info;  heraCoordinateCross; help ] 



    win.Keyboard.KeyDown(Keys.R).Values.Add(fun _ -> 
        transact (fun _ -> 
            match CooTransformation.getRelState "MARS" "SUN" (AVal.force observer) time.Value referenceFrame with
            | Some targetState -> 
                let rot = targetState.rot
                let t = Trafo3d.FromBasis(rot.C0, rot.C1, -rot.C2, V3d.Zero)
                initialView.Value <- CameraView.ofTrafo t.Inverse
            | _ -> ()
        )
    )

    win.Keyboard.KeyDown(Keys.C).Values.Add(fun _ -> 
        transact (fun _ -> 
            cameraMode.Value <-
                match cameraMode.Value with
                | CameraMode.FreeFly -> CameraMode.Orbit
                | CameraMode.Orbit -> CameraMode.FreeFly
        )
    )

    win.Keyboard.KeyDown(Keys.N).Values.Add(fun _ -> 
        transact (fun _ -> 
            ()
        )
    )

    let animationStep () =
        ()

    let s = 
        let sw = Diagnostics.Stopwatch.StartNew()
        animationStep()
        let mutable lastFrame = None
        win.AfterRender.Add(fun _ -> 
            transact (fun _ -> 
                let showCoordinateFrame = false
                if showCoordinateFrame then
                    let observer = observer.GetValue()
                    match CooTransformation.getRelState "MARS" "SUN" observer time.Value referenceFrame with
                    | Some targetState -> 
                        let rot = targetState.rot
                        let t = Trafo3d.FromBasis(rot.C0, rot.C1, rot.C2, V3d.Zero)
                        //spacecraftTrafo.Value <- t
                        ()
                    | _ -> ()

                let dt = 
                    match lastFrame with
                    | None -> TimeSpan.Zero
                    | Some l -> sw.Elapsed - l

                time.Value <- time.Value + dt * args.timeToSimluationTime
                animationStep()
                lastFrame <- Some sw.Elapsed
            )
        )

    
    let task =
        app.Runtime.CompileRender(win.FramebufferSignature, sg)

    win.RenderTask <- task
    win.Run()
    0