module RenderingDemo


#nowarn "9"

open System
open System.Threading
open FSharp.NativeInterop
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

do Aardvark.Base.Aardvark.UnpackNativeDependencies(typeof<CooTransformation.RelState>.Assembly)

type Font = GoogleFontProvider<"Roboto Mono">

module Shaders =

    open FShade
    open Aardvark.Rendering.Effects

    type Vertex = 
        {
            [<Position>] p: V4d
            [<PointSize>] s : float
            [<Color>] c: V4d
            [<PointCoord>] tc: V2d
            [<Semantic("Size")>] ndcSize : float
        }


    let private diffuseSampler =
        sampler2d {
            texture uniform.DiffuseColorTexture
            filter Filter.Anisotropic
            maxAnisotropy 16
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let splatPoint (v : Vertex) = 
        vertex {
            return { v with s = v.ndcSize }
        }

    let round (v : Vertex) =
        fragment {
            let c = v.tc * 2.0 - V2d.II
            let p = V3d(c, 1.0 - Vec.length c.XY)
            let p = p.XZY
            let thetha = acos (p.Z) / Math.PI
            let phi = ((float (sign p.Y)) * acos (p.X / Vec.length p.XY)) / (Math.PI * 2.0)
            // could be used for texturing
            let t = v.c //diffuseSampler.Sample(V2d(phi, thetha))
            let f = Vec.dot c c - 1.0
            if f > 0.0 then discard()
            return { v with c = t }// * Vec.dot V3d.OOI p }
        }

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
   

[<Measure>] type m
[<Measure>] type km
[<Measure>] type s
let meterToKilometers (m : float<m>) = m / 1000.0<m / km> 
let kmToMeters (m : float<km>) = m * 1000.0<m / km> 

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


type BodyDesc = 
    {
        // body name in spice nomenclature
        name: string
        // visual appearance ;)
        color : C4f
        // diameter
        diameter : float<km>
        // good observer (when setting the camera the body, another body which can be used to look at the body.
        goodObserver : string
    }


let bodySources = 
    [|  { name = "sun"        ; color = C4f.White;     diameter = 1392700.0<km>;  goodObserver = "mercury" }
        { name = "mercury"    ; color = C4f.Gray;      diameter = 12742.0<km>;    goodObserver = "earth"   }
        { name = "venus"      ; color = C4f.AliceBlue; diameter = 12742.0<km>;    goodObserver = "earth"   }
        { name = "earth"      ; color = C4f.Blue;      diameter = 12742.0<km>;    goodObserver = "moon"    }
        { name = "moon"       ; color = C4f.Beige;     diameter = 34748.0<km>;    goodObserver = "earth"   }
        { name = "mars"       ; color = C4f.Red;       diameter = 6779.0<km>;     goodObserver = "phobos"  }
        { name = "phobos"     ; color = C4f.Red;       diameter = 22.533<km>;     goodObserver = "mars"    }
        { name = "deimos"     ; color = C4f.Red;       diameter = 12.4<km>;       goodObserver = "mars"    }
        { name = "HERA"       ; color = C4f.Magenta;   diameter = 0.00001<km>;    goodObserver = "mars"    }
        { name = "HERA_AFC-1" ; color = C4f.White;     diameter = 0.00001<km>;    goodObserver = "mars"    }
    |]   

let getBodySource (name : string) = bodySources |> Array.tryFind (fun s -> s.name = name)

type CameraMode =
    | FreeFly
    | Orbit


[<EntryPoint>]
let main argv = 
    
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


    use _ = 
        let r = CooTransformation.Init(true, Path.Combine(".", "logs", "CooTrafo.Log"), 2, 2)
        if r <> 0 then failwith "could not initialize CooTransformation lib."
        { new IDisposable with member x.Dispose() = CooTransformation.DeInit() }

    
    let spiceFileName = 
        if args.spiceKernel.IsNullOrEmpty() then
            let spiceRoot = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "spice_kernels")
            Path.Combine(spiceRoot, @"kernels/mk/hera_crema_2_0_LPO_ECP_PDP.tm")
        else
            args.spiceKernel

    if File.Exists spiceFileName then Log.line "using: %s" spiceFileName
    else
        Log.error "spice kernel: %s does not exist" spiceFileName
        failwith "could not load spice kernels."

    System.Environment.CurrentDirectory <- Path.GetDirectoryName(spiceFileName)
    let r = CooTransformation.AddSpiceKernel(spiceFileName)
    if r <> 0 then failwith "could not add spice kernel"

                 
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

    let projTrafo = frustum |> AVal.map Frustum.projTrafo
    let viewProj = 
        (view, projTrafo) ||> AVal.map2 (fun view projTrafo -> 
            CameraView.viewTrafo view * projTrafo
        )

    let inNdcBox =
        let box = Box3d.FromPoints(V3d(-1,-1,-1),V3d(1,1,1))
        fun (p : V3d) -> box.Contains p

    let getProjPos (t : AdaptiveToken) (clip : bool) (pos : V3d) =
        let vp = viewProj.GetValue(t)
        let ndc = vp.Forward.TransformPosProj(pos)
        let box = Box3d.FromPoints(V3d(-1,-1,-1),V3d(1,1,1))
        if not clip || inNdcBox ndc then 
            V3d ndc |> Some
        else
            None

    let getCurrentProjPos (clip : bool) (pos : V3d) = 
        getProjPos AdaptiveToken.Top clip pos

    let bodies = 
        bodySources |> Array.map (fun b -> 
            { name = b.name; pos = cval V3d.Zero; history = AdaptiveLine(); ndcSize = cval V2d.OO; radius = kmToMeters b.diameter * 0.5 }
        )

    let font = Font.Font


    let animationStep () = 
        bodies |> Array.iter (fun b -> 
            let time = time.GetValue()
            let o = observer.GetValue()
            match CooTransformation.getRelState b.name "SUN" o time referenceFrame with
            | Some rel ->
                b.pos.Value <- rel.pos

                match getCurrentProjPos false rel.pos with
                | None -> ()
                | Some p -> 
                    match b.history.Last with
                    | Some l -> 
                        if Vec.distance p l > 0.005 then 
                            b.history.AddPos rel.pos
                    | None -> 
                        b.history.AddPos rel.pos
            | _ -> ()
        )

    let colors = bodySources |> Array.map (fun s  -> s.color) 
    let vertices = 
        AVal.custom (fun t -> 
            let w = win.Time.GetValue(t)
            time.GetValue(t) |> ignore
            let vp = viewProj.GetValue(t)
            bodies |> Array.map (fun b -> vp.Forward.TransformPosProj (b.pos.GetValue(t)))
        )
    let sizes =
        AVal.custom (fun t -> 
            let p = projTrafo.GetValue(t)
            let location = view.GetValue(t)
            time.GetValue(t) |> ignore
            let s = win.Sizes.GetValue(t) |> V3d
            let computeSize (radius : float) (pos : V3d) =
                let d = V3d(radius, 0.0, Vec.length (location.Location - pos))
                let ndc = p.Forward.TransformPosProj(d)
                abs ndc.X * s
            bodies |> Array.map (fun b -> computeSize (float b.radius) b.pos.Value)
        )

    let scale = aspect |> AVal.map (fun aspect -> Trafo3d.Scale(V3d(1.0, aspect, 1.0)))

    let texts = 
        let contents = 
            bodies |> Array.map (fun  b -> 
                let p = 
                    (b.pos, viewProj, scale) |||> AVal.map3 (fun p vp scale -> 
                        let ndc = vp.Forward.TransformPosProj b.pos.Value
                        let scale = if inNdcBox ndc then scale else Trafo3d.Scale(0.0)
                        Trafo3d.Scale(0.05) * scale * Trafo3d.Translation(ndc.XYO)
                    )
                p, AVal.constant b.name
            )
        Sg.texts font C4b.White (ASet.ofArray contents)




    let planets = 
        Sg.draw IndexedGeometryMode.PointList
        |> Sg.vertexAttribute' DefaultSemantic.Colors colors
        |> Sg.vertexAttribute  DefaultSemantic.Positions vertices
        |> Sg.vertexAttribute "Size" sizes
        |> Sg.shader {
            do! Shaders.splatPoint
            do! Shaders.round
        }

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
        |> Sg.trafo (scale |> AVal.map (fun s -> Trafo3d.Scale(0.05) * s * Trafo3d.Translation(-0.95, 0.95,0.0)))

    let lineSg = 
        let lines =
            bodies |> Array.map (fun b -> 
                let transformedVertices = 
                    (b.history.Positions, viewProj) ||> AVal.map2 (fun vertices vp -> 
                        vertices |> Array.map (fun v -> vp.Forward.TransformPosProjFull v |> V4f)
                    )
                Sg.draw IndexedGeometryMode.LineStrip
                |> Sg.vertexAttribute DefaultSemantic.Positions transformedVertices
            )
        Sg.ofArray lines
        |> Sg.shader { 
            do! DefaultSurfaces.constantColor C4f.White
        }

    let spacecraftTrafo = cval (Trafo3d.Scale 0.0)
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
        coordinateCross 1000000000.0 spacecraftTrafo

    let sg =
        Sg.ofList [planets; texts; info; lineSg; heraCoordinateCross; help ] 

    let clearTrail() =
        for b in bodies do b.history.Clear()

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

    win.Keyboard.KeyDown(Keys.T).Values.Add(fun _ -> 
        transact (fun _ -> 
            time.Value <- DateTime.Parse args.startTime
            clearTrail()
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
            let currentObserver = observerBody.GetValue()
            match bodySources |> Array.tryFindIndex (fun o -> o.name = currentObserver.name) with
            | Some currentIndex -> 
                observerBody.Value <- bodySources[(currentIndex + 1) % bodySources.Length]
            | _ -> ()
            clearTrail()
        )
    )

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
                        spacecraftTrafo.Value <- t
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