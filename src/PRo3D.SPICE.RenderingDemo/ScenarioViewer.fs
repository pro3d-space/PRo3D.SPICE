module ScenarioViewer

open PRo3D.Extensions.FSharp.CooTransformation


#nowarn "9"

open System
open System.IO
open Aardvark.Reconstruction
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


type CameraMode =
    | FreeFly
    | Orbit


let launchMoonFlyby = 
    { 
        startTime = "2024-10-11T14:17:02.233"; 
        timeToSimluationTime = (31.0 * 86400.0) / 150.0
        cameraSpeed = 1.0<m/s>
        observerBody = "EARTH"; 
        referenceFrame = "ECLIPJ2000" 
        spiceKernel = "../../../spice_kernels/kernels/mk/hera_ops.tm"
    }


let main (argv : array<string>) = 
    
    //let args = 
    //    if argv.Length = 0 then
    //        scenarios[0]
    //    else
    //      let result = CommandLine.Parser.Default.ParseArguments<Arguments>(argv)
    //      match result with
    //      | :? Parsed<Arguments> as parsed -> parsed.Value
    //      | :? NotParsed<Arguments> as notParsed -> failwithf "could not parse arguments: %A" notParsed.Errors
    //      | _ -> failwithf "%A" result

    let args = launchMoonFlyby


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
    let r = M44d.Translation(V3d.III*2.0)
    let arr = r.ToArray() 
    let r2 = M44d(arr)
    1

    let getLookAt (body : string) (observer : string) = 
        match CooTransformation.getRelState body "SUN" observer time.Value referenceFrame with
        | None -> failwith "could not get initial camera view"
        | Some targetState -> 
            //CameraView.lookAt targetState.pos V3d.Zero V3d.OOI 
            let rot = targetState.rot
            let t = Trafo3d.FromBasis(rot.C0, rot.C1, rot.C2, targetState.pos)
            CameraView.ofTrafo t.Inverse

    let initialView = 
        getLookAt "HERA" "EARTH" |> cval

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
    let farPlane = 384400.0 * 1000.0
    let frustum = win.Sizes |> AVal.map (fun s -> Frustum.perspective 5.5 0.1 farPlane (float s.X / float s.Y))
    let aspect = win.Sizes |> AVal.map (fun s -> float s.X / float s.Y)
    let scale = aspect |> AVal.map (fun aspect -> Trafo3d.Scale(V3d(1.0, aspect, 1.0)))


    let projTrafo = frustum |> AVal.map Frustum.projTrafo
    let viewProj = 
        (view, projTrafo) ||> AVal.map2 (fun view projTrafo -> 
            CameraView.viewTrafo view * projTrafo
        )

    
    let afcProj (fits : FitsDescription) = 
        let focalLength = 106.0<mm>
        let principalPoint = V2d.Zero
        
        { Aardvark.Reconstruction.Projection.aspect = float fits.imageSize.X / float fits.imageSize.Y; 
                                             focalLength = focalLength |> mmToMeters |> float; 
                                             principalPoint = principalPoint; 
                                             imageSize = fits.imageSize; distortion = Distortion2d.Identity }


    let observationDir, instrumentName = 
        @"C:\pro3ddata\HERA\singleimage", "HERA_AFC-1"

    let instrumentImages =
        observationDir 
        |> Frusta.parseObservations 



    let cam = 
        CooTransformation.getRelState "HERA_AFC1" "SUN" "HERA" time.Value args.referenceFrame    


    let getDirection (body : string) (supportBody : string) (observerBody : string) (dT : TimeSpan) (date : DateTime) (referenceFrame : string) = 
        let t0 = CooTransformation.getRelState body supportBody observerBody (date - dT) referenceFrame
        let t1 = CooTransformation.getRelState body supportBody observerBody date referenceFrame
        match t0, t1 with
        | Some rel0, Some rel1 -> Some (rel1.pos - rel0.pos)
        | _ -> None

    let instrumentTrajectory = 
        instrumentImages 
        |> Array.choose (fun (fits, imageFileName) -> 
            let supportBody = "MERCURY"
            let afc1Pos = CooTransformation.getRelState instrumentName supportBody observerBody.Value.name fits.observationDate referenceFrame
            let moon = CooTransformation.getRelState "MOON" supportBody observerBody.Value.name fits.observationDate referenceFrame
            let earthState = CooTransformation.getRelState "EARTH" supportBody observerBody.Value.name fits.observationDate referenceFrame
            match afc1Pos, moon, earthState with    
            | Some afcPos1, Some moonState, Some earthState -> 
                let focusMiddleEarthMoon = (moonState.pos + earthState.pos) * 0.5
                let afc_z = afcPos1.pos - focusMiddleEarthMoon |> Vec.normalize
                let afc_y = Vec.cross afc_z afcPos1.pos.Normalized |> Vec.normalize
                let afc_x = Vec.cross afc_y afc_z
                let t = Trafo3d.FromBasis(afc_x, afc_y, afc_z, afcPos1.pos)

                let m : double[] = Array.zeroCreate 9
                let pdMat = fixed &m[0]
                let r = CooTransformation.GetPositionTransformationMatrix("HERA_AFC-1", referenceFrame, Time.toUtcFormat fits.observationDate, pdMat)
                if r <> 0 then failwith ""
                let pdMat = M33d(m)

                let camView  = Aardvark.Reconstruction.CameraView.lookAt afcPos1.pos (afcPos1.pos + afc_z) afc_y
                //let camView = { Aardvark.Reconstruction.CameraView.trafo = Euclidean3d.FromTrafo3d t }
                let afcProj = afcProj fits
                let fullCam = { Aardvark.Reconstruction.Camera.view = camView; proj = afcProj }

                let cameraView = CameraView.lookAt afcPos1.pos (afcPos1.pos + afc_z) afc_x
                let frustum = Frustum.perspective 5.5 0.1 1000000000.0 (float fits.imageSize.X / float fits.imageSize.Y)

                Some {| fits = fits; trafo = camView.ViewTrafo; imageFileName = imageFileName; cam = fullCam; 
                        cameraView = cameraView; frustum = frustum |}
            | _ -> None

            //let afcState = CooTransformation.getRelState instrumentName "SUN" observerBody.Value.name fits.observationDate referenceFrame
            //let direction = getDirection instrumentName "SUN" observerBody.Value.name (TimeSpan.FromSeconds(1)) fits.observationDate referenceFrame
            //match afcState, direction with
            //| Some afcState, Some dir -> 
            //    let m : double[] = Array.zeroCreate 9
            //    let pdMat = fixed &m[0]
            //    let r = CooTransformation.GetPositionTransformationMatrix("HERA_AFC-1", "J2000", Time.toUtcFormat fits.observationDate, pdMat)
            //    if r <> 0 then failwith ""

            //    let rotMat = M33d(m)
            //    let camView  = Aardvark.Reconstruction.CameraView.lookAt targetState.pos (targetState.pos + -rotMat.C2) -rotMat.C1
            //    let afcProj = afcProj fits
            //    let fullCam = { Aardvark.Reconstruction.Camera.view = camView; proj = afcProj }

            //    Some {| fits = fits; trafo = camView.ViewTrafo; imageFileName = imageFileName; cam = fullCam |}
            //| _ -> 
            //    None
        ) 

    let frusta = 
        instrumentTrajectory 
        |> Array.map (fun observation -> 
            let scale = 1.0
            let enlargedCam = { observation.cam with proj = { observation.cam.proj with focalLength = observation.cam.proj.focalLength * scale } }
            //Sg.frustum (AVal.constant C4b.Red) (AVal.constant observation.cameraView) (AVal.constant observation.frustum)
            //|> Sg.shader {
            //    do! DefaultSurfaces.stableTrafo
            //}
            //|> Sg.andAlso (Sg.cameraWithPhoto (AVal.constant scale) (AVal.constant 0.7) observation.imageFileName observation.cam)
            Sg.cameraWithPhoto (AVal.constant scale) (AVal.constant 0.7) observation.imageFileName observation.cam
        )
        |> Sg.ofArray

    let trajectory = 
        instrumentTrajectory 
        |> Array.pairwise
        |> Array.map (fun (o1,o2) -> 
            let p0 = o1.trafo.Backward.C3.XYZ
            let p1 = o2.trafo.Backward.C3.XYZ
            [| Line3d(V3d.Zero, p1 - p0) |]
            |> Sg.lines' C4b.White
            |> Sg.translation' p0
        )
        |> Sg.ofArray
        |> Sg.shader {
            do! DefaultSurfaces.stableTrafo
            do! DefaultSurfaces.constantColor C4f.White
        }



    let bodySg =
        IndexedGeometryPrimitives.solidPhiThetaSphere Sphere3d.Unit 32 C4b.White
        |> Sg.ofIndexedGeometry


    let getBodyTrafo (body : BodyDesc) =
        adaptive {
            let! observer = observer
            let! time = time
            if observer = body.name then
                return Trafo3d.Identity
            else
                match CooTransformation.getRelState body.name "SUN" observer time args.referenceFrame with
                | None -> 
                    //Log.warn $"could not get body trafo for body: {body.name}"
                    return Trafo3d.Scale(0.0)
                | Some targetState -> 
                    let rot = targetState.rot
                    let t = Trafo3d.FromBasis(rot.C0, rot.C1, rot.C2, targetState.pos)
                    return t
        }

    let bodies = 
        CelestialBodies.bodySources
        |> Array.map (fun bodySource -> 
            let diameter = 0.5 * (bodySource.diameter |> kmToMeters)
            bodySg
            |> Sg.uniform' "Color" bodySource.color
            |> Sg.scale (float diameter)
            |> Sg.trafo (getBodyTrafo bodySource)
        ) 
        |> Sg.ofArray
        |> Sg.shader {
            do! DefaultSurfaces.stableTrafo
            do! DefaultSurfaces.sgColor
        }
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

    let beforeMain = RenderPass.before "JD" RenderPassOrder.Arbitrary RenderPass.main
    let sg =
        Sg.ofList [bodies |> Sg.pass beforeMain; frusta; trajectory; info;  heraCoordinateCross; help ] 
        |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
        |> Sg.projTrafo projTrafo



    //win.Keyboard.KeyDown(Keys.R).Values.Add(fun _ -> 
    //    transact (fun _ -> 
    //        match CooTransformation.getRelState "MARS" "SUN" (AVal.force observer) time.Value referenceFrame with
    //        | Some targetState -> 
    //            let rot = targetState.rot
    //            let t = Trafo3d.FromBasis(rot.C0, rot.C1, -rot.C2, V3d.Zero)
    //            initialView.Value <- CameraView.ofTrafo t.Inverse
    //        | _ -> ()
    //    )
    //)

    win.Keyboard.KeyDown(Keys.C).Values.Add(fun _ -> 
        transact (fun _ -> 
            cameraMode.Value <-
                match cameraMode.Value with
                | CameraMode.FreeFly -> CameraMode.Orbit
                | CameraMode.Orbit -> CameraMode.FreeFly
        )
    )

    let mutable currentCam = -1
    win.Keyboard.KeyDown(Keys.N).Values.Add(fun _ -> 
        transact (fun _ -> 
            currentCam <- (currentCam + 1) % instrumentTrajectory.Length
            let cam = instrumentTrajectory.[currentCam]
            printfn "switching to %s, %d, %A" cam.imageFileName currentCam cam

            let pos = cam.trafo.Backward.C3.XYZ
            let forward = cam.trafo.Backward.C2.XYZ
            let sky = cam.trafo.Backward.C1.XYZ

            let cameraView = CameraView.lookAt (pos - forward * 10.0) pos sky
            initialView.Value <- cameraView
            time.Value <- cam.fits.observationDate
        )
    )

    let animationStep () =
        ()

    let play = false

    let s = 
        let sw = Diagnostics.Stopwatch.StartNew()
        animationStep()
        let mutable lastFrame = None
        win.AfterRender.Add(fun _ -> 
            if play then
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