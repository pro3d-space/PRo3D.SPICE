module SolarSystemVisualization


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

open PRo3D.SPICE

do Aardvark.Base.Aardvark.UnpackNativeDependencies(typeof<CooTransformation.RelState>.Assembly)

type Font = GoogleFontProvider<"Roboto Mono">

module Shaders =

    open FShade
    open Aardvark.Rendering.Effects

    let private shadowMapVisualization =
        sampler2d {
            texture uniform?ShadowMap
            filter Filter.MinMagLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let showShadowMapIfValid (v : Vertex) = 
        fragment { 
            let valid : bool = uniform?ShadowMapValid
            if valid then
                let v = abs (shadowMapVisualization.SampleLevel(v.tc, 0.0))
                if v.X = 1.0 then 
                    return V4d(1.0,0.0,0.0,1.0)
                else
                    return V4d(v.X, v.Y, v.Z, 1.0)
            else
                return V4d(1.0,0.0,0.0,1.0)
        }



type Arguments = 
    {
        // start time given as UTC string (parsable by System.DateTime)
        // e.g. "2025-03-10 19:08:12.60"
        [<Option('t', "time", Required = true, HelpText = "Simulation start time.")>]
        startTime : string

        // start time given as a time span string
        // e.g. "10.00:00:00" means 10 days
        [<Option('w', "trajectorylength", Required = false, HelpText = "length of trajectory")>]
        trajectoryLength : string

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
            trajectoryLength = "10.00:00:00"
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
    let currentObserver = CelestialBodies.bodySources |> Array.findIndex (fun b -> b.name = args.observerBody) |> cval
    let referenceFrame = args.referenceFrame
    let supportBody = "SUN"
    let bodies = CelestialBodies.bodySources |> Array.map (fun b -> b.name, b) |> AMap.ofArray

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
                            DefaultCameraController.controlZoomWithSpeed speed win.Mouse
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
    let viewTrafo = view |> AVal.map CameraView.viewTrafo

    let supportBody = 
        observer |> AVal.bind (fun observer -> 
            adaptive { 
                match! AMap.tryFind observer bodies with
                | Some observer -> return observer.goodObserver
                | _ -> return "Sun"
            }
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

    let shadowMap, colorDepthMap, shadowMapValid, shadowMapCamera  = 
        let bodyVisualization = Rendering.bodiesVisualization args.referenceFrame supportBody (bodies |> AMap.toASetValues) observer time id 
        Rendering.renderShadowMap win.Runtime args.referenceFrame supportBody bodyVisualization observer time

    let wrapModel (sg : ISg) =
        Rendering.StableTrafoSceneGraphExtension.Sg.wrapStableShadowViewProjTrafo (shadowMapCamera |> AVal.map Camera.viewProjTrafo) sg

    let bodyVisualization = 
        Rendering.bodiesVisualization args.referenceFrame supportBody (bodies |> AMap.toASetValues) observer time wrapModel 



    let shadowMapBias = cval -0.017
    let shadowMapBias = cval -0.000015
    let scene =
        bodyVisualization
        |> Sg.uniform "HasShadowMap" shadowMapValid
        |> Sg.texture "ShadowMap" shadowMap
        |> Sg.uniform' "ShadowMapBias" shadowMapBias
        |> Rendering.StableTrafoSceneGraphExtension.Sg.wrapStableShadowViewProjTrafo (shadowMapCamera |> AVal.map Camera.viewProjTrafo)
        |> Sg.shader {
            do! Rendering.Shaders.transformShadowVertices
            //do! Rendering.Shaders.normalMap
            do! Rendering.Shaders.genAndFlipTextureCoord // for some reason v needs to be 1- flipped. 
            do! DefaultSurfaces.sgColor
            do! Rendering.Shaders.stableTrafo
            do! DefaultSurfaces.diffuseTexture
            do! Rendering.Shaders.shadow
            do! Rendering.Shaders.solarLightingWithSpecular
        }

    let trajectories = 
        let getTrajectoryProperties (bodyName : string) =
            (CelestialBodies.getOrbitLength bodyName, 45) |> AVal.constant

        Rendering.trajectoryVisualization referenceFrame observer time getTrajectoryProperties (bodies |> AMap.toASetValues)
        
    let scene = 
        Sg.ofList [
            scene;
            trajectories
        ]


    let realScene = 
        scene
        |> Sg.viewTrafo viewTrafo
        |> Sg.projTrafo projTrafo


    let font = Font.Font
    let aspectScaling = aspect |> AVal.map (fun aspect -> Trafo3d.Scale(V3d(1.0, aspect, 1.0)))

    let info = 
        let content = time |> AVal.map (fun t -> sprintf "%s" (CooTransformation.Time.toUtcFormat t))
        Sg.text font C4b.Gray content 
        |> Sg.trafo (aspectScaling |> AVal.map (fun s -> Trafo3d.Scale(0.1) * s * Trafo3d.Translation(-0.95,-0.95,0.0)))
     
    let help = 
        let content =
            observer |> AVal.map (fun o -> 
                String.concat Environment.NewLine [
                    "<c>    : switch camera mode"
                    "<t>    : reset time"
                    $"<n>    : switch observer ({o})."
                ]
            )
        Sg.text font C4b.Gray content
        |> Sg.trafo (aspectScaling |> AVal.map (fun s -> Trafo3d.Scale(0.02) * s * Trafo3d.Translation(-0.95, 0.90,0.0)))


    let bodyLabels = 
        bodies.Content
        |> AVal.map (fun bodies -> 
            let contents = 
                bodies |> HashMap.toValueArray |> Array.map (fun bodyDesc -> 
                    let bodyPos = Rendering.getPosition referenceFrame supportBody bodyDesc.name observer time
                    let p = 
                        AVal.custom (fun t -> 
                            let p = bodyPos.GetValue t
                            let vp = viewProj.GetValue t
                            let scale = aspectScaling.GetValue t
                            let observer = observer.GetValue()
                            match p with
                            | None -> Trafo3d.Scale(0.0)
                            | Some p ->
                                let ndc = vp.Forward.TransformPosProj (p) 
                                let scale = if inNdcBox ndc && bodyDesc.name <> observer then scale else Trafo3d.Scale(0.0)
                                Trafo3d.Scale(0.05) * scale * Trafo3d.Translation(ndc.XYZ)
                        )
                    p, AVal.constant bodyDesc.name
                )
            Sg.texts font C4b.White (ASet.ofArray contents)
         )
         |> Sg.dynamic

    let shadowMapVisualization = 
        Sg.fullScreenQuad 
        |> Sg.andAlso (
            // border round the texture visualization to distinguish between black and no visualization
            Sg.fullScreenQuad 
            |> Sg.fillMode' FillMode.Line 
            |> Sg.shader { 
                do! DefaultSurfaces.trafo; 
                do! DefaultSurfaces.constantColor C4f.White 
            }
        )
        |> Sg.trafo' (Trafo3d.Scale(0.2) * Trafo3d.Translation(0.7,0.0,0.0))
        |> Sg.texture "ShadowMap" colorDepthMap
        |> Sg.uniform' "ShadowMapValid" shadowMapValid
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.constantColor C4f.White
            do! Shaders.showShadowMapIfValid
        }
        |> Sg.viewTrafo' Trafo3d.Identity
        |> Sg.projTrafo' Trafo3d.Identity


    let sg = 
        Sg.ofList [
            realScene
            info
            help
            bodyLabels
            shadowMapVisualization
            trajectories
        ]

    let animationStep() =
        ()

    let s = 
        let sw = Diagnostics.Stopwatch.StartNew()
        animationStep()
        let mutable lastFrame = None
        win.AfterRender.Add(fun _ -> 
            transact (fun _ -> 

                let dt = 
                    match lastFrame with
                    | None -> TimeSpan.Zero
                    | Some l -> sw.Elapsed - l

                time.Value <- time.Value + dt * args.timeToSimluationTime
                animationStep()
                lastFrame <- Some sw.Elapsed
            )
        )


    win.Keyboard.KeyDown(Keys.O).Values.Add(fun _ -> 
        transact (fun _ -> 
            currentObserver.Value <- (currentObserver.Value + 1) % bodySources.Length
            observerBody.Value <- bodySources.[currentObserver.Value]
            initialView.Value <- getLookAt observerBody.Value 
        )
    )

    win.Keyboard.KeyDown(Keys.P).Values.Add(fun _ -> 
        let p = view.GetValue()
        Log.line "%A" p.Location
        let r = shadowMap.GetValue()
        let m = r.DownloadDepth()
        m.ForeachCoord(fun (v : V2l) -> 
            let p = m[v.X,v.Y]
            if p <> 1.0f then
                printfn "%A" p
        )
    )

    win.Keyboard.DownWithRepeats.Values.Add(fun k -> 
        if k = Keys.PageDown then
            transact (fun _ -> 
                shadowMapBias.Value <- shadowMapBias.Value - 0.000001
                printfn "%A" shadowMapBias.Value
            )
        elif k = Keys.PageUp then
            transact (fun _ -> 
                shadowMapBias.Value <- shadowMapBias.Value + 0.000001
                printfn "%A" shadowMapBias.Value
            )
    )

    let task =
        app.Runtime.CompileRender(win.FramebufferSignature, sg)

    win.RenderTask <- task
    win.Run()
    0