module SolarSystemVisualization

open System.Globalization


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
open PRo3D.Base

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


//Some useful times with these kernels:

//    Deimos Closest Approach (distance 299.933 km) time is 2025-03-12 12:10:23.528 UTC
//    Deimos in AFC field-of-view:
//        Start: 2025-03-12 11:52:20.482190 UTC, End: 2025-03-12 12:05:49.188895 UTC
//        Start: 2025-03-12 12:07:47.939156 UTC, End: 2025-03-12 12:08:53.801684 UTC
//    Mars Closest Approach (altitude 5681.148 km) time is 2025-03-12 12:50:49.268 UTC

 


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
                 
    let time = cval (DateTime.Parse(args.startTime, CultureInfo.InvariantCulture))

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


    let currentObservation : cval<Option<Set<string> * (FitsDescription * string)>> = cval None

    let instruments =
        let frustum = Frustum.perspective 5.5306897076421 10.0 distanceSunPluto 1.0
        Map.ofList [
            "HERA_AFC-1", frustum
            "HERA_AFC-2", frustum
        ]
                

    let cameraEarthFlyBy (instrumentName : string) (date : DateTime) = 
        AVal.custom (fun t -> 
            let observer = observer.GetValue t
            let supportBody = "SUN"
            let afc1Pos = CooTransformation.getRelState instrumentName supportBody observer date referenceFrame
            let moon = CooTransformation.getRelState "MOON" supportBody observer date referenceFrame
            let earthState = CooTransformation.getRelState "EARTH" supportBody observer date referenceFrame
            match afc1Pos, moon, earthState with    
            | Some afcPos1, Some moonState, Some earthState -> 
                let focusMiddleEarthMoon = (moonState.pos + earthState.pos) * 0.5
                let afc_z = afcPos1.pos - focusMiddleEarthMoon |> Vec.normalize
                let afc_y = Vec.cross afc_z afcPos1.pos.Normalized |> Vec.normalize
                let afc_x = Vec.cross afc_y afc_z
                let cam = CameraView.lookAt afcPos1.pos focusMiddleEarthMoon -afc_y
                Some cam
            | _ -> 
                None
        )
            
    let earthFlyBy (instrument : string) (fits : FitsDescription) =
        cameraEarthFlyBy instrument fits.observationDate |> AVal.map (function
            | None -> None
            | Some extrinsic ->
                match Map.tryFind instrument instruments with
                | None -> None
                | Some frustum -> 
                    let cam = Camera.create extrinsic frustum
                    Some cam
        )

    let observations = 
        Frusta.parseObservations @"C:\pro3ddata\HERA\fake"
        |> Array.map (fun o -> 
            Set.ofList ["earth"; "moon"], o
        )

    let getObservationCamera (planet : string) =
        currentObservation |> AVal.bind (function 
            | None -> 
                AVal.constant None
            | Some (validForPlanets, (fits, _)) ->
                if Set.contains planet validForPlanets then
                    //earthFlyBy fits.instrument fits 
                    earthFlyBy "HERA_AFC-1" fits
                else
                    AVal.constant None
        )


    let startTime = DateTime.Parse("2025-03-11 11:52:20.482190", CultureInfo.InvariantCulture)
    let endTime = DateTime.Parse("2025-03-12 13:20:20.482190", CultureInfo.InvariantCulture)
    let shots = (endTime - startTime) / TimeSpan.FromMinutes(1) |> ceil |> int
    let interval = (endTime - startTime) / float shots
    let snapshots = [ 0 .. shots ] |> List.map (fun i -> startTime + interval * float i) |> List.toArray
        

    let computeMarsFlyByCam (referenceFrame : string) (time : DateTime) =
        let supportBody = "SUN"
        let instrumentName = "HERA"
        let observer = observerBody.Value.name
        if true then
            let afc1Pos = CooTransformation.getRelState instrumentName supportBody observer time referenceFrame
            match afc1Pos with    
            | Some targetState -> 
                let rot = targetState.rot
                let t = Trafo3d.FromBasis(rot.C0, rot.C1, rot.C2, targetState.pos)
                //CameraView.lookAt targetState.pos V3d.Zero V3d.OOI |> Some
                CameraView.ofTrafo t.Inverse |> Some 
            | _ -> 
                None
        else
            let afc1Pos = CooTransformation.getRelState instrumentName supportBody observer time referenceFrame
            let phobos = CooTransformation.getRelState "PHOBOS" supportBody observer time referenceFrame
            match afc1Pos, phobos with    
            | Some afcPos1, Some phobosPos ->
                let afc_z = afcPos1.pos - phobosPos.pos |> Vec.normalize
                let afc_y = Vec.cross afc_z afcPos1.pos.Normalized |> Vec.normalize
                let afc_x = Vec.cross afc_y afc_z
                let t = Trafo3d.FromBasis(afc_x, afc_y, afc_z, afcPos1.pos)
                let camView  = CameraView.lookAt afcPos1.pos phobosPos.pos afc_y
                Some camView
            | _ -> None

    let projectedTexture (planet : string) : aval<ITexture> = 
        currentObservation |> AVal.bind (function 
            | None -> 
                NullTexture.InstanceConst
            | Some (_, (_, img)) ->
                AVal.constant (FileTexture(img))
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
        let bodyVisualization = Rendering.bodiesVisualization args.referenceFrame supportBody (bodies |> AMap.toASetValues) observer time (fun _ sg -> sg) 
        Rendering.renderShadowMap win.Runtime args.referenceFrame supportBody bodyVisualization observer time


    let customObservationCamera = cval None

    let getProjectionTrafo (planet : string) : aval<Option<Trafo3d>> = 
        getObservationCamera planet |> AVal.map (function None -> None | Some c -> Camera.viewProjTrafo c |> Some)

    let shadowMapBias = cval -0.017
    let shadowMapBias = cval -0.000015

    let wrapModel (planet : string) (sg : ISg) =
        let projectedTexture = projectedTexture planet
        let projectedImageTrafo = getProjectionTrafo planet
        sg 
        |> Sg.applyProjectedImage getObservationCamera 
        |> Rendering.StableTrafoSceneGraphExtension.Sg.wrapStableShadowViewProjTrafo (shadowMapCamera |> AVal.map Camera.viewProjTrafo) 
        |> Sg.texture "ProjectedTexture" projectedTexture
        |> Sg.uniform "ProjectedImageModelViewProjValid" (projectedImageTrafo |> AVal.map Option.isSome)


    let simulatedObservation =
        let bodyVisualization = Rendering.bodiesVisualization args.referenceFrame supportBody (bodies |> AMap.toASetValues) observer time wrapModel
        let cam = 
            customObservationCamera 
            //getObservationCamera "earth"
        let signature = 
           win.Runtime.CreateFramebufferSignature [
               DefaultSemantic.Colors, TextureFormat.Rgba8
               DefaultSemantic.DepthStencil, TextureFormat.DepthComponent32f
           ]
        bodyVisualization
        |> Sg.viewTrafo (cam |> AVal.map (function Some c -> Camera.viewTrafo c | None -> Trafo3d.Identity))
        |> Sg.projTrafo (cam |> AVal.map (function Some c -> Camera.projTrafo c | None -> Trafo3d.Identity))
        |> Sg.shader { 
            do! Rendering.Shaders.transformShadowVertices
            //do! Rendering.Shaders.normalMap
            do! Rendering.Shaders.genAndFlipTextureCoord // for some reason v needs to be 1- flipped. 
            do! DefaultSurfaces.sgColor
            do! Rendering.Shaders.stableTrafo
            do! DefaultSurfaces.diffuseTexture
            do! Rendering.Shaders.shadow
            //do! Rendering.Shaders.shadowPCF
            do! Rendering.Shaders.solarLightingWithSpecular
        }
        |> Sg.uniform "HasShadowMap" shadowMapValid
        |> Sg.texture "ShadowMap" shadowMap
        |> Sg.uniform' "ShadowMapBias" shadowMapBias
        |> Sg.compile win.Runtime signature
        |> RenderTask.renderToColor (AVal.constant (V2i.II * 1024))



    let allObservations = 
        let startTime = DateTime.Parse("2025-03-12 10:50:20.482190Z", CultureInfo.InvariantCulture)
        let endTime = DateTime.Parse("2025-03-12 15:50:20.482190Z", CultureInfo.InvariantCulture)
        let shots = (endTime - startTime) / TimeSpan.FromMinutes(1) |> ceil |> int
        let interval = (endTime - startTime) / float shots
        let snapshots = [ 0 .. shots ] |> List.map (fun i -> startTime + interval * float i) |> List.toArray
        time |> AVal.map (fun _ -> 
            snapshots 
            |> Array.choose (fun time -> 
                match computeMarsFlyByCam referenceFrame time, CooTransformation.getRelState "HERA" "SUN" "MARS" time referenceFrame, CooTransformation.getRotationTrafo "IAU_MARS" referenceFrame time  with
                | Some camInMarsSpace, Some rel, Some t -> 
                    let frustum = instruments["HERA_AFC-1"]
                    let v1 = CameraView.viewTrafo camInMarsSpace
                    let view = CameraView.lookAt rel.pos V3d.Zero V3d.OOI 
                    let forward = (t * CameraView.viewTrafo camInMarsSpace * Frustum.projTrafo frustum).Forward
                    forward |> M44f.op_Explicit |> Some

                | _ -> 
                    None
            )
        )


    let wrapModel (planet : string) (sg : ISg) =
        let projectedTexture = projectedTexture planet
        let projectedImageTrafo = fun _ -> customObservationCamera :> aval<_> // getProjectionTrafo planet
        sg 
        |> Sg.applyProjectedImage projectedImageTrafo 
        |> Sg.uniform "ProjectedImagesLocalTrafos" allObservations
        |> Sg.uniform "ProjectedImagesLocalTrafosCount" (allObservations |> AVal.map Array.length)
        |> Rendering.StableTrafoSceneGraphExtension.Sg.wrapStableShadowViewProjTrafo (shadowMapCamera |> AVal.map Camera.viewProjTrafo) 
        |> Sg.texture "ProjectedTexture" simulatedObservation
        |> Sg.uniform "ProjectedImageModelViewProjValid" (customObservationCamera |> AVal.map Option.isSome)

    let bodyVisualization = 
        Rendering.bodiesVisualization args.referenceFrame supportBody (bodies |> AMap.toASetValues) observer time wrapModel 

        

    let scene =
        bodyVisualization
        |> Sg.uniform "HasShadowMap" shadowMapValid
        |> Sg.texture "ShadowMap" shadowMap
        |> Sg.uniform' "ShadowMapBias" shadowMapBias
        |> Sg.shader {
            do! ImageProjection.Shaders.stableImageProjectionTrafo
            do! Rendering.Shaders.transformShadowVertices
            //do! Rendering.Shaders.normalMap
            do! Rendering.Shaders.genAndFlipTextureCoord // for some reason v needs to be 1- flipped. 
            do! DefaultSurfaces.sgColor
            do! Rendering.Shaders.stableTrafo
            do! DefaultSurfaces.diffuseTexture
            do! Rendering.Shaders.shadow
            //do! Rendering.Shaders.shadowPCF
            do! Rendering.Shaders.solarLightingWithSpecular
            do! ImageProjection.Shaders.stableImageProjection
            do! ImageProjection.Shaders.localImageProjections
        }

    let trajectories = 
        let getTrajectoryProperties (bodyName : string) =
            observer |> AVal.map (fun observer -> 
                let body = CelestialBodies.getOrbitLength bodyName
                let observer = CelestialBodies.getOrbitLength observer
                TimeSpan.FromDays(1), 200
            )

        let color body = 
            bodies |> AMap.tryFind body |> AVal.map (function
                | None ->  C4b.White
                | Some c -> C4b c.color
            )

        Rendering.trajectoryVisualization referenceFrame observer time getTrajectoryProperties (constF (AVal.constant true)) color (bodies |> AMap.toASetValues)
        
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

    let observationSimluationVisualization = 
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
        |> Sg.trafo' (Trafo3d.Scale(0.2) * Trafo3d.Translation(0.7,-0.5,0.0))
        |> Sg.texture DefaultSemantic.DiffuseColorTexture simulatedObservation
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.diffuseTexture
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
            observationSimluationVisualization
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
                let frustum = instruments.["HERA_AFC-1"]
                let view = (computeMarsFlyByCam referenceFrame time.Value).Value
                customObservationCamera.Value <- Some (Camera.create view frustum)
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

    let mutable observationIdx = -1
    win.Keyboard.KeyDown(Keys.N).Values.Add(fun _ -> 
        transact (fun _ -> 
            observationIdx <- (observationIdx + 1) % observations.Length
            let (_,(f,_)) as observation = observations[observationIdx] 
            currentObservation.Value <- observation |> Some
            time.Value <- f.observationDate
            printfn "%A" currentObservation.Value
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


    win.Keyboard.KeyDown(Keys.S).Values.Add(fun _ -> 
        snapshots |> Array.iteri (fun i t -> 
            
            let view = (computeMarsFlyByCam referenceFrame time.Value).Value
            let frustum = instruments.["HERA_AFC-1"]
            transact (fun _ -> 
                time.Value <- t
                customObservationCamera.Value <- Some (Camera.create view frustum)
            )
        
            let r = simulatedObservation.GetValue()
            let pi = r.Download()
            pi.Save(Path.Combine(@"C:\pro3ddata\HERA\simulated", $"SIMULATED_{i}.tif"))
        )
    )

    let mutable i = 0
    win.Keyboard.KeyDown(Keys.V).Values.Add(fun _ -> 
        transact (fun _ -> 
            i <- (i + 1) % snapshots.Length
            time.Value <- snapshots[i]
        )
        let frustum = instruments.["HERA_AFC-1"]
        let view = (computeMarsFlyByCam referenceFrame time.Value).Value
        transact (fun _ -> 
            initialView.Value <- view
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