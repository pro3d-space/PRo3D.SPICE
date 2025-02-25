namespace PRo3D.SPICE

open System

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph

open Aardvark.Rendering
open Aardvark.Geometry

open PRo3D.Extensions
open PRo3D.Extensions.FSharp

open PRo3D.Base

module Rendering = 


    module Shaders = 
    
        open FShade
        open Aardvark.Rendering.Effects


        type UniformScope with  
            member x.SunDirectionWorld : V3d = uniform?SunDirectionWorld

        type Vertex = {
            [<Position>]                pos     : V4d
            [<Normal>]                  n       : V3d
            [<BiNormal>]                b       : V3d
            [<Tangent>]                 t       : V3d
            [<Color>]                   c       : V4d
            [<Semantic("LightDir")>]    ldir    : V3d
            [<TexCoord>]                tc      : V2d
        }

        let stableTrafo (v : Vertex) =
            vertex {
                let vp = uniform.ModelViewTrafo * v.pos

                return 
                    { v with
                        pos = uniform.ProjTrafo * vp
                        n = uniform.ModelViewTrafoInv.TransposedTransformDir v.n |> Vec.normalize
                        b = uniform.ModelViewTrafo.TransformDir v.b |> Vec.normalize
                        t = uniform.ModelViewTrafo.TransformDir v.t |> Vec.normalize
                        c = v.c
                        ldir = uniform.ViewTrafo.TransformDir uniform.SunDirectionWorld |> Vec.normalize
                    }
            }

        let viewProjSpaceDepthToColor (v : Vertex) =
            fragment {
                let vp = uniform.ModelViewProjTrafo * v.pos

                let d = vp.Z / vp.W
                return V4d(d, 0.0, 0.0, 1.0)
            }

        type TexturedVertex = {
            [<TexCoord>] tc : V2d
            [<Normal>] n : V3d
            [<Tangent>] t : V3d
        }

        let genAndFlipTextureCoord (v : TexturedVertex) =
            vertex {
                return { v with tc = V2d(v.tc.X, 1.0 - v.tc.Y) }
            }

        let solarLighting (v : Vertex) = 
            fragment {
                let n = v.n |> Vec.normalize
                let c = v.ldir |> Vec.normalize

                let ambient = 0.0
                let diffuse = Vec.dot c n |> max 0.0

                let l = ambient + (1.0 - ambient) * diffuse

                return V4d(v.c.XYZ * l, v.c.W)
            }


        let private specular =
            sampler2d {
                texture uniform?SpecularColorTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        type UniformScope with
            member x.HasSpecularColorTexture : bool = x?HasSpecularColorTexture

        let solarLightingWithSpecular (v : Vertex) = 
            fragment {
                let n = v.n |> Vec.normalize
                let c = v.ldir |> Vec.normalize

                let ambient = 0.2
                let diffuse = Vec.dot c n |> clamp 0.0 1.0

                let l = ambient + (1.0 - ambient) * diffuse

                let s = Vec.dot c n 

                let specColor =
                    if uniform.HasSpecularColorTexture then 
                        let v = specular.Sample(v.tc).XYZ
                        v.X * V3d.III
                    else 
                        V3d.III

                let specularTerm = clamp 0.0 1.0 (pown s 32)
                let specShininess = specColor * specularTerm

                let c = v.c.XYZ * l //+ specShininess

                return V4d(Fun.Min(c, 1.0), v.c.W)
            }

        let private normalSampler =
            sampler2d {
                texture uniform?NormalMapTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let internal normalMap (v : TexturedVertex) =
            fragment {
                let hasNormalMap : bool = uniform?HasNormalMap
                if hasNormalMap then
                    let texColor = normalSampler.Sample(v.tc).XYZ
                    let texNormal = (2.0 * texColor - V3d.III) |> Vec.normalize

                    // make sure tangent space basis is orthonormal -> perform gram-smith normalization
                    let n = v.n.Normalized
                    let t = v.t.Normalized
                    let t = (t - n * (Vec.dot t n)) |> Vec.normalize
                    let b = (Vec.cross n t) |> Vec.normalize // NOTE: v.b might be used here to maintain handedness
                        
                    // texture normal from tangent to world space
                    let n = 
                        texNormal.X * t +
                        texNormal.Y * b +
                        texNormal.Z * n

                    return { v with n = n } 
                else
                    return v
            }
            

        type ShadowVertex = {
            [<Position>]                       p   : V4d
            [<Semantic("PosShadowViewProj")>]  viewProjPos : V4d
            [<Color>]                          c  : V4d
        }

        type UniformScope with
            member x.StableModelViewProjTexture : M44d = uniform?StableModelViewProjTexture
            member x.HasShadowMap : bool = uniform?HasShadowMap

        let private shadowSampler =
            sampler2dShadow {
                texture uniform?ShadowMap
                filter Filter.MinMagLinear
                addressU WrapMode.Border
                addressV WrapMode.Border
                borderColor C4f.White
                comparison ComparisonFunction.LessOrEqual
            }

        let transformShadowVertices (v : ShadowVertex) = 
            vertex {
                return 
                    { v with
                        viewProjPos = uniform.StableModelViewProjTexture * v.p
                    }
            }


        let shadow (v : ShadowVertex) =
            fragment {
                let bias : float = uniform?ShadowMapBias
                let p = v.viewProjPos.XYZ / v.viewProjPos.W
                let tc = V3d(0.5, 0.5,0.5) + V3d(0.5, 0.5, 0.5) * p.XYZ
                let d = min 1.0 (max 0.2 (shadowSampler.Sample(tc.XY, tc.Z + bias)))
                return V4d(v.c.XYZ * d, v.c.W)
            }

        let offsets = 
            [|
                V2d(-1.0, -1.0)
                V2d(1.0, -1.0)
                V2d(-1.0, 1.0)
                V2d(1.0, 1.0)
            |]

        let shadowPCF (v : ShadowVertex) =
            fragment {
                let bias : float = uniform?ShadowMapBias
                let p = v.viewProjPos.XYZ / v.viewProjPos.W
                let tc = V3d(0.5, 0.5, 0.5) + V3d(0.5, 0.5, 0.5) * p.XYZ

                let sampleRadius = 1.0 / (float (Vec.MaxElement shadowSampler.Size)) 
                let numSamples = 4

                let mutable shadow = 0.0
                for i in 0 .. offsets.Length - 1 do
                    shadow <- shadow + shadowSampler.Sample(tc.XY + offsets[i] * sampleRadius, tc.Z + bias)

                shadow <- shadow / float numSamples

                let d = min 1.0 (max 0.2 shadow)
                return V4d(v.c.XYZ * d, v.c.W)
            }

    [<AutoOpen>]
    module StableTrafoSceneGraphExtension =
        open Aardvark.Base.Ag
        open Aardvark.SceneGraph.Semantics.TrafoExtensions
        
        type StableViewProjTrafo(child : ISg, shadowViewProjTrafo : aval<Trafo3d>) =
            inherit Sg.AbstractApplicator(child)
            member x.ShadowViewProjTrafo = shadowViewProjTrafo

        [<Rule>]
        type StableTrafoSemantics() =
            member x.StableModelViewProjTexture(app : StableViewProjTrafo, scope : Ag.Scope) =
                let trafo = 
                    (scope.ModelTrafo, app.ShadowViewProjTrafo) ||> AVal.map2 (*)
                app.Child?StableModelViewProjTexture <- trafo

        module Sg = 
            let wrapStableShadowViewProjTrafo (trafo : aval<Trafo3d>) (sg : ISg) = 
                StableViewProjTrafo(sg, trafo) :> ISg


    let getRelState (referenceFrame : string) (supportBody : aval<string>) (body : string) (observer : aval<string>) (time : aval<DateTime>) =
        (observer, time, supportBody) |||> AVal.map3 (fun observer time supportBody -> 
            CooTransformation.getRelState body supportBody observer time referenceFrame
        )

    let getPosition (referenceFrame : string) (supportBody : aval<string>) (body : string) (observer : aval<string>) (time : aval<DateTime>) = 
        let getPos (o : CooTransformation.RelState) = o.pos
        getRelState referenceFrame supportBody body observer time |> AVal.map (Option.map getPos)

    let fullTrafo  (referenceFrame : string) (supportBody : aval<string>) (body : string) (bodyFrame : Option<string>) (observer : aval<string>) (time : aval<DateTime>) =
        let rotation = 
            match bodyFrame with
            | None -> AVal.constant None
            | Some frame -> 
                time |> AVal.map (fun time -> CooTransformation.getRotationTrafo frame referenceFrame time)
        let pos = getRelState referenceFrame supportBody body observer time
        (rotation, pos) ||> AVal.map2 (fun rot relState -> 
            match rot, relState with
            | Some rot, Some relState -> 
                Some (rot * Trafo3d.Translation relState.pos)
                //Some (Trafo3d.Translation relState.pos)
            | None, Some relState -> relState.pos |> Trafo3d.Translation |> Some
            | _ -> 
                None
        )

    let bodiesVisualization (referenceFrame : string) (supportBody : aval<string>) 
                            (bodies : aset<BodyDesc>) (observer : aval<string>) 
                            (time : aval<DateTime>) (wrapModel : string -> ISg -> ISg) =

        let sphericalUnitBody (scale : float) = 
            PolyMeshPrimitives.Sphere(30, 1.0, C4b.White, DefaultSemantic.DiffuseColorCoordinates, DefaultSemantic.DiffuseColorUTangents, DefaultSemantic.DiffuseColorVTangents)
                                .Transformed(Trafo3d.Scale scale)
                                .GetIndexedGeometry()

            |> Sg.ofIndexedGeometry


        let fallbackTexture = 
            let whitePix =
                let pi = PixImage<byte>(Col.Format.RGBA, V2i.II)
                pi.GetMatrix<C4b>().SetByCoord(fun (c : V2l) -> C4b.White) |> ignore
                pi
            PixTexture2d(PixImageMipMap(whitePix)) :> ITexture


        let bodySgs = 
            bodies 
            |> ASet.map (fun bodyDesc -> 
                let bodyPos = getPosition referenceFrame supportBody bodyDesc.name observer time
                let transformation = 
                    fullTrafo referenceFrame supportBody bodyDesc.name bodyDesc.referenceFrame observer time
                    |> AVal.map (fun trafo -> 
                        match trafo with
                        | Some trafo -> trafo, true
                        | _ -> 
                            Log.warn "could not get trafo for body %s" bodyDesc.name
                            Trafo3d.Identity, false
                    )
                let sunDirection = 
                    let sunPos = getPosition referenceFrame (AVal.constant "EARTH") "Sun" observer time
                    (sunPos, bodyPos)
                    ||> AVal.map2 (fun sunPos bodyPos -> 
                        match sunPos, bodyPos with
                        | Some sunPos, Some bodyPos -> sunPos - bodyPos |> Vec.normalize
                        | _ -> V3d.Zero
                    )
                let isObserver = observer |> AVal.map (fun o -> o = bodyDesc.name)
                let createTexture (filePath : string) =
                    PixTexture2d(filePath |> PixImageMipMap.Load) :> ITexture
                let radius = bodyDesc.diameter |> kmToMeters |> float 
                sphericalUnitBody radius // inefficient, but for a reason, see below for sg.scale
                |> wrapModel bodyDesc.name
                //|> Sg.scale radius (don't use model trafo for scaling here to make transformations less complex, trust me)
                |> Sg.trafo (AVal.map fst transformation)
                |> Sg.onOff (AVal.map snd transformation)
                |> Sg.applyPlanet bodyDesc.name
                |> Sg.uniform "SunDirectionWorld" sunDirection
                |> Sg.uniform' "Color" bodyDesc.color
                |> Sg.texture' DefaultSemantic.DiffuseColorTexture (bodyDesc.diffuseMap |> Option.map createTexture |> Option.defaultValue fallbackTexture)
                |> Sg.uniform' "HasDiffuseColorTexture" (bodyDesc.diffuseMap |> Option.isSome)
                |> Sg.texture' DefaultSemantic.NormalMapTexture (bodyDesc.normalMap |> Option.map createTexture |> Option.defaultValue fallbackTexture)
                |> Sg.uniform' "HasNormalMap" (bodyDesc.diffuseMap |> Option.isSome)
                |> Sg.texture' DefaultSemantic.SpecularColorTexture (bodyDesc.specularMap |> Option.map createTexture |> Option.defaultValue fallbackTexture)
                |> Sg.uniform' "HasSpecularColorTexture" (bodyDesc.diffuseMap |> Option.isSome)
            )
            |> Sg.set


        bodySgs
        |> Sg.cullMode' CullMode.Back


    let renderShadowMap (runtime : IRuntime) (referenceFrame : string) (supportBody : aval<string>) (scene : ISg) 
                        (observer : aval<string>) (time : aval<DateTime>) = 
        
        let sunToObserverCameraView = 
            //let shadowCaster = "MOON"
            let shadowCaster = "SUN"
            getRelState referenceFrame (AVal.constant "VENUS") shadowCaster observer time |> AVal.map (function 
                | None -> None
                | Some relState -> 
                    let pos = relState.pos
                    //let pos = V3d(-3364116.4273718265, -15969149.3536416, 8157900.047172555)
                    CameraView.lookAt pos V3d.Zero V3d.OOI |> Some
            )
            

        let shadowMapValid = sunToObserverCameraView |> AVal.map (function None -> printfn "WJS"; false | Some _ -> true)
        let lightCameraView = sunToObserverCameraView |> AVal.map (Option.defaultValue (CameraView.lookAt V3d.OOO V3d.OOO V3d.OOI))
        let lightView = lightCameraView |> AVal.map CameraView.viewTrafo
        let lightFrustum = 
            lightCameraView |> AVal.map (fun v -> 
                let loc = CameraView.location v
                let fovEarth = 0.05
                let nearEarth = (loc.Length * 0.5)

                let fovMars = 0.004
                let nearMars = (loc.Length * 0.99)
                let farMars = (loc.Length * 1.01)
                Frustum.perspective fovEarth nearMars farMars 1.0
            )
        let lightProj = lightFrustum |> AVal.map  Frustum.projTrafo
        let shadowCamera = (lightCameraView, lightFrustum) ||> AVal.map2 Camera.create
        let shadowMapSize = V2i(8192, 8192) |> AVal.constant

        let signature = 
           runtime.CreateFramebufferSignature [
               DefaultSemantic.Colors, TextureFormat.Rgba8
               DefaultSemantic.DepthStencil, TextureFormat.DepthComponent32f
           ]

        let clearValues =
            clear { 
                depth 1.0
                color C4f.Black 
            }

        let color, shadowMap = 
            scene
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.sgColor
            }
            |> Sg.onOff shadowMapValid
            |> Sg.viewTrafo lightView
            |> Sg.projTrafo lightProj
            |> Sg.compile runtime signature
            |> RenderTask.renderToColorAndDepthWithClear shadowMapSize clearValues

        shadowMap, color, shadowMapValid, shadowCamera


    let getTimeSteps (samples : int) (trajectoryDurationInPast : TimeSpan) (currentTime : DateTime) =
        let trajectoryDuration = trajectoryDurationInPast
        let startTime = currentTime - trajectoryDuration
        Array.init samples (fun i -> 
            let time = currentTime - ((currentTime - startTime) / float samples) * float i
            let alpha = float i / float samples
            struct (time, alpha)
        ) 

    let getTimeStepsPlain (samples : int) (trajectoryDurationInPast : TimeSpan) (currentTime : DateTime) =
        let sampleLength = trajectoryDurationInPast / float samples
        List.init samples (fun i -> 
            let time = currentTime - sampleLength * float i
            let alpha = float i / float samples
            struct (time, alpha)
        ) 


    let trajectoryVisualization (referenceFrame : string) (observer : aval<string>) (time : aval<DateTime>) 
                                (getTrajectorySamples : string -> aval<TimeSpan * int>) 
                                (showTrajectory : string -> aval<bool>)
                                (trajectoryColor : string -> aval<C4b>) (bodies : aset<BodyDesc>) =
 

        let trajectories =
            bodies
            |> ASet.mapA (fun b -> 
                adaptive {
                    let! show = showTrajectory b.name
                    if show then
                        let! trajectoryLength, trajectorySamples = getTrajectorySamples(b.name)

                        // this one only works properly when getTimeStemps provides temporal coherence (which is currently not the case)
                        let cache = LruCache(int64 (trajectorySamples))
                       
                        let! observer = observer

                        let mutable hits = 0
                        let getPosition (time : DateTime) = 
                            let mutable wasHit = true
                            let r = 
                                cache.GetOrAdd(time, 1, fun _ -> 
                                    wasHit <- false
                                    CooTransformation.getRelState b.name defaultSupportBodyWhenIrrelevant observer time referenceFrame
                                )
                            if wasHit then 
                                hits <- hits + 1
                            r

                        let mutable oldTime = None
                        let mutable oldTimes = [||]
                        let! time = time
                        let times = 
                            let opt = true
                            // very ugly hack to allow smooth anomation and vary dynamic trajectory setting at the same time (it could be refactored as an alist acutally)
                            let lessThanOneSampleAway (ts : TimeSpan) = ts < (trajectoryLength / float trajectorySamples)
                            let lessThanTwoAway (ts : TimeSpan) = ts < (trajectoryLength / float trajectorySamples) * 2.0
                            match oldTime with
                            | Some lastTime when lessThanOneSampleAway (time - lastTime) && oldTimes.Length >= trajectorySamples  && opt -> 
                                oldTimes[0] <- struct (time, 1.0)
                                oldTimes
                            | Some lastTime when lessThanTwoAway (time - lastTime) && oldTimes.Length >= trajectorySamples && opt  ->
                                oldTime <- Some time
                                Array.append [|struct (time, 1.0)|] oldTimes[0 .. oldTimes.Length - 2]
                            | _ -> 
                                oldTime <- Some time
                                getTimeStepsPlain trajectorySamples trajectoryLength time |> List.toArray
                                

                        hits <- 0
                        let r = 
                            times 
                            |> Seq.toArray
                            |> Array.choose (fun (struct (t, alpha)) -> 
                                getPosition t
                            )
                            |> Array.pairwise
                            |> Array.map (fun (p0, p1) -> 
                                Line3d(p0.pos, p1.pos)
                            )
                            |> Seq.toArray

                        oldTimes <- times |> Seq.toArray
                        //printfn "hits %s: %d/%d" b.name hits  times.Length

                        return r, trajectoryColor b.name
                    else 
                        return [||], trajectoryColor b.name
                }
            ) 

        let offset = 
            trajectories.Content
            |> AVal.map (fun trajectories ->
                match Seq.tryHead trajectories with
                | Some (t,_) -> 
                    match Array.tryHead t with
                    | Some l -> Trafo3d.Translation l.P0
                    | _ -> Trafo3d.Identity
                | _ -> Trafo3d.Identity
             )

        let trajectoryVisualizations = 
            trajectories
            |> ASet.map (fun (trajectory, c) ->
                let lines = 
                    offset |> AVal.map (fun offset -> 
                        trajectory |> Array.map (fun l -> l.Transformed(offset.Backward))
                    )
                Sg.lines c lines
                |> Sg.trafo offset
            )
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
            }

        trajectoryVisualizations