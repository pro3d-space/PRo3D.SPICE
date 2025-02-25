namespace PRo3D.Base

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Rendering

module ImageProjection =

    module Shaders =

        open Aardvark.Base
    
        open FShade
        open Aardvark.Rendering.Effects


        type UniformScope with  
            member x.ProjectedImageModelViewProjValid : bool = uniform?ProjectedImageModelViewProjValid
            member x.ProjectedImageModelViewProj : M44d = uniform?ProjectedImageModelViewProj
            member x.ProjectedImagesLocalTrafos : M44d[] = uniform?StorageBuffer?ProjectedImagesLocalTrafos
            member x.ProjectedImagesCount : int = uniform?ProjectedImagesLocalTrafosCount

        type Vertex = {
            [<Position>]    pos     : V4d
            [<Semantic("ProjectedImagePos")>] projectedPos : V4d
            [<Color>] c: V4d
            [<Semantic("BodyLocalPos")>] localPos : V4d
            [<Semantic("LocalNormal")>] localNormal : V3d
            [<Normal>] n : V3d
        }

        let private projectedTexture =
            sampler2d {
                texture uniform?ProjectedTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Border
                addressV WrapMode.Border
                borderColor C4f.White
            }


        let stableImageProjectionTrafo (v : Vertex) =
            vertex {
                return { v with projectedPos = uniform.ProjectedImageModelViewProj * v.pos; localPos = v.pos; localNormal = v.n }
            }

        let stableImageProjection (v : Vertex) = 
            fragment {
                let p = v.projectedPos.XYZ / v.projectedPos.W
                let tc = V3d(0.5, 0.5,0.5) + V3d(0.5, 0.5, 0.5) * p.XYZ
                let inRange = Vec.allGreaterOrEqual tc V3d.OOO && Vec.allSmallerOrEqual tc.XYZ V3d.III

                let projN = uniform.ProjectedImageModelViewProj * V4d(v.localNormal, 0.0)
                let borderWidth = 0.01 

                let c = 
                    if uniform.ProjectedImageModelViewProjValid && inRange && projN.Z < 0.0 then
                        let c = projectedTexture.Sample(tc.XY) * v.c
                        let xBorder = (smoothstep 0.0 borderWidth tc.X) * smoothstep 1.0 (1.0 - borderWidth) tc.X 
                        let yBorder = (smoothstep 0.0 borderWidth tc.Y) * smoothstep 1.0 (1.0 - borderWidth) tc.Y
                        let borderFactor = xBorder * yBorder
                        let borderColor = V3d(0.0, 1.0, 0.0)
                        let c = c.XYZ * borderFactor + borderColor * (1.0 - borderFactor)
                        V4d(c, 1.0)
                        //let borderX = tc.X < borderWidth || tc.X > 1.0 - borderWidth 
                        //let borderY = tc.Y < borderWidth || tc.Y > 1.0 - borderWidth
                        //if borderX || borderY then 
                        //    V4d(borderColor, 1.0) 
                        //else
                        //    V4d(c, 1.0)
                    else
                        v.c
                return { v with c = c }
            }

        [<ReflectedDefinition>]
        let isBorder (tc : V3d) =
            let borderWidth = 0.0001 
            //let xBorder = (smoothstep 0.0 borderWidth tc.X) * smoothstep 1.0 (1.0 - borderWidth) tc.X 
            //let yBorder = (smoothstep 0.0 borderWidth tc.Y) * smoothstep 1.0 (1.0 - borderWidth) tc.Y
            //let borderFactor = xBorder * yBorder
            let borderX = tc.X < borderWidth || tc.X > 1.0 - borderWidth 
            let borderY = tc.Y < borderWidth || tc.Y > 1.0 - borderWidth
            borderX || borderY

        [<ReflectedDefinition>]
        let mapClippedProjectionsToColor (validCount : int) (totalCount : int) =
            let ratio = float validCount / float totalCount
            let color = 
                if ratio < 0.1 then V3d(0.0, 0.0, 1.0) // Blue
                elif ratio < 0.2 then V3d(0.0, 1.0, 1.0) // Cyan
                elif ratio < 0.3 then V3d(0.0, 1.0, 0.0) // Green
                elif ratio < 0.4 then V3d(1.0, 1.0, 0.0) // Yellow
                else V3d(1.0, 0.0, 0.0) // Red
            color

        let localImageProjections (v : Vertex) = 
            fragment {
                let mutable clippedCount = 0
                let mutable atLeastOneBorder = false
                for i in 0 .. uniform.ProjectedImagesCount - 1 do
                    let ndc = uniform.ProjectedImagesLocalTrafos[i] * v.localPos
                    let nP = uniform.ProjectedImagesLocalTrafos[i] * V4d(v.localNormal, 0.0)
                    let p = ndc.XYZ / ndc.W
                    let tc = V3d(0.5, 0.5, 0.5) + V3d(0.5, 0.5, 0.5) * p.XYZ
                    let clipped = Vec.anyGreater tc.XY V2d.II || Vec.anySmaller tc.XY V2d.OO
                    if nP.Z > 0 || clipped then
                        //atLeastOneBorder <- isBorder tc.XYZ
                        clippedCount <- clippedCount + 1

                if atLeastOneBorder then 
                    return V4d(0.0,1.0,0.0,1.0)
                else
                    if clippedCount < uniform.ProjectedImagesCount then 
                        let color = mapClippedProjectionsToColor (uniform.ProjectedImagesCount  - clippedCount) uniform.ProjectedImagesCount 
                        let c = v.c.XYZ * 0.8 + color * 0.2
                        return V4d(c, 1.0)
                    else 
                        return v.c
            }

module ImageProjectionTrafoSceneGraph =
    open Aardvark.Base.Ag
    open Aardvark.SceneGraph.Semantics.TrafoExtensions

    type PlanetApplicator(child : ISg, planet : string) =
        inherit Sg.AbstractApplicator(child)
        member x.Planet = planet
        
    [<Rule>]
    type PlanetSemantics() =
        member x.Planet(app : PlanetApplicator, scope : Ag.Scope) =
            app.Child?Planet <- app.Planet
        

    type ProjectedImageApplicator(child : ISg, viewProjection : string -> aval<Option<Camera>>) =
        inherit Sg.AbstractApplicator(child)
        member x.ViewProjection = viewProjection

    [<Rule>]
    type ProjectedImageSemantics() =
        member x.ProjectedImageModelViewProj(app : ProjectedImageApplicator, scope : Ag.Scope) =
            let planet : string = scope?Planet
            let projectionTrafo = app.ViewProjection planet
            let modelTrafo = scope.ModelTrafo 
            let possiblyTrafo = 
                projectionTrafo |> AVal.bind (function
                    | None -> AVal.constant None
                    | Some vp -> 
                        AVal.map (fun m -> m * Camera.viewProjTrafo vp |> Some) modelTrafo
                )
            let trafo = possiblyTrafo |> AVal.map (Option.defaultValue Trafo3d.Identity)
            app.Child?ProjectedImageModelViewProj <- trafo

     
module Sg = 
    open ImageProjectionTrafoSceneGraph

    let applyPlanet (planet : string) (sg : ISg) =
        PlanetApplicator(sg, planet)

    let applyProjectedImage (viewProjTrafo : string -> aval<Option<Camera>>) (sg : ISg) =
        ProjectedImageApplicator(sg, viewProjTrafo) :> ISg