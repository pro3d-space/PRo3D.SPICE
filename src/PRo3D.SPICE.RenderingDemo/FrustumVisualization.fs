namespace Aardvark.SceneGraph

open FSharp.Data.Adaptive
open FSharp.Data.Adaptive.Operators

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph

open Aardvark.Reconstruction

[<AutoOpen>]
module ``Camera Sg Extensions`` =

    module Sg = 

        module private Shader =
            open FShade

            type UniformScope with
                member x.Alpha : float = uniform?Alpha
                member x.OtherViewProj : M44d = uniform?OtherViewProj


            let alpha (v : Effects.Vertex) =
                fragment {
                    return V4d(v.c.XYZ, v.c.W * uniform.Alpha)
                }


            let sammy =
                sampler2d {
                    texture uniform?DiffuseColorTexture
                    addressU WrapMode.Wrap
                    addressV WrapMode.Wrap
                    filter Filter.MinMagMipLinear
                }

            type Verty =
                {
                    [<Position>] pos : V4d
                    [<Semantic("ModelPos")>] mp : V4d
                }

            let modelPos (v : Effects.Vertex) =
                vertex {
                    return { pos = v.pos; mp = v.pos }
                }

            let reprojectedTexture (v : Verty) =
                fragment {
                    let w = uniform.OtherViewProj * v.mp
                    let w = w.XY / w.W
                    let tc = w * V2d(0.5, 0.5) + V2d(0.5, 0.5)
                    return sammy.Sample(tc)
                }


        let cameraImagePass = RenderPass.after "cameraImagePass" RenderPassOrder.Arbitrary RenderPass.main
        let afterCameraImagePass = RenderPass.after "AfterCameraImagePass" RenderPassOrder.Arbitrary cameraImagePass
   
        let unitFrustum =
            let call = DrawCallInfo(FaceVertexCount = 16, InstanceCount = 1)

            let positions =
                [|
                    V3f.Zero
                    V3f(-1.0f, -1.0f, -1.0f)
                    V3f( 1.0f, -1.0f, -1.0f)
                    V3f( 1.0f,  1.0f, -1.0f)
                    V3f(-1.0f,  1.0f, -1.0f)
                |]

            let colors =
                [|
                    C4b.Yellow
                    C4b.Yellow
                    C4b.Yellow
                    C4b.Yellow
                    C4b.Yellow
                |]

            let index =
                [|
                    0; 1
                    0; 2
                    0; 3
                    0; 4
                    1; 2
                    2; 3
                    3; 4
                    4; 1
                |]

            Sg.render IndexedGeometryMode.LineList call
                |> Sg.vertexAttribute DefaultSemantic.Positions ~~positions
                |> Sg.vertexAttribute DefaultSemantic.Colors ~~colors
                |> Sg.index ~~index

        let focalPlane =
            let call = DrawCallInfo(FaceVertexCount = 6, InstanceCount = 1)

            let positions =
                [|
                    V3f(-1.0f, -1.0f, -1.0f)
                    V3f( 1.0f, -1.0f, -1.0f)
                    V3f( 1.0f,  1.0f, -1.0f)
                    V3f(-1.0f,  1.0f, -1.0f)
                |]

            let texCoords =
                [|
                    V2f.OO
                    V2f.IO
                    V2f.II
                    V2f.OI
                |]

            let index =
                [|
                    0; 1; 2
                    0; 2; 3
                |]

            Sg.render IndexedGeometryMode.TriangleList call
                |> Sg.vertexAttribute DefaultSemantic.Positions ~~positions
                |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates ~~texCoords
                |> Sg.index ~~index

        let camera (scale : aval<float>) (c : Camera) =
            let view = Camera.viewTrafo c
        
            let center = Sg.lines ~~C4b.Red ~~[| Line3d(V3d.Zero, -V3d.OOI) |]


            // pp.X = t * (0,0,-1)
            // pp.Y = t * (0,0,-1)
            let pp = -c.proj.principalPoint
        

            Sg.ofList [ 
                unitFrustum
                |> Sg.transform (Trafo3d.ShearXY(pp.X, pp.Y) * Trafo3d.Scale(1.0, 1.0 / c.proj.aspect, c.proj.focalLength))
               
                center
                |> Sg.transform (Trafo3d.Scale(1.0, 1.0 / c.proj.aspect, c.proj.focalLength))
            ]
            |> Sg.trafo (scale |> AVal.map Trafo3d.Scale)
            |> Sg.transform view.Inverse
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.thickLine
                do! DefaultSurfaces.thickLineRoundCaps
                //do! DefaultSurfaces.vertexColor
            }
            |> Sg.uniform "LineWidth" ~~3.0
    
        let cameraPhoto (scale : aval<float>) (alpha : aval<float>) (imgPath : string) (c : Camera) =
            let view = Camera.viewTrafo c


            // pp.X = t * (0,0,-1)
            // pp.Y = t * (0,0,-1)
            let pp = -c.proj.principalPoint

            focalPlane
            |> Sg.transform (Trafo3d.ShearXY(pp.X, pp.Y) * Trafo3d.Scale(1.0, 1.0 / c.proj.aspect, c.proj.focalLength))
            |> Sg.trafo (scale |> AVal.map Trafo3d.Scale)
            |> Sg.diffuseFileTexture imgPath true
            |> Sg.transform view.Inverse
            |> Sg.blendMode ~~BlendMode.Blend
            |> Sg.pass cameraImagePass
            |> Sg.uniform "Alpha" alpha
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.diffuseTexture 
                do! Shader.alpha
            }

        let cameraWithPhoto (scale : aval<float>) (alpha : aval<float>) (imgPath : string) (c : Camera) =
            Sg.ofList [
                camera scale c
                cameraPhoto scale alpha imgPath c
            ]