namespace PRo3D.SPICE.VisualizationApp


open System.Threading
open System.Threading.Tasks
open System.Text.Json
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Giraffe
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Application
open Aardvark.Dom
open Aardvark.Dom.Remote
open Aardvark.Rendering
open Aardvark.Application.Slim

open Aardvark.Dom.Utilities

module DomApp =

    open Adaptify
    open FSharp.Data.Adaptive

    type Message =
        | Inc
        | Dec
        | Down of Button
        | Up of Button


    let update (env : Env<Message>) (model : Model) (msg : Message) =
        match msg with
        | Inc -> 
            { model with count = model.count + 1 }
        | Dec -> 
            { model with count = model.count - 1 }

    let view (env : Env<Message>) (model : AdaptiveModel) =
        body {
            h1 { model.count |> AVal.map (sprintf "Count: %d") }

            div {

                button {
                    Dom.OnClick (fun _ -> env.Emit Inc)
                    Dom.OnContextMenu ((fun _ -> env.Emit Inc), preventDefault = true)
                    "+"
                }
            
                button {
                    Dom.OnClick (fun _ -> env.Emit Dec)
                    Dom.OnContextMenu ((fun _ -> env.Emit Dec), preventDefault = true)
                    "-"
                }
                
                renderControl  {
                    Style [Width "100%"; Height "600px"; Background "#202124"] 
                    Samples 4
                    Quality 100
                
                    SimpleOrbitController {
                        Location = V3d.III * 10.0
                        Center = V3d.Zero
                        RotateButton = Button.Left
                        PanButton = Button.Middle  
                    }

                    let! size = RenderControl.ViewportSize
                    let! time = RenderControl.Time

                    // apply a camera
                    //let view = CameraView.lookAt (V3d(3,4,5)) V3d.Zero V3d.OOI |> CameraView.viewTrafo |> AVal.constant
                    let proj = size |> AVal.map (fun s -> Frustum.perspective 80.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
                    //Sg.View view
                    Sg.Proj proj

                    Shader { DefaultSurfaces.trafo; DefaultSurfaces.simpleLighting }  
                    Sg.Draw (Aardvark.SceneGraph.IndexedGeometryPrimitives.solidPhiThetaSphere Sphere3d.Unit 20 C4b.White)
                    RenderControlBuilderExt.wrap sg
                }
            
            }
        }

    let app _ =
        {
            initial = { count = 0; }
            update = update
            view = view
            unpersist = Unpersist.instance
        }