open System
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

open App



module Elm =
    open Adaptify
    open FSharp.Data.Adaptive

    type Message =
        | SetSimulationTime of DateTime

    let updateSimulation (model : Model) (simulationTime : DateTime) =
        model

    let update (env : Env<Message>) (model : Model) (msg : Message) =
        match msg with
        | SetSimulationTime(t) -> 
            model

    let renderPlanets (model : AdaptiveModel) (viewProj : aval<Trafo3d>) =
        let vertices =
            (model.bodies.Content, viewProj) ||> AVal.map2 (fun bodies viewProj -> 
                bodies |> HashMap.toArray |> Array.choose (fun (_, body) -> 
                    match body.pos with
                    | None -> None
                    | Some pos -> 
                        let ndc = viewProj.Forward.TransformPosProj(pos) |> V3f
                        Some ndc
                )
            )
        sg {
            Sg.VertexAttribute(DefaultSemantic.Positions.ToString(), vertices)
            Sg.Render(vertices |> AVal.map (fun v -> v.Length))
        }
       

    let view (env : Env<Message>) (model : AdaptiveModel) =
        body {
            h1 { "PRo3D.SPICE.App" }


            renderControl  {
                Style [Width "100%"; Height "600px"; Background "#202124"] 
                Samples 8
                TabIndex 0

                let! size = RenderControl.ViewportSize
                let! time = RenderControl.Time

                let view = CameraView.lookAt (V3d(3,4,5)) V3d.Zero V3d.OOI |> CameraView.viewTrafo |> AVal.constant
                let proj = size |> AVal.map (fun s -> Frustum.perspective 80.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
                Sg.View view
                Sg.Proj proj

                let viewProj = (view, proj) ||> AVal.map2 (fun v p -> v * p)

                RenderControl.OnRendered(fun s ->
                    Rendered(s.FrameTime, V2d s.Size, viewProj.GetValue()) |> env.Emit
                )

                //// apply a camera

   
            }

        }

    let app s =
        {
            initial = Model.initial s
            update = update
            view = view
            unpersist = Unpersist.instance
        }



[<EntryPoint>]
let main _ =
    Aardvark.Init()

    let app = new OpenGlApplication()

    let time = DateTime.Parse("2025-03-10 19:08:12.60")
    let time = DateTime.Parse("2025-03-10 19:08:12.60")
    //let time = DateTime.Parse("2024-12-01 19:08:12.60")

    let run (ctx : DomContext) = 
        App.start ctx (Elm.app { time = time })


    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseSockets()
                    .Configure(fun b -> b.UseWebSockets().UseGiraffe (DomNode.toRoute app.Runtime run))
                    .ConfigureServices(fun s -> s.AddGiraffe() |> ignore)
                    |> ignore
        )
        .Build()
        .Run()
    0


    