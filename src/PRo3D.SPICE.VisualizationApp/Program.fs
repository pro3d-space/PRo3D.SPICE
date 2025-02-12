open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Giraffe

open Aardvark.Base

open Aardvark.Dom
open Aardvark.Dom.Remote

open Aardvark.Application.Slim
open Aardium

open Aardvark.Dom.Remote
open PRo3D.SPICE.VisualizationApp


[<EntryPoint>]
let main _ =
    Aardium.init()

    Aardvark.Init()
    let app = new OpenGlApplication()
    let noDisposable = { new System.IDisposable with member x.Dispose() = () }


    let run (ctx : DomContext) = 
        App.start ctx (DomApp.app ())


    let service = 
        task {
            let! _ =
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
                    .StartAsync()

            Aardium.run {
                url "http://localhost:5000/"
                width 1024
                height 768
                debug true
            }
        }

    service.Wait()
    0
