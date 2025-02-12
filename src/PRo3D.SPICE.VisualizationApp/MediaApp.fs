namespace PRo3D.SPICE.VisualizationApp

open Aardvark.UI
open Aardvark.UI.Primitives

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Rendering

type Message = 
    | Camera of OrbitMessage


module App =

    let initialCamera = OrbitController.initial 

    let update (model : Model) (msg : Message) =
        match msg with
        | Camera m ->
            { model with cameraState = OrbitController.update model.cameraState m }

    let viewScene (model : AdaptiveModel) =
        Sg.box (AVal.constant C4b.Green) (AVal.constant Box3d.Unit)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.simpleLighting
        }


    let view (model : AdaptiveModel) =

        let renderControl =
           OrbitController.controlledControl model.cameraState Camera (Frustum.perspective 60.0 0.1 100.0 1.0 |> AVal.constant)
                        (AttributeMap.ofListCond [
                            always <| style "width: 100%; grid-row: 2; height:100%; background:black";
                            always <| attribute "showFPS" "true";         // optional, default is false
                            //attribute "showLoader" "false"    // optional, default is true
                            //attribute "data-renderalways" "1" // optional, default is incremental rendering
                            always <| attribute "data-samples" "4"        // optional, default is 1
                        ])
                (viewScene model)


        div [style "display: grid; grid-template-rows: 40px 1fr; width: 100%; height: 100%" ] [
            div [style "grid-row: 1"] [
                text "Hello 3D"
                br []
            ]
            renderControl
            br []
            text "use first person shooter WASD + mouse controls to control the 3d scene"
        ]

    let threads (model : Model) =
        OrbitController.threads model.cameraState |> ThreadPool.map Camera


    let app : App<_,_,_> =
        {
            unpersist = Unpersist.instance
            threads = threads
            initial =
                {
                   cameraState = initialCamera
                }
            update = update
            view = view
        }