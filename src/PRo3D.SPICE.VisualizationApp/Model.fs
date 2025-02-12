namespace PRo3D.SPICE.VisualizationApp


open System

open Aardvark.UI.Primitives
open Adaptify.FSharp.Core
open Adaptify

[<ModelType>]
type Model =
    {
        count : int


        trajectoryLength : TimeSpan
        currentTime : DateTime
    }