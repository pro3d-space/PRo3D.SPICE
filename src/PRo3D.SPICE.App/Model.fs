namespace App

open System
open Adaptify
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Dom

type RefEqual<'a>(v : 'a) =
    member x.Value = v

[<ModelType>]
type Trail = 
    {
        positions : RefEqual<V3d[]>
    }

type Trail with
    member x.Last = 
        let arr = x.positions.Value
        if arr.Length = 0 then None
        else
            arr[arr.Length - 1] |> Some

module Trail =
    let empty = { positions = RefEqual [||] }

[<AutoOpen>]
module Units =
    [<Measure>] type m
    [<Measure>] type km

    let mPerKm : float<m / km> = 1000.0<m / km>


[<ModelType>]
type SimulatedTime = 
    {
        time : DateTime
    }

[<ModelType>]
type Model =
    {
        simulationTime  : SimulatedTime
    }

type BodyDesc = 
    {
        name          : string;
        abstractColor : C4f
        diameter      : double<km>
    }

module Model = 


    let bodySources = 
        [| { name = "sun";        abstractColor = C4f.White;     diameter = 1392700.0<km>  }
           { name = "mercury";    abstractColor = C4f.Gray;      diameter = 12742.0<km>    }
           { name = "venus";      abstractColor = C4f.AliceBlue; diameter = 12742.0<km>    }
           { name = "earth";      abstractColor = C4f.Blue;      diameter = 12742.0<km>    }
           { name = "moon";       abstractColor = C4f.Beige;     diameter = 34748.0<km>    }
           { name = "mars";       abstractColor = C4f.Red;       diameter = 6779.0<km>     }
           { name = "phobos";     abstractColor = C4f.Red;       diameter = 22.533<km>     }
           { name = "deimos";     abstractColor = C4f.Red;       diameter = 12.4<km>       }
           { name = "HERA";       abstractColor = C4f.Magenta;   diameter = 0.00001<km>    }
           { name = "HERA_AFC-1"; abstractColor = C4f.White;     diameter = 0.00001<km>    }
        |]

    let initial (s : SimulatedTime)= 
        {
            simulationTime = s
        }