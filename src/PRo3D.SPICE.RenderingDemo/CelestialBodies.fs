namespace PRo3D.SPICE

open FSharp.Data.Adaptive
open Aardvark.Base

[<AutoOpen>]
module CelestialBodies =

    [<Measure>] type m
    [<Measure>] type km
    [<Measure>] type s
    let meterToKilometers (m : float<m>) = m / 1000.0<m / km> 
    let kmToMeters (m : float<km>) = m * 1000.0<m / km> 

    type BodyDesc = 
        {
            // body name in spice nomenclature
            name: string
            // visual appearance ;)
            color : C4f
            // diameter
            diameter : float<km>
            // good observer (when setting the camera the body, another body which can be used to look at the body.
            goodObserver : string
        }


    let bodySources = 
        [|  { name = "sun"        ; color = C4f.White;     diameter = 1392700.0<km>;  goodObserver = "mercury" }
            { name = "mercury"    ; color = C4f.Gray;      diameter = 12742.0<km>;    goodObserver = "earth"   }
            { name = "venus"      ; color = C4f.AliceBlue; diameter = 12742.0<km>;    goodObserver = "earth"   }
            { name = "earth"      ; color = C4f.Blue;      diameter = 12742.0<km>;    goodObserver = "moon"    }
            { name = "moon"       ; color = C4f.Beige;     diameter = 34748.0<km>;    goodObserver = "earth"   }
            { name = "mars"       ; color = C4f.Red;       diameter = 6779.0<km>;     goodObserver = "phobos"  }
            { name = "phobos"     ; color = C4f.Red;       diameter = 22.533<km>;     goodObserver = "mars"    }
            { name = "deimos"     ; color = C4f.Red;       diameter = 12.4<km>;       goodObserver = "mars"    }
            { name = "HERA"       ; color = C4f.Magenta;   diameter = 0.00001<km>;    goodObserver = "mars"    }
            { name = "HERA_AFC-1" ; color = C4f.White;     diameter = 0.00001<km>;    goodObserver = "mars"    }
        |]   


    let getBodySource (name : string) = bodySources |> Array.tryFind (fun s -> s.name = name)