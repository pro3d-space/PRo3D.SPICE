namespace PRo3D.SPICE

[<AutoOpen>]
module Units =
    [<Measure>] type m
    [<Measure>] type km

    let mPerKm : float<m / km> = 1000.0<m / km>
