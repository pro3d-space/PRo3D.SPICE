//18d689e1-c0dc-b773-f259-b6a5c49c873a
//cf74113b-dd04-057c-e431-38e7bf67c7f7
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "1182" // value is unused
namespace rec PRo3D.SPICE.VisualizationApp

open System
open FSharp.Data.Adaptive
open Adaptify
open PRo3D.SPICE.VisualizationApp
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveModel(value : Model) =
    let _count_ = FSharp.Data.Adaptive.cval(value.count)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Model) = AdaptiveModel(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Model) -> AdaptiveModel(value)) (fun (adaptive : AdaptiveModel) (value : Model) -> adaptive.Update(value))
    member __.Update(value : Model) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Model>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _count_.Value <- value.count
    member __.Current = __adaptive
    member __.count = _count_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>

