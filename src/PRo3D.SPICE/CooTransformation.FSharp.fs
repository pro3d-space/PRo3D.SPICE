namespace PRo3D.Extensions.FSharp

open System
open System.Runtime.InteropServices
open FSharp.NativeInterop

open Aardvark.Base

#nowarn "9"

open PRo3D.Extensions

module CooTransformation =


    [<Struct>]
    type RelState = 
        {
            pos : V3d
            vel : V3d
            rot : M33d
        }

    module Time =

        let toUtcFormat (d : DateTime) = 
            d.ToUniversalTime()
             .ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'");


    let getRelState (pcTargetBody : string) (pcSupportBody : string) (pcObserverBody : string) 
                    (pcObserverDateTime : DateTime) (pcOutputReferenceFrame : string) =

        let p : double[] = Array.zeroCreate 3
        let m : double[] = Array.zeroCreate 9
        let r = 
            let pdPosVec = fixed &p[0]
            let pdRotMat = fixed &m[0]
            CooTransformation.GetRelState(pcTargetBody, pcSupportBody, pcObserverBody, Time.toUtcFormat pcObserverDateTime, 
                                          pcOutputReferenceFrame, NativePtr.toNativeInt pdPosVec, NativePtr.toNativeInt pdRotMat)
        if r <> 0 then 
            None
        else 
            Some { pos = V3d(p[0],p[1],p[2]); vel = V3d.Zero; rot = M33d(m).Transposed }

