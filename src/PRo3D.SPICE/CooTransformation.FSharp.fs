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

    let l = obj()

    module Time =

        let toUtcFormat (d : DateTime) = 
            d.ToUniversalTime()
             .ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'");


    let getRelState (pcTargetBody : string) (pcSupportBody : string) (pcObserverBody : string) 
                    (pcObserverDateTime : DateTime) (pcOutputReferenceFrame : string) =
                     
        lock l (fun _ -> 

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
                // CooTransformation stores row 
                Some { pos = V3d(p[0],p[1],p[2]); vel = V3d.Zero; rot = M33d(m).Transposed }
        )


    let getRotationTrafo (fromFrame : string) (toFrame : string) (time : DateTime) =
        lock l (fun _ -> 
            let m : double[] = Array.zeroCreate 9
            let pdMat = fixed &m[0]
            let r = CooTransformation.GetPositionTransformationMatrix(fromFrame, toFrame, Time.toUtcFormat time, pdMat) 
            let rot = M33d(m)
            if r = 0 && rot.Determinant > 0.95 then
                let forward = M44d(rot)
                Trafo3d(forward, forward.Inverse) |> Some
            else
                printfn "could not get rot trafo for frame: %s" fromFrame
                Trafo3d.Identity |> Some
        )
            