#nowarn "9"
open System
open System.IO
open FSharp.NativeInterop

open Expecto

open PRo3D.Extensions
open PRo3D.Extensions.FSharp

let logDir = Path.Combine(".", "logs")
let spiceRoot = Path.Combine(__SOURCE_DIRECTORY__, "..", "..")
let spiceFileName = Path.Combine(spiceRoot, @"./hera/kernels/mk/hera_crema_2_0_LPO_ECP_PDP.tm")

do Aardvark.Base.Aardvark.UnpackNativeDependencies(typeof<CooTransformation.RelState>.Assembly)

let init () =
    if not (Directory.Exists(logDir)) then 
        Directory.CreateDirectory(logDir) |> ignore

    let r = CooTransformation.Init(true, Path.Combine(logDir, "CooTrafo.log"), 4, 4)
    if r <> 0 then failwith "init failed."
    { new IDisposable with member x.Dispose() = CooTransformation.DeInit()}

let tests () =
    testSequenced <| testList "init" [
        test "InitDeInit" {
            let i = init()
            i.Dispose()
        }
        test "CorrectVersion" {
            use _ = init()
            let v = CooTransformation.GetAPIVersion()
            Expect.equal v 5u "returned wrong version"
        }

        use _ = init()
        System.Environment.CurrentDirectory <- Path.GetDirectoryName(spiceFileName)
        let init = CooTransformation.AddSpiceKernel(spiceFileName)
        Expect.equal 0 init "spice adding"

        test "GetRelState" {
            let t = "2026-12-03 08:15:00.00"
            let p : double[] = Array.zeroCreate 3
            let m : double[] = Array.zeroCreate 9
            let pdPosVec = fixed &p[0]
            let pdRotMat = fixed &m[0]
            let result = CooTransformation.GetRelState("EARTH", "SUN", "MOON", t, "J2000", NativePtr.toNativeInt pdPosVec, NativePtr.toNativeInt pdRotMat)
            Expect.equal result 0 "GetRelState" // returns -1
        }

        test "LatLonToXyz" {
            let mutable lat,lon,alt = 0.0,0.0,0.0
            let result = CooTransformation.Xyz2LatLonAlt("mars", 1.0, 1.0, 1.0, &lat, &lon, &alt)
            Expect.equal 0 result "Xyz2LatLonAlt result code"
        }
        test "XyzToLatLon" {
            let mutable px,py,pz = 0.0,0.0,0.0
            let result = CooTransformation.LatLonAlt2Xyz("MARS", 18.447, 77.402, 0, &px, &py, &pz)
            printfn "%A" (py, py, pz)
            Expect.equal 0 result "LatLonAlt2Xyz result code"
        }

        test "GetPositionTransformationMatrix" {
            let t = "2026-12-03 08:15:00.00"
            let m : double[] = Array.zeroCreate 9
            let pdMat = fixed &m[0]
            let result = CooTransformation.GetPositionTransformationMatrix("IAU_EARTH", "J2000", t, pdMat)
            Expect.equal 0 result "GetPositionTransformationMatrix"
        }

    ]


[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] args (tests ())