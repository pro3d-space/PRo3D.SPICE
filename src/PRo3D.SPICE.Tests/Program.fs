#nowarn "9"
open System
open System.IO
open System.Runtime.InteropServices
open FSharp.NativeInterop

open Expecto

open PRo3D.Extensions
open PRo3D.Extensions.FSharp

/// Direct P/Invoke into the re-exported CSPICE C API shipped as the native
/// "cspice" library (see PRo3D-Extensions / SpiceShared). This demonstrates
/// that consumers can call SPICE entry points directly, without going through
/// the CooTransformation wrapper. Both functions used here need no kernels.
module CSpiceDirect =
    [<DllImport("cspice", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr tkvrsn_c(string item)

    [<DllImport("cspice", CallingConvention = CallingConvention.Cdecl)>]
    extern double vnorm_c(double[] v1)

    [<DllImport("cspice", CallingConvention = CallingConvention.Cdecl)>]
    extern void furnsh_c(string file)

    [<DllImport("cspice", CallingConvention = CallingConvention.Cdecl)>]
    extern void str2et_c(string str, double& et)

let logDir = Path.Combine(".", "logs")
let spiceRoot = Path.Combine(__SOURCE_DIRECTORY__, "..", "..")
let heraKernelPath = Path.Combine(spiceRoot, "spice_kernels", "kernels", "mk", "hera_ops.tm")

// Tests that need ephemeris / mission kernels (provided locally by the private
// hera submodule) are skipped when those kernels are absent -- e.g. in CI,
// which only runs the basic tests covered by the embedded default kernels.
let hasExternalKernels = File.Exists(heraKernelPath)

do Aardvark.Base.Aardvark.UnpackNativeDependencies(typeof<CooTransformation.RelState>.Assembly)

let init () =
    if not (Directory.Exists(logDir)) then 
        Directory.CreateDirectory(logDir) |> ignore

    let r = CooTransformation.Init(true, Path.Combine(logDir, "CooTrafo.log"), 4, 4)
    if r <> 0 then failwith "init failed."
    { new IDisposable with member x.Dispose() = CooTransformation.DeInit()}

let tests () =
    testSequenced <| testList "init" [
        test "LoadDefaultSpiceKernels" {
            use _ = init()
            DefaultSpiceKernels.loadDefaults()

            // Jezero crater on Mars: lat 18.444, lon 77.451, alt 0
            let jezeroLat = 18.444
            let jezeroLon = 77.451
            let jezeroAlt = 0.0

            let mutable px, py, pz = 0.0, 0.0, 0.0
            let r1 = CooTransformation.LatLonAlt2Xyz("MARS", jezeroLat, jezeroLon, jezeroAlt, &px, &py, &pz)
            Expect.equal 0 r1 "LatLonAlt2Xyz failed"
            printfn "Jezero XYZ: %f, %f, %f" px py pz

            let mutable lat2, lon2, alt2 = 0.0, 0.0, 0.0
            let r2 = CooTransformation.Xyz2LatLonAlt("MARS", px, py, pz, &lat2, &lon2, &alt2)
            Expect.equal 0 r2 "Xyz2LatLonAlt failed"
            printfn "Round-trip LatLonAlt: %f, %f, %f" lat2 lon2 alt2

            Expect.floatClose Accuracy.medium jezeroLat lat2 "latitude round-trip"
            Expect.floatClose Accuracy.medium jezeroLon lon2 "longitude round-trip"
            Expect.floatClose Accuracy.medium jezeroAlt alt2 "altitude round-trip"
        }
        test "InitDeInit" {
            let i = init()
            i.Dispose()
        }
        test "CorrectVersion" {
            use _ = init()
            let v = CooTransformation.GetAPIVersion()
            Expect.equal v 5u "returned wrong version"
        }

        test "CSpiceDirectStr2Et" {
            // Exercises the default leapseconds kernel (naif0012.tls) through the
            // re-exported cspice library: furnsh the LSK, then convert UTC -> ET.
            use _ = init()
            DefaultSpiceKernels.loadDefaults()  // also extracts the kernels to disk
            let lsk = Path.Combine(DefaultSpiceKernels.defaultKernelDir, "naif0012.tls")
            CSpiceDirect.furnsh_c(lsk)          // load LSK into cspice's own kernel pool
            let mutable et = 0.0
            CSpiceDirect.str2et_c("2020-01-01 00:00:00", &et)
            printfn "ET for 2020-01-01 00:00:00 UTC (cspice str2et_c): %f" et
            // ~20 years (minus 12h) after the J2000 epoch, in TDB seconds.
            Expect.isTrue (et > 6.0e8 && et < 6.4e8) "ET outside expected range"
        }

        test "CSpiceDirectPInvoke" {
            // Call the re-exported CSPICE C API directly through the native
            // "cspice" library (no CooTransformation wrapper, no kernels needed).
            let version = Marshal.PtrToStringAnsi(CSpiceDirect.tkvrsn_c("TOOLKIT"))
            printfn "CSPICE toolkit version (direct P/Invoke): %s" version
            Expect.isNotNull version "tkvrsn_c returned null"
            Expect.stringStarts version "CSPICE" "unexpected toolkit version string"

            let n = CSpiceDirect.vnorm_c([| 3.0; 4.0; 0.0 |])
            Expect.floatClose Accuracy.high 5.0 n "vnorm_c({3,4,0}) should be 5"
        }

        if hasExternalKernels then
            test "GetRelState" {
                use _ = init()
                DefaultSpiceKernels.loadDefaults()
                let t = "2026-12-03 08:15:00.00"
                let p : double[] = Array.zeroCreate 3
                let m : double[] = Array.zeroCreate 9
                let pdPosVec = fixed &p[0]
                let pdRotMat = fixed &m[0]
                let result = CooTransformation.GetRelState("EARTH", "SUN", "MOON", t, "J2000", NativePtr.toNativeInt pdPosVec, NativePtr.toNativeInt pdRotMat)
                Expect.equal result 0 "GetRelState" // returns -1
            }
        else
            printfn "WARNING: Skipping GetRelState test - external (ephemeris) kernels not found"

        test "LatLonToXyz" {
            use _ = init()
            DefaultSpiceKernels.loadDefaults()
            let mutable lat,lon,alt = 0.0,0.0,0.0
            let result = CooTransformation.Xyz2LatLonAlt("mars", 1.0, 1.0, 1.0, &lat, &lon, &alt)
            Expect.equal 0 result "Xyz2LatLonAlt result code"
        }
        test "XyzToLatLon" {
            use _ = init()
            DefaultSpiceKernels.loadDefaults()
            let mutable px,py,pz = 0.0,0.0,0.0
            let result = CooTransformation.LatLonAlt2Xyz("MARS", 18.447, 77.402, 0, &px, &py, &pz)
            printfn "%A" (py, py, pz)
            Expect.equal 0 result "LatLonAlt2Xyz result code"
        }

        if File.Exists(heraKernelPath) then
            test "GetPositionTransformationMatrix" {
                use _ = init()
                let previousDir = System.Environment.CurrentDirectory
                System.Environment.CurrentDirectory <- Path.GetDirectoryName(heraKernelPath)
                CooTransformation.AddSpiceKernel(heraKernelPath) |> ignore
                System.Environment.CurrentDirectory <- previousDir
                let t = "2020-12-03 08:15:00.00"
                let m : double[] = Array.zeroCreate 9
                let pdMat = fixed &m[0]
                let result = CooTransformation.GetPositionTransformationMatrix("IAU_MARS", "J2000", t, pdMat)
                Expect.equal 0 result "GetPositionTransformationMatrix"
            }
        else
            printfn "WARNING: Skipping GetPositionTransformationMatrix test - HERA spice kernels not found at %s" heraKernelPath
    ]


[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] args (tests ())