// Packaged-consumer native-delivery test.
//
// Reproduces the exact native path a real nuget consumer (PRo3D) hits:
//   PRo3D.SPICE.dll (natives embedded, compressed, no runtimes/ folder)
//     -> Aardvark.Base.Aardvark.UnpackNativeDependencies extracts for os/arch
//        -> P/Invoke CooTransformation
// On a broken osx-arm64 mapping the unpack lays down the x86_64 dylib and
// Init() throws "incompatible architecture" -- the failure this test exists to
// catch on every CI platform.

module PRo3D.SPICE.PackagedConsumerTest.Program

open System
open System.IO
open System.Runtime.InteropServices

open PRo3D.Extensions
open PRo3D.Extensions.FSharp

let mutable failures = 0
let fail (msg : string) =
    eprintfn "FAIL: %s" msg
    failures <- failures + 1
let check cond msg = if not cond then fail msg

// Native file names CooTransformation / cspice can take on any platform.
let private nativeNames =
    [ "CooTransformation.dll"; "libCooTransformation.so"; "libCooTransformation.dylib"
      "cspice.dll"; "libcspice.so"; "libcspice.dylib" ]

let private nativesIn (dir : string) =
    nativeNames |> List.map (fun n -> Path.Combine(dir, n)) |> List.filter File.Exists

[<EntryPoint>]
let main _ =
    let spiceAssembly = typeof<CooTransformation.RelState>.Assembly
    let asmDir = Path.GetDirectoryName(spiceAssembly.Location)

    printfn "== packaged-consumer native-unpack test =="
    printfn "OS           : %s" (RuntimeInformation.OSDescription)
    printfn "Process arch : %A" RuntimeInformation.ProcessArchitecture
    printfn "RID          : %s" RuntimeInformation.RuntimeIdentifier
    printfn "PRo3D.SPICE  : %s" spiceAssembly.Location

    // Force the embed/unpack path: remove any native already sitting next to
    // the managed DLL, so a passing Init can ONLY mean UnpackNativeDependencies
    // placed the arch-correct binary. (From the nupkg there are none, but this
    // makes the guarantee explicit and robust if run against a dirty output.)
    for f in nativesIn asmDir do
        printfn "removing pre-existing native (would mask the unpack path): %s" f
        File.Delete f
    check (List.isEmpty (nativesIn asmDir))
          "native libs still present next to the DLL -- cannot isolate the unpack path"

    printfn "calling Aardvark.Base.Aardvark.UnpackNativeDependencies ..."
    Aardvark.Base.Aardvark.UnpackNativeDependencies(spiceAssembly)

    let unpacked = nativesIn asmDir |> List.map Path.GetFileName
    printfn "natives present after unpack: %A" unpacked
    check (unpacked |> List.exists (fun n -> n.Contains "CooTransformation"))
          "UnpackNativeDependencies produced no CooTransformation native for this os/arch"

    // Actually P/Invoke it. A wrong-arch native throws here
    // (DllNotFoundException / BadImageFormat "incompatible architecture") --
    // this is the real arch assertion, not just a file-exists check.
    let logDir = Path.Combine(asmDir, "logs")
    Directory.CreateDirectory logDir |> ignore
    let rInit = CooTransformation.Init(true, Path.Combine(logDir, "packaged-consumer.log"), 4, 4)
    check (rInit = 0) (sprintf "CooTransformation.Init returned %d" rInit)

    if rInit = 0 then
        try
            // Trivial SPICE work through the freshly unpacked native.
            let v = CooTransformation.GetAPIVersion()
            printfn "CooTransformation API version: %d" v
            check (v = 7u) (sprintf "unexpected API version %d (expected 7)" v)

            DefaultSpiceKernels.loadDefaults()

            // Jezero crater on Mars -> XYZ -> back; must round-trip.
            let jLat, jLon, jAlt = 18.444, 77.451, 0.0
            let mutable px, py, pz = 0.0, 0.0, 0.0
            let r1 = CooTransformation.LatLonAlt2Xyz("MARS", jLat, jLon, jAlt, &px, &py, &pz)
            check (r1 = 0) (sprintf "LatLonAlt2Xyz returned %d" r1)
            printfn "Jezero XYZ: %f, %f, %f" px py pz

            let mutable lat2, lon2, alt2 = 0.0, 0.0, 0.0
            let r2 = CooTransformation.Xyz2LatLonAlt("MARS", px, py, pz, &lat2, &lon2, &alt2)
            check (r2 = 0) (sprintf "Xyz2LatLonAlt returned %d" r2)
            printfn "round-trip lat/lon/alt: %f, %f, %f" lat2 lon2 alt2
            check (abs (lat2 - jLat) < 1e-3) "latitude round-trip off"
            check (abs (lon2 - jLon) < 1e-3) "longitude round-trip off"
            check (abs (alt2 - jAlt) < 1e-3) "altitude round-trip off"
        finally
            CooTransformation.DeInit()

    if failures = 0 then
        printfn "SUCCESS: embedded natives unpacked and SPICE calls worked on this os/arch."
        0
    else
        eprintfn "FAILED with %d error(s)." failures
        1
