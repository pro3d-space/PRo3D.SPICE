namespace PRo3D.Extensions

open System
open System.IO
open System.Reflection

module DefaultSpiceKernels =

    let private assembly = Assembly.GetExecutingAssembly()

    let private embeddedKernels =
        [| "PRo3D.SPICE.resources.defaultSpiceKernel.naif0012.tls"
           "PRo3D.SPICE.resources.defaultSpiceKernel.pck00010.tpc"
           "PRo3D.SPICE.resources.defaultSpiceKernel.pck00010-base.tpc" |]

    let private resourceFileName (resourceName : string) =
        let parts = resourceName.Split('.')
        $"{parts.[parts.Length - 2]}.{parts.[parts.Length - 1]}"

    let private extractResource (resourceName : string) (targetDir : string) =
        let targetPath = Path.Combine(targetDir, resourceFileName resourceName)
        use stream = assembly.GetManifestResourceStream(resourceName)
        if isNull stream then
            failwithf "Embedded resource '%s' not found" resourceName
        use fileStream = File.Create(targetPath)
        stream.CopyTo(fileStream)
        targetPath

    /// Directory the embedded default kernels are extracted to by loadDefaults.
    let defaultKernelDir = Path.Combine(Path.GetTempPath(), "PRo3D.SPICE.DefaultKernels")

    let loadDefaults () =
        Directory.CreateDirectory(defaultKernelDir) |> ignore
        for resourceName in embeddedKernels do
            extractResource resourceName defaultKernelDir |> ignore
        let previousDir = Environment.CurrentDirectory
        try
            Environment.CurrentDirectory <- defaultKernelDir
            for resourceName in embeddedKernels do
                CooTransformation.AddSpiceKernel(Path.Combine(defaultKernelDir, resourceFileName resourceName)) |> ignore
        finally
            Environment.CurrentDirectory <- previousDir
