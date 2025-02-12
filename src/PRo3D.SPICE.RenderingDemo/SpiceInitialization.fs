namespace PRo3D.SPICE

open System
open System.IO

open Aardvark.Base
open PRo3D.Extensions

module SPICE =

    
    let initializeAndLoadKernels (spiceKernelFileName : string) =

        let init = 
            let r = CooTransformation.Init(false, Path.Combine(".", "logs", "CooTrafo.Log"), 0, 0)
            if r <> 0 then failwith "could not initialize CooTransformation lib."
            { new IDisposable with member x.Dispose() = CooTransformation.DeInit() }

    
        let spiceFileName = 
            if spiceKernelFileName.IsNullOrEmpty() then
                let spiceRoot = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "spice_kernels")
                let marsFlyBy = Path.Combine(spiceRoot, @"kernels/mk/hera_crema_2_0_LPO_ECP_PDP.tm")
                marsFlyBy
            else
                spiceKernelFileName

        if File.Exists spiceFileName then Log.line "using: %s" spiceFileName
        else
            Log.error "spice kernel: %s does not exist" spiceFileName
            failwith "could not load spice kernels."

        System.Environment.CurrentDirectory <- Path.GetDirectoryName(spiceFileName)
        let r = CooTransformation.AddSpiceKernel(spiceFileName)
        if r <> 0 then failwith "could not add spice kernel"

        init