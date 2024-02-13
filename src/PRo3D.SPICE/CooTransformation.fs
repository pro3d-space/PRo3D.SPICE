namespace PRo3D.Extensions

open System
open System.Runtime.InteropServices


[<AutoOpen>]
module CooTransformation =


    [<Literal>]
    let cooTransformationLib = @"CooTransformation.dll"

    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern int Init(bool bConsoleLog, string pcLogFile, int nConsoleLogLevel, int nFileLogLevel)

    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern void DeInit();

    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern uint32 GetAPIVersion();

    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern int AddSpiceKernel(string pSpiceKernelFileName)

    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern int Xyz2LatLonRad(double dX, double dY, double dZ, double* pdLat, double* pdLon, double* pdRad)

 
    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern int Xyz2LatLonAlt(string pcPlanet, double dX, double dY, double dZ, double& pdLat, double& pdLon, double& pdAlt)
    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern int LatLonAlt2Xyz(string pcPlanet, double pLat, double pLon, double pAlt, double& pdX, double& pdY, double& pdZ)

    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern int GetRelState(string pcTargetBody,
                           string pcSupportBody,
                           string pcObserverBody,
                           string pcObserverDatetime,
                           string pcOutputReferenceFrame,
                           IntPtr pdPosVec,
                           IntPtr pdRotMat)

    [<DllImport(cooTransformationLib, CallingConvention = CallingConvention.Cdecl)>]
    extern int GetPositionTransformationMatrix(string pcFrom, string pcTo, string pcDatetime, double* pdRotMat);
