### 1.0.10
* added `UnloadSpiceKernel` (native + F# binding), so kernels can actually be swapped instead of only ever accumulating
* fixed `GetPositionTransformationMatrix` leaving CSPICE's error state set on failure, which could silently poison the next unrelated SPICE call
* fixed `DeInit` to actually clear all loaded SPICE kernels (`kclear_c`) instead of only closing the log file
* fixed `Init`'s log file growing without bound across every process launch for the life of an install; now truncated once it exceeds ~20 MB
* fixed `getRotationTrafo` (F#) returning `Some Trafo3d.Identity` instead of `None` on failure, silently masking frame-transform failures as a bogus identity rotation

### 1.0.9
* added macOS arm64 (Apple Silicon) native binaries
* added cspice native library re-exporting the full CSPICE C API for direct P/Invoke

### 1.0.8
* updated aardpack tool

### 1.0.7
* added default spice kernels

### 1.0.6
* fixed osx build

### 1.0.5
* added osx build

### 1.0.4
* added getRotationTrafo

### 1.0.3
* updated aardvark packages

### 1.0.2 
* better credits  
  
### 0.0.2
* first port to separate project from https://github.com/pro3d-space/PRo3D/tree/1c8601d9fc88f81a03dae12965af1fb72fe61bcd/src/InstrumentPlatforms 