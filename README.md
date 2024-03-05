# SPICE for PRo3D and dotnet

This repository provides a library to work with [SPICE](https://naif.jpl.nasa.gov/naif/toolkit.html) platform-indepentently in dotnet by wrapping the native [PRo3D-Extensions](https://github.com/DaKup/PRo3D-Extensions) library written in C++. Moreover, this repository contains:
 * the library accessible as a nuget package,
 * as well as a sample applications which allows to experiment with SPICE and interactive visualizations.

![](./docs/mars-flyby.gif)

[PRo3D](https://pro3d.space/) uses the library in a more sophisticated way for space-missions. However, just like PRo3D, the visualization component is also written using [the aardvark-platform](https://github.com/aardvark-platform) and shows how to deal with large coordinates and different coordinate frames in graphics applications.


# Getting spice kernels for testing

Get spice kernels for example from here: [https://s2e2.cosmos.esa.int/bitbucket/scm/spice_kernels/hera.git](https://s2e2.cosmos.esa.int/bitbucket/scm/spice_kernels/hera.git).
The tests per default try to load kernels from this local path `./spice_kernels/hera/kernels/mk/hera_crema_2_0_LPO_ECP_PDP.tm`.

```
mkdir spice_kernels
git clone https://s2e2.cosmos.esa.int/bitbucket/scm/spice_kernels/hera.git spice_kernels
```

or use pull the submodule.


# Building & Running the tests

```
dotnet tool restore
dotnet paket restore
./build.sh or .\build.cmd
dotnet run --project src/PRo3D.SPICE.Tests/PRo3D.SPICE.Tests.fsproj
```
```
[17:16:20 INF] EXPECTO! 6 tests run in 00:00:00.2391825 for init - 6 passed, 0 ignored, 0 failed, 0 errored. Success! <Expecto>
```

# Running the simple rendering demo

```
dotnet run --project src/PRo3D.SPICE.RenderingDemo/PRo3D.SPICE.RenderingDemo.fsproj
```

## Command line flags

The project `PRo3D.SPICe.RenderingDemo" supports several command line utilities.
```
cd ./bin/Debug/net6.0/
dotnet PRo3D.SPICE.RenderingDemo.dll --time 2025-03-10 19:08:12.60 --timefactor 17856.0 --speed 100000000.0 --observer mars --referenceframe ECLIPJ2000
```

![](docs/mars-flyby.gif)

Another command-line setup could be:

```
dotnet PRo3D.SPICE.RenderingDemo.dll --time 2025-03-10 19:08:12.60 --timefactor 37856.0 --speed 100000000.0 --observer phobos --referenceframe ECLIPJ2000 --spiceKernel "../../../spice_kernels/kernels/mk/hera_crema_2_0_LPO_ECP_PDP.tm"
```
Note that a custom path to a spice kernel was supplied.

By setting observer to `HERA` one can investigate the fly-by and animate the camera:
![Alt text](./docs/hera_observer.gif)

# Integration in PRo3D

By using PRo3D's rendering real surface data can be rendered together with the simple body representation used in the example:

![](./docs/mars_observer_mola.gif)

or impressive fly-by scenarios:

![](./docs/flyby-opc.gif)


