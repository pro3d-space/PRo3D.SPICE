# Getting spice kernels for testing

Get spice kernels for example from here: [https://s2e2.cosmos.esa.int/bitbucket/scm/spice_kernels/hera.git](https://s2e2.cosmos.esa.int/bitbucket/scm/spice_kernels/hera.git).
The tests per default try to load kernels from this local path `./hera/kernels/mk/hera_crema_2_0_LPO_ECP_PDP.tm`.

```
git clone https://s2e2.cosmos.esa.int/bitbucket/scm/spice_kernels/hera.git
```


# Building & Running the tests

```
dotnet tool restore
dotnet paket restore
dotnet run --project src/PRo3D.SPICE.Tests/PRo3D.SPICE.Tests.fsproj
```
```
[17:16:20 INF] EXPECTO! 6 tests run in 00:00:00.2391825 for init - 6 passed, 0 ignored, 0 failed, 0 errored. Success! <Expecto>
```

