This folder contains default spice kernels to have a battery-included pro3d.spice lib which can be used without extra management of spice kernels.

- `pck00010.tpc` / `pck00010-base.tpc`: tweaked planetary constants kernels; the embedded kernel overrides spin to flip longitude.
- `naif0012.tls`: unmodified NAIF generic leapseconds kernel, from
  https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/naif0012.tls

All kernels originate from NASA's Navigation and Ancillary Information Facility (NAIF) and are subject to the terms in `SPICEKernels-LICENSE.txt`.