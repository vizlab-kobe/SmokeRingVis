# InSituVis.mpi.CameraPathTimeStep

This MPI application visualizes the SmokeRing simulation with camera path and
time-step control.

## Placement

```sh
SmokeRingVis/App/InSituVis.mpi.CameraPathTimeStep/
```

The binary name is `InSituVis.mpi.CameraPathTimeStep` because `kvsmake -G` uses
the current directory name as the target name.

## Build

```sh
./make.sh
```

`make.sh` updates `../../Lib.smoke_ring_m.mpi/constants.F90` for the MPI process
layout and per-process grid size defined in the script, rebuilds the simulation
library when needed, and then runs `kvsmake -G -use_mpi` / `kvsmake`.

## Run

```sh
./run.sh
```

`run.sh` reads `NPROC_X`, `NPROC_Y`, and `NPROC_Z` from `make.sh`, computes the
MPI process count, creates the data subdirectories from `params.namelist`, and
executes the application with `mpiexec`.

`MPIEXEC` and `MPI_OPTIONS` can be set before running the script. When Open MPI
is detected and `MPI_OPTIONS` is empty, `--oversubscribe` is added
automatically.

## Output

The application writes images and timing / entropy CSV files under `Output`.

## Clean

```sh
./clean.sh
```

This stops a running application process when present, removes KVS build files,
removes `Output`, and removes the data directory specified in
`params.namelist`.
