# InSituVis.mpi.CameraFocusMulti

This MPI application visualizes the SmokeRing simulation with multi-candidate
camera focus control.

## Placement

```sh
SmokeRingVis/App/InSituVis.mpi.CameraFocusMulti/
```

The binary name is `InSituVis.mpi.CameraFocusMulti` because `kvsmake -G` uses
the current directory name as the target name.

## Build

```sh
./make.sh
```

`make.sh` updates `../../Lib.smoke_ring_m.mpi/constants.F90` for the MPI process
layout and per-process grid size defined in the script, rebuilds the simulation
library when needed, and then runs `kvsmake -G -use_mpi` / `kvsmake`.

If a legacy standalone path-search source file is present in this directory,
`make.sh` excludes it from the application target.

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

The application writes image outputs and camera path selection data under
`Output`. The generated `Output/output_video_params.csv` can be post-processed
with `CameraOptimalPathSearch`:

```sh
../../Tools/CameraOptimalPathSearch/CameraOptimalPathSearch \
    -base-dir . \
    -candidate-num 5 \
    -first-file 0 \
    -entropy-ratio 1 \
    -focus-path-ratio 1 \
    -camera-path-ratio 1
```

The selected images and route images are copied to `ex_Output/Output`.

## Clean

```sh
./clean.sh
```

This stops a running application process when present, removes KVS build files,
removes `Output`, and removes the data directory specified in
`params.namelist`.

```sh
./cleanExoutput.sh
```

This removes the post-processed `ex_Output` directory when present.
