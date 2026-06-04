# InSituVis

This is the serial in-situ visualization application for the SmokeRing
simulation.

## Placement

```sh
SmokeRingVis/App/InSituVis/
```

The binary name is `InSituVis` because `kvsmake -G` uses the current directory
name as the target name.

## Build

```sh
./make.sh
```

`make.sh` updates `../../Lib.smoke_ring_m/constants.f90` for the grid size
defined in the script, rebuilds the simulation library when needed, and then
runs `kvsmake`.

## Run

```sh
./run.sh
```

`run.sh` executes:

```sh
./InSituVis params.namelist 1
```

On macOS, the script raises the stack size before execution.

## Clean

```sh
./clean.sh
```

This removes KVS build files and the local `Output` directory.
