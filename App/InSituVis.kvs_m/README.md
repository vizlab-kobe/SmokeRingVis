# InSituVis.kvs_m

This is the serial SmokeRing in-situ visualization application using the
Fortran KVS module interface.

## Placement

```sh
SmokeRingVis/App/InSituVis.kvs_m/
```

The binary name is `InSituVis.kvs_m` because `kvsmake -G` uses the current
directory name as the target name.

## Build

```sh
./make.sh
```

`make.sh` generates `Makefile.kvs` when needed and then runs `kvsmake`.

## Run

```sh
./run.sh
```

`run.sh` executes:

```sh
./InSituVis.kvs_m params.namelist 1
```

On macOS, the script raises the stack size before execution.

## Clean

```sh
./clean.sh
```

This removes KVS build files and the local `Output` directory.
