# SmokeRingVis
In-situ visualization for a simple CFD simulation ([smoke-ring](https://github.com/akageyama/smoke-ring)).

## Installation

### Pre-requisities

The follwoing packages needs to be installed before compiling SmokeRingVis.
- [KVS](https://github.com/naohisas/KVS)
- [InSituVis](https://github.com/vizlab-kobe/InSituVis)

#### KVS
KVS supports OSMesa and MPI needs to be installed.

1. Install OSMesa and MPI

    OSMesa and MPI need to be install before compile the KVS. Please refer to the followin URLs to install them.<br>
    - [OSMesa](https://github.com/naohisas/KVS/blob/develop/Source/SupportOSMesa/README.md)
    - [MPI](https://github.com/naohisas/KVS/blob/develop/Source/SupportMPI/README.md)

2. Get KVS source codes from the GitHub repository as follows:
    ```
    $ git clone https://github.com/naohisas/KVS.git
    ```

3. Modify kvs.conf as follows:
    ```
    $ cd KVS
    $ <modify kvs.conf>
    ```

    - Change the following items from 0(disable) to 1(enable).<br>
    ```
    KVS_ENABLE_OPENGL     = 1
    KVS_SUPPORT_MPI       = 1
    KVS_SUPPORT_OSMESA    = 1
    ```
    - Change the following items from 1(enable) to 0(disable).<br>
    ```
    KVS_SUPPORT_GLU       = 0
    KVS_SUPPORT_GLUT      = 0
    ```
    - Change the following items to 1 if needed. <br>
    ```
    KVS_ENABLE_OPENMP     = 1
    ```

4. Compile and install the KVS
    ```
    $ make
    $ make install
    ```

#### InSituVis
InSituVis, which is a in-situ visualization framework based on KVS, is required.

1. Get the InSituVis source codes from the GitHub repository as follows:
    ```
    $ git clone https://github.com/vizlab-kobe/InSituVis.git
    ```

    Note: Since the InSituVis is a header-only library, there is no need to compile the downloaded libary.

## Build and Execution

1. Clone the SmokeRingVis repository from GitHub as follows:
```
$ git clone --recursive https://github.com/vizlab-kobe/SmokeRingVis.git
```

2. Compile the fortran module library.
```
$ cd Lib.smoke_ring_m.mpi
$ python3 kvsmake.py
```
- The following variables in the kvsmake.conf file can be used to specify the compiler to the environment.
  ```
  KVS_MPI_FC  := mpif90
  KVS_MPI_CC  := mpicc
  KVS_MPI_CPP := mpic++
  KVS_MPI_LD  := mpif90
  ```

3. Compile and execute an application program.
```
$ cd App/InSituVis.mpi
$ ./make.sh
$ ./run.sh
```
- The following variables in the make.sh file can be used to specify the number of processes along each axis of computational space and the number of grid points for each axis in each process. 
  ```
  NPROC_X=3
  NPROC_Y=2
  NPROC_Z=2
  
  NXPP=30
  NYPP=20
  NZPP=20
  ```
  In this case, 3 processes are specified in the x-axis direction, 2 in the y-axis direction, and 2 in the z-axis direction, and the computational space is divided into 3x2x2 (12) subregions. Each subregion (subvolume) is composed of ```(NXPP+2) x (NYPP+2) x (NZPP+2) = 32 x 22 x 22``` grid points added overlap grids between adjacent subvolume. In this case, the grid size of whole volume data is ```(NPROC_X*NXPP) x (NPROC_Y*NYPP) x (NPROC_Z*NZPP) = (30x3) x (20x2) x (20x2) = 90 x 40 x 40``` grid points.
- The rendered images are output in the directory named ```Output``` under the current directory.
- The simulation parameters can be changed by the following vairables in the params.namelist file.
  ```
  &data03      Nloops_this_job = 10000     /
  &data04  Viscous_diffusivity = 3.0e-2    /
  &data05  Thermal_diffusivity = 3.0e-2    /
  ```
- The visualization method can be changed by modifying the following parameter in the main.f90 file.
  ```fortran
  80:  insitu_vis = InSituVis( Isosurface ) ! OrthoSlice, Isosurface, or VolumeRendering
  ```
- Several visualization parameters can be specified by modifying the following parameter in the InSituVis.cpp file.
  ```cpp
  13: // Parameters
  14: namespace Params
  15: {
  16: const auto ImageSize = kvs::Vec2ui{ 512, 512 }; // width x height
  17: const auto AnalysisInterval = 100; // analysis (visuaization) time interval
  18: const auto ViewPos = kvs::Vec3{ 7, 5, 6 }; // viewpoint position
  19: const auto ViewDir = InSituVis::Viewpoint::Direction::Uni; // Uni or Omni
  20: const auto Viewpoint = InSituVis::Viewpoint{ { ViewDir, ViewPos } }; // viewpoint
  21: } // end of namespace Params
  ```

4. Clean the compiled binary files and the output directory.
```
$ ./clean.sh
```
- Note: This command will forcely delete the output directory ("Output") in which the rendered images are stored. If the rendering images need to be saved, move this directory to another location or rename it before executing this command.
