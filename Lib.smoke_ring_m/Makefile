#-------------------------------------------------------------------
# class-hpc-smoke-ring: A simple sample field solver.
#
#    by Akira Kageyama, Kobe University, Japan.
#       email: sgks@mac.com
#
#    Copyright 2018 Akira Kageyama
#
#    This software is released under the MIT License.
#
#-------------------------------------------------------------------
#    src/Makefile
#-------------------------------------------------------------------

.PHONY: data clean
#.PHONY:smoke_ring clean

.SUFFIXES:
.SUFFIXES: .f90 .o

FC = ifort
FFLAGS = -O3 -qopenmp -march=core-avx2 


.f90.o:
	$(FC) $(FFLAGS) -c $<


OBJS = constants.o \
       ut.o \
       grid.o \
       job.o \
       params.o \
       debug.o \
       field.o \
       slicedata.o \
       solver.o \
       main.o \
       vis.o \

data: smoke_ring params.namelist
	./smoke_ring params.namelist 1

smoke_ring: $(OBJS)
	$(FC) $(FFLAGS) -o smoke_ring $(OBJS) 

vis.o: constants.o field.o ut.o solver.o 
grid.o: constants.o debug.o ut.o
ut.o: constants.o
params.o: constants.o ut.o
debug.o: constants.o ut.o params.o
field.o: constants.o grid.o
slicedata.o: ut.o field.o params.o debug.o solver.o
solver.o: constants.o grid.o ut.o field.o debug.o params.o job.o
job.o: constants.o ut.o
main.o: constants.o \
        ut.o \
        grid.o \
        job.o \
        params.o \
        debug.o \
        field.o \
        slicedata.o \
        solver.o \
        vis.o

clean:
	rm -f smoke_ring core ${OBJS} *.mod *.L
	rm -f _data_slice* *~
	rm -rf *.bmp