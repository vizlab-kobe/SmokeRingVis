#-------------------------------------------------------------------
# class-hpc-smoke-ring: A simple sample field solver.
#
#    by Akira Kageyama, Kobe University, Japan.
#       email: sgks@mac.com
#
#    This software is released under the MIT License.
#
#-------------------------------------------------------------------
#    src/Makefile
#-------------------------------------------------------------------

.PHONY: data clean

.SUFFIXES:
.SUFFIXES: .f90 .o

# FC = gfortran
# FC = ifort
FC = mpif90
#FFLAGS = -xAVX -qopenmp -qopt-report -qopt-report-phase=openmp \
	-qopt-report-file=stdout
FFLAGS = -xAVX -qopenmp

# LINK = -lmpi

.f90.o:
	$(FC) $(FFLAGS) -c $< $(LINK)


OBJS = constants.o \
       ut.o \
       rank.o \
       grid.o \
       job.o \
       params.o \
       debug.o \
       field.o \
       slicedata.o \
       solver.o \
       main.o


smoke_ring: $(OBJS)
	$(FC) $(FFLAGS) -o smoke_ring $(OBJS) $(LINK)
	cp smoke_ring ../job/smoke_ring

run: smoke_ring
	chmod 755 ./hybrid.sh
	./hybrid.sh

grid.o: constants.o debug.o ut.o rank.o
ut.o: constants.o
rank.o : constants.o
params.o: constants.o ut.o
debug.o: constants.o ut.o params.o
field.o: constants.o grid.o rank.o
slicedata.o: ut.o field.o params.o debug.o solver.o grid.o
solver.o: constants.o grid.o ut.o field.o debug.o params.o job.o rank.o
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
        rank.o  

clean:
	rm -f core ${OBJS} *.mod *.L *~
	rm -f _data_slice
	rm -f smoke_ring
