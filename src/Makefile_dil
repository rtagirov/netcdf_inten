#FC=ifort  
#FC=mpiifort
FC=mpif90
PROG=getinten 

FFLAGS= -c #-traceback -c -heap-arrays -check bounds  
FPPFLAGS=-DMPI 
NFDIR=../../libraries/lib_netcdf/
# /cm/shared/apps/netcdf/intel/4.6.1/

INCLUDENET=-I${NFDIR}include
NETCDFLIB=-L../netcdf -lnet -L${NFDIR}lib -lnetcdf 

####################################################################

OBJECTS=arrays.o

all: getinten 

getinten: $(OBJECTS)  main.o 
	 $(FC) $(LFLAGS) $(FPPFLAGS) -o getinten  main.o arrays.o  $(INCLUDENET) $(NETCDFLIB) 
	 mv getinten ../bin 
arrays.o : arrays.F90
	$(FC) $(FFLAGS) $(FPPFLAGS) arrays.F90 

main.o : $(OBJECTS)  main.F90
	$(FC) $(FFLAGS) $(FPPFLAGS) main.F90 	


# ========================================================================
clean:
	rm -f *~ *.o *.mod main~  
