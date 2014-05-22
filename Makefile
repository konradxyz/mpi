#
# A template makefile for the MPI assignment.
# Concurrent and Distributed Programming Course, spring 2014.
# Faculty of Mathematics, Informatics and Mechanics.
# University of Warsaw, Warsaw, Poland.
#
# Copyright (C) Konrad Iwanicki, 2014.
#

MPICC = mpicc
MATGEN_TYPE ?= matgen-mt
MATGEN_FILE = $(MATGEN_TYPE).o 

all: msp-seq-naive.exe msp-par.exe


%.exe: %.o $(MATGEN_FILE)
	$(MPICC) -O3 -o $@ $^

%.o: %.c matgen.h Makefile
	$(MPICC) -O3 -c -o $@ $<

clean:
	rm -f *.o *core *~ *.out *.err *.exe

