# Version $Id$ 
# Path    $Source$ 
# Date    $Date$

# Makefile for I/O API Tools library

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<

include $(ETDSRC)/modmake.inc

FILES_A = chkgrid.o    dscm3grd.o dscm3lay.o \
          chkreal.o    getfline.o   hdrmiss3.o   parsline.o \
          checkmem.o   dscm3grd.o   getifdsc.o   lambert.o    polmesg.o  \
          chkgrid.o    fltrneg.o    getm3epi.o   multunit.o   rd3mask.o  \
          chkint.o     getcfdsc.o   getnlist.o   padzero.o    unitfac.o  \
          unitmatch.o

INCLUDES = $(ETINC)/IOCNST3.EXT $(ETINC)/IOSTRG3.EXT

# NOTE: A some point, insert dependencies on include files

all: lib

lib: $(IOMODALL) $(FILES_A)
	ar rv $(TOOLS_BIN)/libedsstools.a $(FILES_A)
	ar rv $(TOOLS_BIN)/libedsstools.a $(IOMODALL)

debug: $(IOMODALL) $(FILES_A)
	ar rv $(TOOLS_BIN)/libedsstools.debug.a $(FILES_A) 
	ar rv $(TOOLS_BIN)/libedsstools.debug.a $(IOMODALL)
#
# Module dependencies
#

chkgrid.o: $(MODGRID) $(ETINC)/FLTERR.EXT
	$(FC) $(FFLAGS) -c -o $@ chkgrid.f


clean:
	/bin/rm *.o
