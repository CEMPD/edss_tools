# Version %W% 
# Path    %P%
# Date    %G% %U%

# Makefile for I/O API Tools library

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<

include $(IOMOD_SRC)/modmake.inc

FILES_A = chkgrid.o  \
          check3.o     chkreal.o    getfline.o   hdrmiss3.o   parsline.o \
          checkmem.o   dscm3grd.o   getifdsc.o   lambert.o    polmesg.o  \
          chkgrid.o    fltrneg.o    getm3epi.o   multunit.o   rd3mask.o  \
          chkint.o     getcfdsc.o   getnlist.o   padzero.o    unitfac.o  \
          unitmatch.o

INCLUDES = $(IOINC)/IOCNST3.EXT $(IOINC)/IOSTRG3.EXT

# NOTE: A some point, insert dependencies on include files

all: lib

lib: $(IOMODALL) $(FILES_A)
	ar rv $(TOOLS_BIN)/libiotools.a $(FILES_A)
	ar rv $(TOOLS_BIN)/libiotools.a $(IOMODALL)

debug: $(IOMODALL) $(FILES_A)
	ar rv $(TOOLS_BIN)/libiotools.debug.a $(FILES_A) 
	ar rv $(TOOLS_BIN)/libiotools.debug.a $(IOMODALL)
#
# Module dependencies
#

chkgrid.o: $(MODGRID) $(IOINC)/FLTERR.EXT
	$(FC) $(FFLAGS) -c -o $@ $<


clean:
	/bin/rm *.o
