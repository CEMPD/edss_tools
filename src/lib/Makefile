# Version $Id$ 
# Path    $Source$ 
# Date    $Date$

# Makefile for I/O API Tools library

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<
.F.o:
	$(FC) $(FFLAGS) -c -o $@ $<

include $(ETDSRC)/modmake.inc

FILES = \
  checkmem.o   chkgrid.o    chkint.o     chkreal.o    dscm3grd.o   dscm3lay.o  \
  fltrneg.o    getcfdsc.o   getfline.o   getifdsc.o   getm3epi.o   getnlist.o  \
  hdrmiss3.o   ioapi_grd_size.o          multunit.o   openphys.o   padnzero.o  \
  padzero.o    parsline.o   polmesg.o    rd3mask.o    rdsetmask.o  unitfac.o   \
  unitmatch.o  flterr.o

all: lib

lib: $(IOMODALL) $(FILES)
	ar rv $(TOOLS_BIN)/libedsstools.a $(FILES)
	ar rv $(TOOLS_BIN)/libedsstools.a $(IOMODALL)

debug: $(IOMODALL) $(FILES)
	ar rv $(TOOLS_BIN)/libedsstools.debug.a $(FILES) 
	ar rv $(TOOLS_BIN)/libedsstools.debug.a $(IOMODALL)
#
# Module and include dependencies
#

chkgrid.o:   $(MODGRID) $(ETINC)/IOCNST3.EXT $(IOINC)/PARMS3.EXT $(IOINC)/FDESC3.EXT
dscm3grd.o:  $(ETINC)/IOCNST3.EXT $(ETINC)/IOSTRG3.EXT $(ETINC)/IOPRVT3.EXT $(IOINC)/PARMS3.EXT
dscm3lay.o:  $(ETINC)/IOCNST3.EXT $(IOINC)/PARMS3.EXT
getcfdsc.o:  $(ETINC)/IOCNST3.EXT $(IOINC)/PARMS3.EXT
getifdsc.o:  $(IOINC)/PARMS3.EXT
getm3epi.o:  $(ETINC)/IOCNST3.EXT 
hdrmiss3.o:  $(IOINC)/FDESC3.EXT $(IOINC)/PARMS3.EXT
multunit.o:  $(ETINC)/IOSTRG3.EXT $(ETINC)/IOPRVT3.EXT
openphys.o:  $(ETINC)/IOCNST3.EXT 
polmesg.o:   $(ETINC)/IOCNST3.EXT 
rd3mask.o:   $(ETINC)/IOSTRG3.EXT $(ETINC)/IOPRVT3.EXT $(IOINC)/IODECL3.EXT
rdsetmask.o: $(ETINC)/IOSTRG3.EXT $(ETINC)/IOPRVT3.EXT
unitfac.o:   $(ETINC)/IOCNST3.EXT $(ETINC)/IOSTRG3.EXT $(ETINC)/IOPRVT3.EXT
unitmatch.o: $(ETINC)/IOSTRG3.EXT $(ETINC)/IOPRVT3.EXT

clean: 
	/bin/rm -f *.o $(ETMOD_OUT)/*.o $(ETMOD_OUT)/*.mod $(ETMOD_OUT)/*.M
