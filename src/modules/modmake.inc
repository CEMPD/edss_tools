# @(#)$Header$ 
#
# Include file for making modules and doing needed copies, etc.
#
IOMOD_OUT = $(IOMOD_SRC)/$(EDSS_EXE)

MODGRID  = $(IOMOD_OUT)/modgrid.o

#
IOMODALL   = $(MODGRID)
# 
# Module dependencies...  

$(MODGRID): $(IOMOD_SRC)/modgrid.f $(IOINC)/IOPRVT3.EXT $(IOINC)/IOSTRG3.EXT
	$(FC) $(FFLAGS) -c -o $@ $(IOMOD_SRC)/modgrid.f
	if ( test -f modgrid.mod ) ; then mv modgrid.mod $(IOMOD_OUT) ; fi
	if ( test -f modgrid.M   ) ; then mv modgrid.M   $(IOMOD_OUT) ; fi
	if ( test -f MODGRID.mod ) ; then mv MODGRID.mod $(IOMOD_OUT) ; fi
