# @(#)$Header$ 
#
# Include file for making modules and doing needed copies, etc.
#
ETMOD_OUT = $(ETDSRC)/$(SMOKE_EXE)

MODGRID  = $(ETMOD_OUT)/modgrid.o

#
IOMODALL   = $(MODGRID)

# Module dependencies...  

$(MODGRID): $(ETDSRC)/modgrid.f $(ETINC)/IOPRVT3.EXT $(ETINC)/IOSTRG3.EXT
	$(FC) $(FFLAGS) -c -o $@ $<
	if ( test -f modgrid.mod ) ; then mv modgrid.mod $(ETMOD_OUT) ; fi
	if ( test -f modgrid.M   ) ; then mv modgrid.M   $(ETMOD_OUT) ; fi
	if ( test -f MODGRID.mod ) ; then mv MODGRID.mod $(ETMOD_OUT) ; fi
