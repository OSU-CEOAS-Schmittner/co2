#
# Makefile for Ferret External Functions
#
# January 20 1998
# Jonathan Callahan
#
#  15-Nov-1999 Ansley Manke  
#  remove refs to ef_utility/*.o  (now in ferret executable)
#
#  ACM 2/2001  debug macros 
#  ACM 2/2002  change targets to all and extras; somehow standard
#               not working well w/ linux.
#
#  Jul 25, 2017 Andreas Schmittner
#  modified for co2 functions
#  
# include platform specific macro definitions
#
include ../ef_utility/site_specific.mk
#include ../ef_utility/platform_specific.mk.$(BUILDTYPE)
include ../ef_utility/platform_specific.mk
 
#
# Macros
#

.SUFFIXES: .so

#
# Rules
#

.F.so:
	$(F77) $(FFLAGS) -c  $<
	$(LD) $(LD_DYN_FLAGS) $(SYSLIBS) $*.o -o $*.so

#
# Targets
#

all:	pco2.so o2sat.so ph.so co2aq.so co3.so hco3.so omega_cal.so omega_ara.so

debug:
	$(MAKE) "FFLAGS = $(FFLAGS) -g -Ddebug" "CFLAGS = $(CFLAGS) -g -Ddebug"  all

install:
#	cp *.so $(FER_LOCAL_EXTFCNS)
	cp *.so $(MY_LOCAL_EXTFCNS)

clean:
	-rm -f *.o *.so

#
# End of Makefile
#
