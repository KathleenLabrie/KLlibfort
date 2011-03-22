################################################################
#
# Makefile for libKLimgio
#
# Additional image I/0 routines. Include FITS image manipulation.
#
# Required libraries:
#	libCJP.a		Chris' library [Fortran, Imfort]
#	libKLutil.a		My utility library [C]
#	libcfitsio.a		Cfitsio library to manipulate FITS images [C]
#	libKLcfitsio.a		My front end to FITS image manipulation [C]
#
# Coding languages: Fortran, C
# Compilers: g77, gcc
# OS: RedHat Linux, Mac OS X
#
################################################################

MAKE = make
GZIP = gzip -S .gz

#### Environment Variable Definitions ####
UNAME = `uname -s`
OS = redhat
PREFIX = $(HOME)/prgf
LIBDIR = $(PREFIX)/lib

EPREFIX = $(PREFIX)
LPREFIX = $(EPREFIX)
SPREFIX = $(EPREFIX)/lib/src

C_PREFIX = $(HOME)/prgc
C_INCLDIR = $(C_PREFIX)/include
C_LIBDIR = $(C_PREFIX)/lib

#### Compiler Variable Definitions ####
FC = g77
FFLAGS = -g -O2
CC = gcc
CFLAGS = -g -O2 -I$(C_INCLDIR)

############### DO NOT EDIT BELOW THIS LINE ####################

#### Variable Definitions ####
PKGNAME = KLimgio
BASIC_FILES = Makefile Changes TODO
DOCS = doc.txt
VERSION = 0
RELEASE = $(VERSION).1.3

#### Object Definitions ####
#FOBJ_LIBS = fits_read_r4.o fits_write_r4.o
FOBJ_LIBS = fits_read_r4.o
COBJ_LIBS = cfitsread.o
FOBJECTS = $(FOBJ_LIBS)
COBJECTS = $(COBJ_LIBS)
FSRC = $(FOBJECTS:.o=.f)
CSRC = $(COBJECTS:.o=.c)
OBJECTS = $(FOBJECTS) $(COBJECTS)
SRC = $(FSRC) $(CSRC) fits_write_r4.f

#FLIBS = -L$(LIBDIR) -lCJP
#CLIBS = -L$(C_LIBDIR) -lKLutil -lKLfitsio -lcfitsio -lm

#### Rule Definitions ####
%.o : %.c
	$(CC) -c $(CFLAGS) $<
%.o : %.f
	$(FC) -c $(FFLAGS) $<

.PHONY: lib$(PKGNAME).a
main : lib$(PKGNAME).a

lib$(PKGNAME).a: $(OBJECTS)
	ar cr lib$(PKGNAME).a $(OBJECTS)
	ranlib lib$(PKGNAME).a

all : main

## Installing ##
.PHONY: install-lib install-src
install : install-lib
install-all : install-lib install-src

install-lib :
	install -d -m 0755 $(LPREFIX)/lib
	install -m 0644 lib$(PKGNAME).a $(LPREFIX)/lib
	ranlib $(LPREFIX)/lib/lib$(PKGNAME).a

install-src :
	install -d -m 0755 $(SPREFIX)/$(PKGNAME)
	install -m 0644 $(SRC) $(BASIC_FILES) $(DOCS) \
                $(SPREFIX)/$(PKGNAME)/

## Packaging ##
package-src :
	tar cvf lib$(PKGNAME)-$(RELEASE).tar $(BASIC_FILES) $(DOCS) $(SRC)
	$(GZIP) lib$(PKGNAME)-$(RELEASE).tar

.PHONY: clean
clean :
	\rm -f lib$(PKGNAME).a $(OBJECTS)

### End of Makefile ###
