##########################################################
#
# Makefile for libNR
#
# Some of the Numerical Recipes routines.
#
# Required libraries: none
#
# Coding languages: Fortran
# Compilers: g77
# OS: RedHat Linux, Mac OS X
#
##########################################################

MAKE = make
GZIP = gzip -S .gz

#### Environment Variable Definitions ####
OS = macosx
PREFIX = /Users/klabrie/prgf
LIBDIR = $(PREFIX)/lib

EPREFIX = $(PREFIX)
LPREFIX = $(PREFIX)
SPREFIX = $(PREFIX)/lib/src

#### Compiler Variable Definitions ####
FC = g77
FFLAGS = -g -O2

################ DO NOT EDIT BELOW THIS LINE #############

#### Variable Definitions ####
PKGNAME = NR
BASIC_FILES = Makefile Changes TODO
DOCS = doc.txt
VERSION = 0
RELEASE = $(VERSION).1.2

#### Object Definitions ####
OBJ_LIB = covsrt.o gaussj.o mrqcof.o mrqmin.o
OBJECTS = $(OBJ_LIB)
SRC = $(OBJECTS:.o=.f)

#### Rule Definitions ####
%.o : %.f
	$(FC) -c $(FFLAGS) $<

.PHONY: lib$(PKGNAME).a
main : lib$(PKGNAME).a

lib$(PKGNAME).a : $(OBJ_LIB)
	ar cr lib$(PKGNAME).a $(OBJ_LIB)
	ranlib lib$(PKGNAME).a

all : main

## Installing ##
.PHONY: install-lib install-src
install : install-lib
install-all : install-lib install-src

install-lib :
	install -d -m 0755 $(LPREFIX)/lib
	install -m 0644 lib$(PKGNAME).a $(LPREFIX)/lib/

install-src :
	install -d -m 0755 $(SPREFIX)/$(PKGNAME)
	install -m 0644 $(BASIC_FILES) $(DOCS) $(SRC) $(SPREFIX)/$(PKGNAME)/

## Packaging ##
.PHONY: package-src
package-src :
	tar cvf lib$(PKGNAME)-$(RELEASE).tar $(BASIC_FILES) $(DOCS) $(SRC)
	$(GZIP) lib$(PKGNAME)-$(RELEASE).tar

## Cleaning ##
.PHONY: clean
clean :
	\rm -f lib$(PKGNAME).a $(OBJECTS)

### End of Makefile ###
