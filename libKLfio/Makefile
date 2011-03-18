###########################################################
#
# Makefile for libKLfio.a
#
# The KLfio library contains file i/o routines
#
# Coding language: Fortran
# Compiler: g77
# Platform: RedHat Linux, Mac OS X
#
###########################################################

MAKE = make
GZIP = gzip -S .gz

#### Environment Variable Definitions ####
UNAME = `uname -s`
OS = redhat
PREFIX = $(HOME)/prgf
LIBDIR = $(PREFIX)/lib

EPREFIX = $(PREFIX)
LPREFIX = $(PREFIX)
SPREFIX = $(PREFIX)/lib/src

#### Compiler Variable Definitions ####
FC = g77
FFLAGS = -g -O2

################ DO NOT EDIT BELOW THIS LINE ##############

#### Variable Definitions ####
PKGNAME = KLfio
BASIC_FILES = Makefile Changes TODO test.dat testdef.dat
DOCS = KLfio.txt
VERSION = 0
RELEASE = $(VERSION).1.3

#### Object Definitions ####
OBJ_LIB = read_coo.o read_array.o write_array.o
OBJ_TEST = test.o
OBJECTS = $(OBJ_LIB) $(OBJ_TEST)
SRC = $(OBJECTS:.o=.f)

#### Rule Definitions ####
%.o : %.f
	$(FC) -c $(FFLAGS) $<

.PHONY: lib$(PKGNAME).a
main : lib$(PKGNAME).a

lib$(PKGNAME).a : $(OBJ_LIB)
	ar cr lib$(PKGNAME).a $(OBJ_LIB)
	ranlib lib$(PKGNAME).a

test : lib$(PKGNAME).a $(OBJ_TEST)
	$(FC) -o test $(OBJ_TEST) lib$(PKGNAME).a
	@\rm -f testout.dat
	./test
	@if (diff testout.dat testdef.dat); then \
		echo "Test successful."; \
		rm testout.dat; \
	else \
		echo "ERROR: Test failed."; \
		exit 1; \
	fi

all : main test

## Installing ##
.PHONY: install-lib install-src
install : install-lib
install-all : install-lib install-src

install-lib :
	install -d -m 0755 $(LPREFIX)/lib
	install -m 0644 lib$(PKGNAME).a $(LPREFIX)/lib

install-src :
	install -d -m 0755 $(SPREFIX)/$(PKGNAME)
	install -m 0644 $(BASIC_FILES) $(DOCS) $(SRC) $(SPREFIX)/$(PKGNAME)

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
