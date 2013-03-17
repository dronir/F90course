# Makefile for course project

# Where to put object files
OBJDIR=objects
SOURCEDIR=src
DEPS=distributions.o

FC=gfortran
FFLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95 -J $(OBJDIR)

DDEPS = $(addprefix $(OBJDIR)/,$(DEPS))

default all: tests

objectdir:
	mkdir $(OBJDIR)

tests: $(DDEPS)
	$(FC) $(FFLAGS) -o tests.x $(SOURCEDIR)/tests.f90 $(DDEPS)

$(OBJDIR)/%.o : $(SOURCEDIR)/%.f90 | objectdir
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf project.x
	rm -rf $(OBJDIR)
