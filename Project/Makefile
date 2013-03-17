# Makefile for course project

# Where to put object files
OBJDIR=objects
SOURCEDIR=src
DEPS=distributions.o
EXENAME=tests.x

FC=gfortran
FFLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95 -J $(OBJDIR)

DDEPS = $(addprefix $(OBJDIR)/,$(DEPS))

default all: tests

tests: $(DDEPS)
	$(FC) $(FFLAGS) -o $(EXENAME) $(SOURCEDIR)/tests.f90 $(DDEPS)

$(OBJDIR)/%.o : $(SOURCEDIR)/%.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -c $< -o $@

$(OBJDIR):
	mkdir $(OBJDIR)

clean:
	rm -rf $(EXENAME)
	rm -rf $(OBJDIR)
