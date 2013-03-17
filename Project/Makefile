# Makefile for course project

# Set up paths and test program's executable name
OBJDIR=objects
SOURCEDIR=src
EXECUTABLES=tests.x

# Set up compiler and compilation flags
FC=gfortran
FFLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95 -J $(OBJDIR)

# List the dependencies of distributions.o
DEPS=utility.o

# Add directory path to dependencies
DDEPS = $(addprefix $(OBJDIR)/,$(DEPS))

# By default, we build all given executables
default all: $(EXECUTABLES)

# Rule to build an executable, depending on distributions.o
%.x: $(SOURCEDIR)/%.f90 $(OBJDIR)/distributions.o
	$(FC) $(FFLAGS) -o $@ $^

# Rule to compile distributions.o, depending on the other deps
$(OBJDIR)/distributions.o: $(DDEPS)
	$(FC) $(FFLAGS) -c $(SOURCEDIR)/distributions.f90 -o $@

# Rule to compile all deps, making the 'objects' directory if necessary
$(OBJDIR)/%.o : $(SOURCEDIR)/%.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Rule to make the objects directory
$(OBJDIR):
	mkdir $(OBJDIR)

# Rule to clean up everything
.PHONY clean:
	rm -rf $(EXENAME)
	rm -rf $(OBJDIR)
