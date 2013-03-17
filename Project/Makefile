# Makefile for course project

# Set up paths and test program's executable name
OBJDIR=objects
SOURCEDIR=src
EXECUTABLES=tests.x

# Set up compiler and compilation flags
FC=gfortran
FFLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95 -J $(OBJDIR)

# List the dependencies of distributions.o
DEPS=utility.o normal.o uniform.o chisq.o

# Add the proper directory path to the dependencies
DDEPS = $(addprefix $(OBJDIR)/,$(DEPS))

# By default, we build everything
default: all

# "Everything" means all the executables given
.phony all: $(EXECUTABLES)

# Rule to build an executable, depending on distributions.o
%.x: $(SOURCEDIR)/%.f90 $(OBJDIR)/distributions.o $(DDEPS)
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
.phony clean:
	rm -rf $(OBJDIR)
	rm -rf $(EXECUTABLES)
