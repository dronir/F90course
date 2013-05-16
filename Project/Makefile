# Makefile for course project

# Set up paths and test program's executable name
OBJDIR=objects
SOURCEDIR=src
EXECUTABLES=tests.x

# Set up report compiling stuff. This allows one to either
# use pdflatex to make a pdf, or latex to make a dvi.
REPORT=report.pdf
TEX=pdflatex
TEXDIR=tex
TEXFLAGS=-output-directory $(TEXDIR)/output -halt-on-error

# Set up compiler and compilation flags
FC=gfortran
FFLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95 -J $(OBJDIR)

# List the dependencies of distributions.o
DEPS=utility.o normal.o uniform.o chisq.o

# Add the proper directory path to the dependencies
DDEPS = $(addprefix $(OBJDIR)/, $(DEPS))

# By default, we build just the code, not the report
default: code

# "All" means all the executables and the report
.phony all: code report

# "Code" means just the executables, not the report
.phony code: $(EXECUTABLES)

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

# Rule to build the report
$(REPORT): $(TEXDIR)/report.tex | $(TEXDIR)/output
	$(TEX) $(TEXFLAGS) $^ >/dev/null
	mv $(TEXDIR)/output/$(REPORT) .

# Make tex output directory if it doesn't exist
$(TEXDIR)/output:
	mkdir $(TEXDIR)/output

# Phony target to build the report
.phony report: $(REPORT)

# Rule to clean up everything
.phony clean:
	rm -rf $(OBJDIR)
	rm -rf $(EXECUTABLES)
	rm -rf $(REPORT)
	rm -rf $(TEXDIR)/output
