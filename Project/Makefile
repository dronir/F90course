# Makefile for course project

FLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95

BASE=exercise8
DEPS=

default all: project

project:
	gfortran $(FLAGS) -o project.x project.f90 $(DEPS)

clean:
	rm -rf project.x
	rm -rf *.o
