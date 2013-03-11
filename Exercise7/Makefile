# Makefile for Exercise 7

FLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95

BASE=exercise7
PARTS = part2 part3
DEPS=func.f90

default all: $(PARTS)

part%:
	gfortran $(FLAGS) -o $@ $(BASE)_$@.f90 $(DEPS)

clean:
	rm -rf part*
