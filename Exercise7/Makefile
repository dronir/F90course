# Makefile for Exercise 7

FLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95

BASE=exercise7
PARTS = part2

default all: $(PARTS)

part2:
	gfortran $(FLAGS) -o $@ $(BASE)_$@.f90 func.f90

part%:
	gfortran $(FLAGS) -o $@ $(BASE)_$@.f90

clean:
	rm -rf part*
