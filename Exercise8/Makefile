# Makefile for Exercise 78

FLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95

BASE=exercise8
PARTS = part1

default all: $(PARTS)

part%:
	gfortran $(FLAGS) -o $@ $(BASE)_$@.f90

clean:
	rm -rf part*
