# Makefile for Exercise 4

FLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95

BASE=exercise4
PARTS = part1 part2 part3 part4

default all: $(PARTS)

part%:
	gfortran $(FLAGS) -o $@ $(BASE)_$@.f90

clean:
	rm -rf part*
