# Makefile for Exercise 5

FLAGS=-Wall -pedantic -O2 -fimplicit-none -std=f95

BASE=exercise5
PARTS=part1 part2

default all: $(PARTS)

part%:
	gfortran $(FLAGS) -o $@ $(BASE)_$@.f90

clean:
	rm -rf part*
