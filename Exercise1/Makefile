# Makefile for Exercise 1

FLAGS=-Wall

BASE=exercise1
PARTS = part1 part2 part3 part4

default: $(PARTS)

part%:
	gfortran $(FLAGS) -o $@ $(BASE)_$@.f90

clean:
	rm -v part*
