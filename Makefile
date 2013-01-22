EXERCISES=Exercise1 Exercise2

all:
	make -C $(EXERCISES)

clean:
	rm $(EXERCISES)/part*
