EXERCISES=Exercise1

all:
	make -C $(EXERCISES)

clean:
	rm $(EXERCISES)/part*
