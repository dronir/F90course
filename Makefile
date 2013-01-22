EXERCISES=Exercise1 Exercise2

default all:
	@ for i in $(EXERCISES); do \
		echo "Building" $$i; \
		make -C $$i; \
	done

clean:
	@ for i in $(EXERCISES); do \
		echo "Cleaning in" $$i; \
		make -C $$i clean; \
	done

