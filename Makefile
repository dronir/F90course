EXERCISES=Exercise1 Exercise2

default all:
	@ for i in $(EXERCISES); do \
		echo $$i; \
		make -C $$i; \
	done

clean:
	@ for i in $(EXERCISES); do \
		echo $$i; \
		make -C $$i clean; \
	done

