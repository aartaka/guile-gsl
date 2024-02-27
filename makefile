libguilegslblas.so: src/guilegslblas.c
	$(CC) $^ -fPIC --shared -o $@ -lgsl -lm -lgslcblas

all: libguilegslblas.so
