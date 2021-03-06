CC=g++
CFLAGS=-c
LIBS=
VPATH=./src
OBJECTS = getR2.o InOut.o Calculations.o Utils.o

all: main

main: $(OBJECTS)
	$(CC) $(OBJECTS) $(LIBS) -o getr2

%.o: %.c++
	$(CC) $(CFLAGS) $<

clean:
	rm -f *.o 

