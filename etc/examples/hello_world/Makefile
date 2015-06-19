PINEC := pinec
CC := clang

LDFLAGS := -lpinert
PROGNAME := hello

SOURCES := $(shell find . -name "*.pine")
OBJECTS := $(SOURCES:.pine=.o)

all: $(PROGNAME)

$(PROGNAME): $(OBJECTS)
	$(CC) $(LDFLAGS) -o $(PROGNAME) $(OBJECTS)

%.o: %.pine
	$(PINEC) -o $@ $<

clean:
	rm -f $(PROGNAME)
	rm -f $(OBJECTS)
