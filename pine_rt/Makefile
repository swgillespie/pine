CC := clang

INCLUDEDIR := .
CFLAGS := -std=c99 -O2 -Wall -Werror -Wextra -pedantic -Wshadow -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -I$(INCLUDEDIR) -g -fpic
LDFLAGS := -lgc
LIBNAME := libpinert.so


SOURCES := $(shell find . -name "*.c")
OBJECTS := $(SOURCES:.c=.o)

all: $(LIBNAME)

$(LIBNAME): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -shared -o $(LIBNAME)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(LIBNAME)
	rm -f $(OBJECTS)
	rm -f $(shell find . -name *~)
