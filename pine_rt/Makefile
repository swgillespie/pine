INSTALL_PATH?=/usr/local

CC := clang

INCLUDEDIR := .
CFLAGS := -std=c99 -g -Wall -Werror -Wextra -pedantic -Wshadow -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -I$(INCLUDEDIR) -g -fpic
LDFLAGS := -lgc
LIBNAME := libpinert.dylib

PINE_RT_INSTALL_PATH=$(INSTALL_PATH)/lib

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

install: $(LIBNAME)
	mkdir -p $(PINE_RT_INSTALL_PATH)
	cp $(LIBNAME) $(PINE_RT_INSTALL_PATH)
