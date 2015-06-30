INSTALL_PATH?=/usr/local

CC := clang
RUNTIME_INCLUDE_DIR := pine_rt
CFLAGS := -fpic -std=c99 -Wall -Werror -Wextra -pedantic -Wshadow    \
          -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes   \
					-I$(RUNTIME_INCLUDE_DIR)
LDFLAGS := -lgc

PINEC := pinec

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	RUNTIME_NAME:=libpinert.dylib
endif
ifeq ($(UNAME_S),Linux)
	RUNTIME_NAME:=libpinert.so
endif

ifndef RUNTIME_NAME
	$(error your platform isn't supported, sorry! Only Linux and Darwin work.)
endif

ifeq ($(RELEASE),1)
	TARGET_DIR :=target/release
	CARGO_FLAGS := --release
else
	TARGET_DIR :=target/debug
	CARGO_FLAGS :=
	CFLAGS += -DDEBUG -g
endif

PINE_COMPILER := $(TARGET_DIR)/$(PINEC)
PINE_RUNTIME  := $(TARGET_DIR)/$(RUNTIME_NAME)
PINE_TEST_HARNESS := $(TARGET_DIR)/pine_test
PINE_RUNTIME_SOURCES := $(shell find pine_rt -name *.c)
PINE_RUNTIME_OBJECTS := $(PINE_RUNTIME_SOURCES:.c=.o)

PINEC_SOURCE_DIR := pinec
PINEC_TEST_DIR := test

all: $(PINE_COMPILER) $(PINE_RUNTIME)

$(PINE_COMPILER):
	mkdir -p $(TARGET_DIR)
	cd pinec; cargo build $(CARGO_FLAGS)
	cp pinec/$(PINE_COMPILER) $(PINE_COMPILER)

$(PINE_RUNTIME): $(PINE_RUNTIME_OBJECTS)
	mkdir -p $(TARGET_DIR)
	$(CC) $(LDFLAGS) $(PINE_RUNTIME_OBJECTS) -shared -o $(PINE_RUNTIME)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

install: $(PINE_COMPILER) $(PINE_RUNTIME)
	cp $(PINE_COMPILER) $(INSTALL_PATH)/bin
	cp $(PINE_RUNTIME) $(INSTALL_PATH)/lib

test: $(PINE_COMPILER) $(PINE_RUNTIME) $(PINE_TEST_HARNESS)
	RUST_TEST_THREADS=1 PINE_BINARIES=$(TARGET_DIR) PINEC_TEST_SOURCES=$(PINEC_TEST_DIR) $(PINE_TEST_HARNESS)

$(PINE_TEST_HARNESS):
	cd pine_test; cargo build $(CARGO_FLAGS)
	cp pine_test/$(PINE_TEST_HARNESS) $(PINE_TEST_HARNESS)

check-%:
	@echo $($*)

compiler: $(PINE_COMPILER)

runtime: $(PINE_RUNTIME)

clean:
	cd pinec; cargo clean
	cd pine_test; cargo clean
	rm -rf target
	rm -f $(shell find pine_rt -name "*.o") $(shell find . -name "*~")
