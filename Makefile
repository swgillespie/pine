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

PINEC_VERSION=0.0.1-alpha
PINEC_COMPILE_TIME=$(shell date)

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

all: cargo_build_pinec $(PINE_RUNTIME)
	@echo "[+] Build complete!"

cargo_build_pinec:
	@mkdir -p $(TARGET_DIR)
	@echo "[+] Building pine compiler -> $(PINE_COMPILER)"
	@cd pinec; PINEC_VERSION="$(PINEC_VERSION)" PINEC_COMPILE_TIME="$(PINEC_COMPILE_TIME)" cargo build $(CARGO_FLAGS)
	@cp pinec/$(PINE_COMPILER) $(PINE_COMPILER)

$(PINE_RUNTIME): $(PINE_RUNTIME_OBJECTS)
	@mkdir -p $(TARGET_DIR)
	@echo "[+] Linking pine runtime -> $(PINE_RUNTIME)"
	@$(CC) $(LDFLAGS) $(PINE_RUNTIME_OBJECTS) -shared -o $(PINE_RUNTIME)

%.o: %.c
	@echo "[+] CC $< -> $@"
	@$(CC) $(CFLAGS) -c -o $@ $<

install: cargo_build_pinec $(PINE_RUNTIME)
	@echo "[+] $(PINE_COMPILER) -> $(INSTALL_PATH)/bin"
	@cp $(PINE_COMPILER) $(INSTALL_PATH)/bin
	@echo "[+] $(PINE_RUNTIME) -> $(INSTALL_PATH)/lib"
	@cp $(PINE_RUNTIME) $(INSTALL_PATH)/lib

test: cargo_build_pinec $(PINE_RUNTIME) cargo_build_test_harness
	@echo "[+] Running tests"
	@RUST_TEST_THREADS=1 PINE_BINARIES=$(TARGET_DIR) PINEC_TEST_SOURCES=$(PINEC_TEST_DIR) $(PINE_TEST_HARNESS)

cargo_build_test_harness:
	@echo "[+] Building pinec test harness -> $(PINE_TEST_HARNESS)"
	@cd pine_test; cargo build $(CARGO_FLAGS)
	@cp pine_test/$(PINE_TEST_HARNESS) $(PINE_TEST_HARNESS)

check-%:
	@echo $($*)

clean:
	@echo "[+] Removing compilation artifacts"
	@cd pinec; cargo clean
	@cd pine_test; cargo clean
	@rm -rf target
	@rm -f $(shell find pine_rt -name "*.o") $(shell find . -name "*~")
