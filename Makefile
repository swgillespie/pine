INSTALL_PATH?=/usr/local

PINE_COMPILER=pinec/target/debug/pinec
PINE_RUNTIME=pine_rt/libpinert.dylib

PINEC_SOURCE_DIR := pinec
PINEC_TEST_DIR := test

all: $(PINE_COMPILER) $(PINE_RUNTIME)

$(PINE_COMPILER):
	cd pinec; cargo build

$(PINE_RUNTIME):
	make -C pine_rt

install:
	cp $(PINE_COMPILER) $(INSTALL_PATH)/bin
	make -C pine_rt/ install

test: $(PINE_COMPILER) $(PINE_RUNTIME)
	cd pine_test; cargo build;
	 PINEC=$(PINE_COMPILER) PINEC_TEST_SOURCES=$(PINEC_TEST_DIR) ./pine_test/target/debug/pine_test

clean:
	make -C pine_rt/ clean
	cd pinec; cargo clean
