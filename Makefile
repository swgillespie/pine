INSTALL_PATH?=/usr/local

PINE_COMPILER=pinec/target/release/pinec
PINE_RUNTIME=pine_rt/libpinert.dylib

PINEC_SOURCE_DIR := pinec

all: $(PINE_COMPILER) $(PINE_RUNTIME)

$(PINE_COMPILER):
	cd pinec; cargo build --release

$(PINE_RUNTIME):
	make -C pine_rt

install:
	cp $(PINE_COMPILER) $(INSTALL_PATH)/bin
	make -C pine_rt/ install

clean:
	make -C pine_rt/ clean
	cd pinec; cargo clean
