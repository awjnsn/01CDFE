EMULATOR := zig-out/bin/01CDFE
TEST_ROM := /opt/test_roms/cpu_instrs.gb

all: build test run

build: $(EMULATOR)
	zig build

test: src/main.zig src/state.zig
	zig test src/main.zig
	zig test src/state.zig

run:
	$(EMULATOR) $(TEST_ROM) 	

.PHONY: all build test
