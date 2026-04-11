CFLAGS = -fPIC -shared -Wall -Wextra
ifdef DEBUG
    CFLAGS += -g3 -O0 -fsanitize=address,undefined
else
    CFLAGS += -O2
endif

all: build/oosh build/std.so

build/std.so: csrc/std.c
	@mkdir -p build
	cc $(CFLAGS) -Icsrc -o $@ $^

build/oosh: oosh.cabal app/* csrc/wrapper.c csrc/wrapper.h
	@mkdir -p build
	cabal build $(if $(DEBUG),-fdebug,)
	cp $$(cabal list-bin oosh) $@
