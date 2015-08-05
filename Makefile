SRC=src/value.hs src/error.hs src/variable.hs src/primitives.hs src/ffi.hs src/eval.hs src/parser.hs src/ymir.hs
DST=bin/ymir
INCLUDE=include/value.hs include/ffi.hs include/ffi_stub.h

all: $(DST) $(INCLUDE) clean

$(DST): $(SRC)
	ghc -dynamic -o $(DST) $(SRC)

$(INCLUDE): $(SRC)
	cp src/value.hs include/value.hs
	cp src/ffi.hs include/ffi.hs
	cp src/ffi_stub.h include/ffi_stub.h

clean: $(INCLUDE)
	rm src/*.hi
	rm src/*.o
