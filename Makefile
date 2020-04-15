SRC=src/value.hs src/error.hs src/variable.hs src/primitives.hs src/ffi.hs src/function.hs src/eval.hs src/environment.hs src/interpreter.hs src/parser.hs src/extern.hs src/ymir.hs
DST=bin/ymir
INCLUDE=include/value.hs include/error.hs include/variable.hs include/primitives.hs include/ffi.hs include/function.hs include/eval.hs include/environment.hs include/interpreter.hs include/parser.hs include/extern.hs include/extern_stub.h

all: $(DST) $(INCLUDE) clean

$(DST): $(SRC)
	ghc -dynamic -o $(DST) $(SRC)

$(INCLUDE): $(SRC)
	cp src/value.hs include/
	cp src/error.hs include/
	cp src/variable.hs include/
	cp src/primitives.hs include/
	cp src/ffi.hs include/
	cp src/function.hs include/
	cp src/eval.hs include/
	cp src/interpreter.hs include/
	cp src/parser.hs include/
	cp src/extern.hs include/
	cp src/extern_stub.h include/extern_stub.h

clean: $(INCLUDE)
	rm src/*.hi
	rm src/*.o
