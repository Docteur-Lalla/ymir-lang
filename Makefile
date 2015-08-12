SRC=src/value.hs src/error.hs src/variable.hs src/primitives.hs src/ffi.hs src/function.hs src/eval.hs src/parser.hs src/extern.hs src/ymir.hs
DST=bin/ymir
INCLUDE=include/value.hs include/extern.hs include/extern_stub.h

all: $(DST) $(INCLUDE) clean

$(DST): $(SRC)
	ghc -dynamic -o $(DST) $(SRC)

$(INCLUDE): $(SRC)
	cp src/*.hs include/
	cp src/extern_stub.h include/extern_stub.h

clean: $(INCLUDE)
	rm src/*.hi
	rm src/*.o
