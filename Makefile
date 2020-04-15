SRC=src/environment.hs \
		src/error.hs \
		src/eval.hs \
		src/ffi.hs \
		src/function.hs \
		src/interpreter.hs \
		src/parser.hs \
		src/primitives.hs \
		src/value.hs \
		src/ymir.hs

DST=bin/ymir

INCLUDE=include/environment.hs \
				include/error.hs \
				include/eval.hs \
				include/extern_stub.h \
				include/ffi.hs \
				include/function.hs \
				include/interpreter.hs \
				include/parser.hs \
				include/primitives.hs \
				include/value.hs

all: $(DST) $(INCLUDE) clean

$(DST): $(SRC)
	ghc -dynamic -o $(DST) $(SRC)

$(INCLUDE): $(SRC)
	cp src/value.hs include/
	cp src/error.hs include/
	cp src/primitives.hs include/
	cp src/ffi.hs include/
	cp src/function.hs include/
	cp src/interpreter.hs include/
	cp src/parser.hs include/
	cp src/extern.hs include/
	cp src/extern_stub.h include/extern_stub.h

clean: $(INCLUDE)
	rm src/*.hi
	rm src/*.o
