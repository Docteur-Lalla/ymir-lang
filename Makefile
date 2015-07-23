SRC=src/value.hs src/error.hs src/variable.hs src/primitives.hs src/ffi.hs src/eval.hs src/parser.hs src/ymir.hs
DST=bin/ymir

$(DST): $(SRC)
	ghc -dynamic -o $(DST) $(SRC)
