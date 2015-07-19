SRC=src/value.hs src/error.hs src/primitives.hs src/eval.hs src/parser.hs src/ymir.hs
DST=bin/ymir

$(DST): $(SRC)
	ghc -o $(DST) $(SRC)
