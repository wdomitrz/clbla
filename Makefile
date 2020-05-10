.PHONY: all parser_files clean

all: clbla clblai

ParClbla.hs:
	happy -gca Parser/ParClbla.y

LexClbla.hs:
	alex -g Parser/LexClbla.x

clbla: $(wildcard src/*.hs)
	ghc -isrc --make src/clbla.hs -o ./clbla -W -Wall

clblai: $(wildcard src/*.hs)
	ghc -isrc --make src/clblai.hs -o ./clblai -W -Wall

clean:
	rm -f src/*.hi src/*.o src/Parser/*.hi src/Parser/*.o

distclean: clean
	rm -f clbla clblai
