.PHONY: all clean

all: interpreter clbla clblai

interpreter: clbla
	cp clbla interpreter

clbla: $(wildcard src/*.hs) $(wildcard src/Parser/*.hs)
	ghc -isrc --make src/clbla.hs -o ./clbla -W -Wall

clblai: $(wildcard src/*.hs) $(wildcard src/Parser/*.hs)
	ghc -isrc --make src/clblai.hs -o ./clblai -W -Wall

clean:
	rm -f src/*.hi src/*.o src/Parser/*.hi src/Parser/*.o

distclean: clean
	rm -f interpreter clbla clblai
