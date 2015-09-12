SOURCE=$(wildcard *.hs)

Main: $(SOURCE)
	ghc $^ -o $@ -i. -i..

.PHONY: clean

clean:
	-rm *.o *.hi Main
