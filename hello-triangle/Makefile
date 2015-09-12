SOURCES=$(wildcard *.hs)

Main: $(SOURCES)
	ghc $^ -i. -i.. -o $@

.PHONY: clean

clean:
	-rm *.o *.hi Main
