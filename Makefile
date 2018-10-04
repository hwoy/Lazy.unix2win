GHC = ghc

2WIN = 2w
2UNIX = 2u
2MAC = 2m

.PHONY: all clean

all: $(2WIN) $(2UNIX) $(2MAC)

$(2WIN): 2w.hs
	$(GHC) -O2 -Wall 2w.hs

$(2UNIX): 2u.hs
	$(GHC) -O2 -Wall 2u.hs

$(2MAC): 2m.hs
	$(GHC) -O2 -Wall 2m.hs

clean:
	rm -rf *.hi *.o *.exe $(2WIN) $(2UNIX) $(2MAC)