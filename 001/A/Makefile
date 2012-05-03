HC = ghc
CFLAGS = --make -O
TARGET = main
TARGETS = $(TARGET) gen
OBJS = haskell.hi haskell.o
SOURCE = haskell.hs
GEN_SOURCE = gen.hs
GEN_OBJS = gen.hi gen.o

hs: $(TARGET)

$(TARGET): $(SOURCE)
	$(HC) $(CFLAGS) -o $@ $^
	rm $(OBJS)

gen: $(GEN_SOURCE)
	$(HC) $(CFLAGS) -o $@ $^
	rm $(GEN_OBJS)

clean:
	rm -f *.o $(TARGETS) $(OBJS) $(GEN_OBJS)

.PHONY: cpp clean

