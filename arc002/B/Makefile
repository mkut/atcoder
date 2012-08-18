################################
## Makefile for Haskell
## Author: mkut
## Date  : 2012.4.16
## Rev   : 0.1
################################

## project settings
TARGET = main
SOURCES = haskell.hs
MILIBS = IO Contest

## command settings
HC = ghc
HCFLAGS = --make -O

## directory settings
MILIB_DIR = /home/mkut/milib/haskell

## file name macros
OBJS = $(SOURCES:%.hs=%.hi) $(SOURCES:%.hs=%.o)
MILIB_SRC = $(MILIBS:%=$(MILIB_DIR)/%.hs)

## make rules
.PHONY: default clean

default: $(TARGET)

$(TARGET): $(SOURCES) $(MILIB_SRC)
	$(HC) $(HCFLAGS) -o $@ $^
	rm $(OBJS)

clean:
	rm -f *.o $(TARGET) $(OBJS) $(GEN_OBJS)
