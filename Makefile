CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -pedantic
LDFLAGS :=
LDLIBS :=

USE_READLINE ?= 1
ifeq ($(USE_READLINE),1)
  CXXFLAGS += -DLATTICE_USE_READLINE
  LDLIBS += -lreadline
endif

TARGET := lattice
SRC := main.cpp

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRC)
	$(CXX) $(CXXFLAGS) $(SRC) -o $(TARGET) $(LDFLAGS) $(LDLIBS)

clean:
	rm -f $(TARGET)
