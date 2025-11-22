CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -pedantic
LDFLAGS :=
LDLIBS :=

# Set USE_READLINE=0 to build without readline/history support.
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
