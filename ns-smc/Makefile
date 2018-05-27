CXX = g++
INCLUDE = -I include -I examples
OPTIM = -O3 -DNDEBUG
WARN = -Wall -Wextra -pedantic
CXXFLAGS = -std=c++11 $(INCLUDE) $(OPTIM) $(WARN)

default:
	@echo "Compiling library source files..."
	$(CXX) $(CXXFLAGS) -c src/RNG.cpp
	$(CXX) $(CXXFLAGS) -c src/Utils.cpp

	@echo "Making static library..."
	ar rcs libtwinpeaks2018.a *.o

	@echo "Compiling examples..."
	$(CXX) $(CXXFLAGS) -c examples/SpikeSlab.cpp

	@echo "Building binaries..."
	$(CXX) $(CXXFLAGS) -c src/main.cpp
	$(CXX) -L . -o main *.o -ltwinpeaks2018

	rm -f *.o

