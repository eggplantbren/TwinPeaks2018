CXX = g++
INCLUDE = -I include -I examples
OPTIM = -O3 -DNDEBUG
WARN = -Wall -Wextra -pedantic
CXXFLAGS = -std=c++11 $(INCLUDE) $(OPTIM) $(WARN)

default:
	$(CXX) $(CXXFLAGS) -c src/RNG.cpp
	$(CXX) $(CXXFLAGS) -c src/Utils.cpp

	$(CXX) $(CXXFLAGS) -c examples/SpikeSlab.cpp
	$(CXX) $(CXXFLAGS) -c src/main.cpp

	$(CXX) $(CXXFLAGS) -o main *.o

	rm -f *.o

