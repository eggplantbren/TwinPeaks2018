CXX = g++
INCLUDE = -I include
OPTIM = -O3 -DNDEBUG
WARN = -Wall -Wextra -pedantic
CXXFLAGS = $(INCLUDE) $(OPTIM) $(WARN)

default:
	$(CXX) $(CXXFLAGS) -c src/RNG.cpp
	$(CXX) $(CXXFLAGS) -c include/Sampler.h
	$(CXX) $(CXXFLAGS) -c src/Utils.cpp

	$(CXX) $(CXXFLAGS) -c src/main.cpp

	$(CXX) $(CXXFLAGS) -o main *.o

	rm -f *.o

