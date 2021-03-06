CXX = g++
INCLUDE = -I include -I examples
OPTIM = -O3 -DNDEBUG -march=native
WARN = -Wall -Wextra -pedantic
CXXFLAGS = -std=c++11 $(INCLUDE) $(OPTIM) $(WARN)
LINK = -ltwinpeaks2018 -lpthread -lyaml-cpp

default:
	@echo "Compiling library source files..."
	$(CXX) $(CXXFLAGS) -c src/Config.cpp
	$(CXX) $(CXXFLAGS) -c src/RNG.cpp
	$(CXX) $(CXXFLAGS) -c src/RNGPool.cpp
	$(CXX) $(CXXFLAGS) -c src/Utils.cpp

	@echo "Making static library..."
	ar rcs libtwinpeaks2018.a *.o

	@echo "Compiling examples..."
	$(CXX) $(CXXFLAGS) -c examples/Atoms.cpp
	$(CXX) $(CXXFLAGS) -c examples/Demo.cpp
	$(CXX) $(CXXFLAGS) -c examples/Potts.cpp
	$(CXX) $(CXXFLAGS) -c examples/Rosenbrock.cpp
	$(CXX) $(CXXFLAGS) -c examples/SpikeSlab.cpp

	@echo "Building binaries..."
	$(CXX) $(CXXFLAGS) -c src/main.cpp
	$(CXX) -pthread -L . -o TwinPeaks2018 *.o $(LINK)

	@echo "Tidying up..."
	rm -f *.o

