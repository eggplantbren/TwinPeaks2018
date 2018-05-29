#ifndef TwinPeaks2018_RNGPool_h
#define TwinPeaks2018_RNGPool_h

#include "RNG.h"
#include <vector>

namespace TwinPeaks2018
{

// An RNGPool is just a vector of RNGs!
using RNGPool = std::vector<RNG>;

// Create an RNGPool
RNGPool create(size_t num, unsigned int first_seed);

}

#endif

