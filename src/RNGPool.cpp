#include "RNGPool.h"

namespace TwinPeaks2018
{

RNGPool create(size_t num, unsigned int first_seed)
{
    RNGPool rng_pool;
    for(size_t i=0; i<num; ++i)
        rng_pool.emplace_back(RNG(first_seed + i));
    return rng_pool;
}

} // namespace TwinPeaks2018

