#include <ctime>
#include <iostream>
#include "RNG.h"
#include "Sampler.h"
#include "SpikeSlab.h"

using namespace TwinPeaks2018;

int main()
{
    // Make an RNG
    RNG rng(time(0));

    // Make a sampler
    Sampler<SpikeSlab> sampler(100);
    sampler.do_iteration(rng);

    return 0;
}

