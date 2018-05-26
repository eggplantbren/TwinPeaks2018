#include <ctime>
#include <iostream>
#include "Logger.h"
#include "RNG.h"
#include "Sampler.h"
#include "SpikeSlab.h"

using namespace TwinPeaks2018;

int main()
{
    // Make an RNG
    RNG rng(time(0));

    // Choose the verbosity level of the logger
    Logger::logger.set_verbosity(Verbosity::high);

    // Make a sampler
    Sampler<SpikeSlab> sampler(100);

    for(int i=0; i<10000; ++i)
        sampler.do_iteration(rng);

    return 0;
}

