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

    std::fstream fout("lnz_estimates.txt", std::ios::out);
    fout << std::setprecision(12);
    for(int i=0; i<1000000; ++i)
    {
        // Make a sampler and run it
        Sampler<SpikeSlab> sampler(1000);
        sampler.run_to_depth(100.0, rng);
        fout << sampler.get_lnz_estimate() << std::endl;
    }

    return 0;
}

