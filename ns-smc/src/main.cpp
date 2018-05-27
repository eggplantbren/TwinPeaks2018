#include <ctime>
#include <iostream>
#include "Config.h"
#include "RNG.h"
#include "Sampler.h"
#include "SpikeSlab.h"

using namespace TwinPeaks2018;

int main()
{
    // Load the configuration
    Config::global_config.load("config.yaml");

    // Make an RNG
    RNG rng(time(0));

    // Make a sampler and run it
    Sampler<SpikeSlab> sampler(100);
    sampler.run_to_depth(100.0, rng);
    std::cout << "ln(Z) = " << sampler.get_lnz_estimate() << ".\n";

    return 0;
}

