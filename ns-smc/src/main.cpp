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
    RNG rng(Config::global_config.get_rng_seed());

    // Make a sampler and run it
    Sampler<SpikeSlab> sampler(Config::global_config.get_num_particles());
    sampler.run_to_depth(Config::global_config.get_depth(), rng);
    std::cout << "ln(Z) = " << sampler.get_lnz_estimate() << ".\n";

    return 0;
}

