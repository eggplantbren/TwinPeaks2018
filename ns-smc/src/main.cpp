#include <ctime>
#include <iostream>
#include "Config.h"
#include "RNG.h"
#include "SwitchSampler.h"
#include "TwoScalars.h"

using namespace TwinPeaks2018;

int main()
{
    // Load the configuration
    Config::global_config.load("config.yaml");

    // Make an RNG
    RNG rng(Config::global_config.get_rng_seed());

    // Make a sampler and run it 100 times
    for(int i=0; i<100; ++i)
    {
        SwitchSampler<TwoScalars> sampler
                        (i+1, Config::global_config.get_num_particles());
        sampler.run_to_depth(Config::global_config.get_depth(), rng);
    }

    return 0;
}

