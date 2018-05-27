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

    // Make a sampler and run it 1000 times
    for(int i=0; i<10; ++i)
    {
        SwitchSampler<TwoScalars> sampler
                        (Config::global_config.get_num_particles(), i==0);
        sampler.run_to_depth(Config::global_config.get_depth(), rng);
    }

    return 0;
}

