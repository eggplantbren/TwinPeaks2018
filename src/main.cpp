#include <ctime>
#include <iostream>
#include "Config.h"
#include "RNG.h"
#include "RNGPool.h"
#include "SwitchSampler.h"
#include "TwoScalars.h"

using namespace TwinPeaks2018;

int main()
{
    // Which example?
    using Example = TwoScalars;

    // Load the configuration
    Config::global_config.load("config.yaml");

    // Make a collection of RNGs
    RNGPool rngs(Config::global_config.get_num_threads(),
                 Config::global_config.get_rng_seed());

    // Make a sampler and run it many times
    for(unsigned int i=0; i<Config::global_config.get_switch_sampler_reps();
                                                                        ++i)
    {
        do_rep<Example>(i+1, rngs[0]);
    }

    return 0;
}

