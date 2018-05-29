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
    unsigned int num_threads = Config::global_config.get_num_threads();
    unsigned int num_reps = Config::global_config.get_switch_sampler_reps();
    RNGPool rngs = create(num_threads, Config::global_config.get_rng_seed());

    // Compute the number of batches
    unsigned int num_batches = num_reps/num_threads;
    if(num_reps%num_threads != 0)
        ++num_batches;

    for(unsigned int i=0; i<num_batches; ++i)
    {
        unsigned int first = num_threads*i + 1;
        unsigned int last  = num_threads*i + num_threads;
        if(last > num_reps)
            last = num_reps;
        rngs = do_batch<Example>(first, last, rngs);
    }

    return 0;
}

