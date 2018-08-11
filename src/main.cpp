#include <ctime>
#include <iostream>
#include "Config.h"
#include "All.h"
#include "RNG.h"
#include "RNGPool.h"
#include "RectangleSampler.h"

using namespace TwinPeaks2018;

int main()
{
    // Which example?
    using Example = Demo;

    // Load the configuration
    Config::global_config.load("config.yaml");

    // RectangleSampler
    size_t num_particles = Config::global_config.get_num_particles();
    RectangleSampler<Example> sampler(num_particles);

    // Run with a default RNG for now
    RNG rng;
    sampler.run_to_depth(500.0, rng);

    return 0;
}

