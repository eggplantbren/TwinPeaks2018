#include <ctime>
#include <iostream>
#include "Config.h"
#include "All.h"
#include "RNG.h"
#include "RNGPool.h"
#include "SwitchSampler.h"

using namespace TwinPeaks2018;

int main()
{
    // Which example?
    using Example = Demo;
    static_assert(Example::num_scalars <= 2,
                  "Model must have two or fewer scalars");

    // Load the configuration
    Config::global_config.load("config.yaml");

    // Use the SwitchSampler
    run_switch_sampler<Example>();

    return 0;
}

