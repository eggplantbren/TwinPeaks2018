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
    using Example = Potts;

    // Load the configuration
    Config::global_config.load("config.yaml");

    // Use the SwitchSampler
    run_switch_sampler<Example>();

    return 0;
}

