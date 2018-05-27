#include "Config.h"
#include <iostream>
#include <stdexcept>
#include <yaml-cpp/yaml.h>

namespace TwinPeaks2018
{

// Construct global instance
Config Config::global_config;

Config::Config()
:num_particles(100)
,mcmc_steps(1000)
,thin(1)
,num_threads(1)
,depth(1E12)
{

}

void Config::load(const char* filename)
{
    // Load the file
    YAML::Node file = YAML::LoadFile(filename);

    // Read in the values
    if(file["rng_seed"].as<std::string>() == "auto")
        rng_seed = time(0);
    else
        rng_seed = file["rng_seed"].as<unsigned int>();

    num_particles = file["num_particles"].as<size_t>();
    mcmc_steps = file["mcmc_steps"].as<unsigned int>();
    num_threads = file["num_threads"].as<unsigned int>();
    depth = (file["depth"].as<std::string>() == "auto")
            ?(1E12)
            :(file["depth"].as<double>());

    // Check for non-stupid values
    if(num_particles <= 1
                || mcmc_steps <= 0
                || num_threads == 0
                || num_threads > 24
                || depth < 5.0)
    {
        throw
          std::domain_error("Warning: Unusual values in configuration file.");
    }
}


} // namespace TwinPeaks2018

