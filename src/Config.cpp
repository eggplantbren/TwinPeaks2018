#include "Config.h"
#include <fstream>
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
,variable_mcmc_steps(false)
,thin(1)
,num_threads(1)
,switch_sampler_reps(100)
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

    // Save the rng seed to a file
    std::fstream fout("output/rng_seed.out", std::ios::out);
    fout << rng_seed;
    fout.close();


    num_particles = file["num_particles"].as<size_t>();
    mcmc_steps = file["mcmc_steps"].as<unsigned int>();
    variable_mcmc_steps = file["variable_mcmc_steps"].as<bool>();
    thin = file["thin"].as<unsigned int>();
    num_threads = file["num_threads"].as<unsigned int>();
    switch_sampler_reps = file["switch_sampler_reps"].as<unsigned int>();
    depth = file["depth"].as<double>();

    // Check for non-stupid values
    if(num_particles <= 1
                || mcmc_steps <= 0
                || thin == 0
                || num_threads == 0
                || switch_sampler_reps < 1)
    {
        throw
          std::domain_error("Warning: Invalid values in configuration file.");
    }
}


} // namespace TwinPeaks2018

