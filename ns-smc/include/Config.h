#ifndef TwinPeaks2018_Config_h
#define TwinPeaks2018_Config_h

#include <cstdlib>

namespace TwinPeaks2018
{

class Config
{
    public:

        // A global static instance (singleton pattern)
        static Config global_config;

    private:

        // Fairly self-explanatory quantities
        unsigned int rng_seed;
        size_t num_particles;
        unsigned int mcmc_steps;
        unsigned int thin;
        unsigned int num_threads;

        // Target depth
        double depth;

    public:

        // Constructor uses default values
        Config();

        // Load from a YAML file.
        void load(const char* filename);

};

} // namespace TwinPeaks2018

#endif

