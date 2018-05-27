#ifndef TwinPeaks2018_SwitchSampler_h
#define TwinPeaks2018_SwitchSampler_h

#include <vector>
#include "RNG.h"

namespace TwinPeaks2018
{

// SwitchSampler over space T.
// These use the version of the algorithm where a different scalar is
// increased at each iteration. Multiple runs are needed.
template<typename T>
class SwitchSampler
{
    private:

        // Whether it's the first run
        bool first_run;

        // The number of particles and the implied compression factor
        size_t num_particles;
        double ln_compression_ratio;
        std::vector<double> direction;
        std::vector<double> threshold;

        // Particles and their scalars
        std::vector<T> particles;
        std::vector<double> fs;
        std::vector<double> gs;

        // Current iteration
        unsigned int iteration;

        // Generate initial particles
        void initialize(RNG& rng);

        // Find worst particle and return its index
        size_t find_worst(size_t scalar) const;

        // Save the kth particle to disk
        void save_particle(size_t k, double ln_prior_mass) const;

        // Replace the kth particle (must be the worst!)
        void replace(size_t k, RNG& rng);

        // Do one iteration
        void do_iteration(RNG& rng, bool replace_dead_particle=true);

    public:

        // Constructor
        SwitchSampler(size_t _num_particles, bool _first_run=true);

        // Run to a given depth
        void run_to_depth(double depth, RNG& rng);

        // Getter
        double get_lnz_estimate() const;

};

} // namespace TwinPeaks2018

#include "SwitchSamplerImpl.h"

#endif

