#ifndef TwinPeaks2018_Sampler_h
#define TwinPeaks2018_Sampler_h

#include <vector>
#include "RNG.h"

namespace TwinPeaks2018
{

// Sampler over space T.
template<typename T>
class Sampler
{
    private:

        // The number of particles
        size_t num_particles;

        // Particles and their log likelihoods
        std::vector<T> particles;
        std::vector<double> logls;

        // Current iteration
        unsigned int iteration;

        // Estimate of ln(Z)
        double lnz_estimate;

        // Generate initial particles
        void initialize(RNG& rng);

        // Find worst particle and return its index
        size_t find_worst() const;

        // Save the kth particle to disk
        void save_particle(size_t k) const;

        // Replace the kth particle (must be the worst!)
        void replace(size_t k, RNG& rng);

        // Do one iteration
        void do_iteration(RNG& rng);

    public:

        // Constructor
        Sampler(size_t _num_particles);

        // Run to a given depth
        void run_to_depth(double depth, RNG& rng);

        // Getter
        double get_lnz_estimate() const;

};

} // namespace TwinPeaks2018

#include "SamplerImpl.h"

#endif

