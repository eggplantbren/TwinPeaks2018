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

        // Generate initial particles
        void initialize(RNG& rng);

    public:

        // Constructor
        Sampler(size_t _num_particles);

        // Do one iteration
        void do_iteration(RNG& rng);


};

} // namespace TwinPeaks2018

#include "SamplerImpl.h"

#endif

