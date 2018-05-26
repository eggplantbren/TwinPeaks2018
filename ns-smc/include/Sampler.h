#ifndef TwinPeaks2018_Sampler_h
#define TwinPeaks2018_Sampler_h

#include <vector>

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

    public:

        // Constructor
        Sampler(size_t _num_particles);


};

} // namespace TwinPeaks2018

#include "SamplerImpl.h"

#endif

