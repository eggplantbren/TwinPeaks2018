#include <iostream>
#include "Utils.h"

namespace TwinPeaks2018
{

template<typename T>
RectangleSampler<T>::RectangleSampler(size_t _num_particles)
:num_particles(_num_particles)
,particles(num_particles)
,fs(num_particles)
,gs(num_particles)
,augmented_uccs(num_particles)
,background(num_particles)
,background_fs(num_particles)
,background_gs(num_particles)
,iteration(0)
{

}

template<typename T>
void RectangleSampler<T>::initialise(RNG& rng)
{
    if(iteration != 0)
    {
        std::cerr << "Already initialised. Doing nothing." << std::endl;
        return;
    }

    std::cout << "# Generating " << num_particles << ' ';
    std::cout << "particles and " << num_particles << ' ';
    std::cout << "background particles from the prior..." << std::flush;

    for(size_t i=0; i<num_particles; ++i)
    {
        particles[i].from_prior(rng);
        auto ss = particles[i].scalars();
        fs[i] = ss[0];
        gs[i] = ss[1];

        background[i].from_prior(rng);
        ss = background[i].scalars();
        background_fs[i] = ss[0];
        background_gs[i] = ss[1];
    }

    compute_uccs(rng);

    std::cout << "done." << std::endl;
    iteration = 1;
}

template<typename T>
void RectangleSampler<T>::compute_uccs(RNG& rng)
{
    for(size_t i=0; i<num_particles; ++i)
    {
        augmented_uccs[i] = 0.0;
        for(size_t j=0; j<num_particles; ++j)
        {
            if(fs[i] < background_fs[j] && gs[i] < background_gs[j])
                augmented_uccs[i] += 1.0;
        }
        augmented_uccs[i] += rng.rand();
    }
}

template<typename T>
void RectangleSampler<T>::run_to_depth(double depth, RNG& rng)
{
    // Initialise the particles
    initialise(rng);

    // Do iterations
    size_t num_iterations = static_cast<size_t>(num_particles*depth);
    for(size_t i=0; i<num_iterations; ++i)
        do_iteration(rng);
}


template<typename T>
void RectangleSampler<T>::do_iteration(RNG& rng)
{


}


} // namespace TwinPeaks2018

