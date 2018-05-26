#include <iostream>
#include <stdexcept>

namespace TwinPeaks2018
{

// Constructor
template<typename T>
Sampler<T>::Sampler(size_t _num_particles)
:num_particles(_num_particles)
,particles(num_particles)
,logls(num_particles)
,iteration(0)
{
    if(num_particles == 0)
        throw std::invalid_argument("Can't use zero particles.");
}


template<typename T>
void Sampler<T>::initialize(RNG& rng)
{
    std::cout << "# Generating " << num_particles << " particles...";
    std::cout << std::flush;

    for(size_t i=0; i<num_particles; ++i)
    {
        particles[i].from_prior(rng);
        logls[i] = particles[i].log_likelihood();
    }

    std::cout << "done." << std::endl;
}


template<typename T>
void Sampler<T>::do_iteration(RNG& rng)
{
    if(iteration == 0)
        initialize();


    ++iteration;
}

} // namespace TwinPeaks2018

