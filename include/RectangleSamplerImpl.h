#include <iostream>

namespace TwinPeaks2018
{

template<typename T>
RectangleSampler<T>::RectangleSampler(size_t _num_particles)
:num_particles(_num_particles)
,particles(num_particles)
,scalars(num_particles, std::vector<double>(T::num_scalars))
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

    std::cout << "Generating " << num_particles << ' ';
    std::cout << "particles from the prior..." << std::flush;

    for(size_t i=0; i<num_particles; ++i)
    {
        particles[i].from_prior(rng);
        scalars[i] = particles[i].scalars();
    }

    std::cout << "done." << std::endl;

    iteration = 1;
}

template<typename T>
void RectangleSampler<T>::run_to_depth(double depth, RNG& rng)
{
    initialise(rng);
}

} // namespace TwinPeaks2018

