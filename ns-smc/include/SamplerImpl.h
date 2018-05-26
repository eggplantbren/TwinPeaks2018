#include <stdexcept>

namespace TwinPeaks2018
{

// Constructor
template<typename T>
Sampler<T>::Sampler(size_t _num_particles)
:num_particles(_num_particles)
{
    if(num_particles == 0)
        throw std::invalid_argument("Can't use zero particles.");
}

} // namespace TwinPeaks2018

