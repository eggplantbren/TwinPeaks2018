#ifndef TwinPeaks2018_RectangleSampler_h
#define TwinPeaks2018_RectangleSampler_h

#include <vector>
#include "RNG.h"

namespace TwinPeaks2018
{

// RectangleSampler over space T.
template<typename T>
class RectangleSampler
{
    private:

        // The number of particles
        size_t num_particles;

        // Particles and their scalars
        std::vector<T> particles;
        std::vector<std::vector<double>> scalars;

        // Current iteration
        unsigned int iteration;

        // Generate the particles from the prior
        void initialise(RNG& rng);

        // Test whether one tuple is below another
        static bool is_below(const std::tuple<double, double>& s_tb1,
                             const std::tuple<double, double>& s_tb2);

    public:

        // Constructor
        RectangleSampler(size_t _num_particles);

        // Run to a given depth
        void run_to_depth(double depth, RNG& rng);

};

} // namespace TwinPeaks2018

#include "RectangleSamplerImpl.h"

#endif

