#ifndef TwinPeaks2018_RectangleSampler_h
#define TwinPeaks2018_RectangleSampler_h

#include <ostream>
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
        std::vector<double> fs, gs;
        std::vector<double> augmented_uccs; // Augmented with tiebreakers

        // Background particles
        std::vector<T> background;
        std::vector<double> background_fs, background_gs;

        // Current iteration
        unsigned int iteration;

        // Generate the particles from the prior
        void initialise(RNG& rng);

        // Compute and augment uccs
        void compute_uccs(RNG& rng);

        // Do one iteration
        void do_iteration(RNG& rng);

    public:

        // Constructor
        RectangleSampler(size_t _num_particles);

        // Run to a given depth
        void run_to_depth(double depth, RNG& rng);
};

} // namespace TwinPeaks2018

#include "RectangleSamplerImpl.h"

#endif

