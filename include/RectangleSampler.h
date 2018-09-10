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

        // Particle locations and LCC grid
        std::vector<std::vector<int>> particle_locations;
        std::vector<std::vector<unsigned int>> lcc_grid;

        // Current iteration
        unsigned int iteration;

        // Generate the particles from the prior
        void initialise(RNG& rng);

        // Do one iteration
        void do_iteration(RNG& rng);

        // Compute the LCC grid
        void compute_lcc_grid();

        // Test whether one tuple is below another
        static bool is_below(const std::tuple<double, double>& s_tb1,
                             const std::tuple<double, double>& s_tb2);

    public:

        // Constructor
        RectangleSampler(size_t _num_particles);

        // Run to a given depth
        void run_to_depth(double depth, RNG& rng);

        // Print the LCC grid
        void print_lcc_grid(std::ostream& out) const;

        // Getter
        const std::vector<std::vector<unsigned int>>& get_lcc_grid() const
        { return lcc_grid; }
};

// Find the lowest nonzero value in a vector of vectors
unsigned int lowest_nonzero_value
                (const std::vector<std::vector<unsigned int>>& xs);

} // namespace TwinPeaks2018

#include "RectangleSamplerImpl.h"

#endif

