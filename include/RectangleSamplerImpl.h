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
,particle_locations(num_particles, std::vector<int>(num_particles))
,lcc_grid(num_particles, std::vector<unsigned int>(num_particles))
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
    std::cout << "particles from the prior..." << std::flush;

    for(size_t i=0; i<num_particles; ++i)
    {
        particles[i].from_prior(rng);
        auto ss = particles[i].scalars();
        fs[i] = ss[0];
        gs[i] = ss[1];
    }

    std::cout << "done." << std::endl;

    iteration = 1;
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
    // Compute the LCC grid
    compute_lcc_grid();
}

template<typename T>
void RectangleSampler<T>::compute_lcc_grid()
{
    // Zero the LCC grid and particle locations
    for(size_t i=0; i<num_particles; ++i)
    {
        for(size_t j=0; j<num_particles; ++j)
        {
            particle_locations[i][j] = -1;
            lcc_grid[i][j] = 0;
        }
    }

    // Ranks in terms of the two scalars
    auto rf = ranks(fs);
    auto rg = ranks(gs);
    for(size_t i=0; i<num_particles; ++i)
    {
        particle_locations[rf[i]][rg[i]] = i;
        lcc_grid[rf[i]][rg[i]] += 1;
    }

    // Cumulative sum horizontally for each row
    for(size_t i=0; i<num_particles; ++i)
    {
        for(size_t j=1; j<num_particles; ++j)
            lcc_grid[i][j] = lcc_grid[i][j-1] + lcc_grid[i][j];
    }

    // Cumulative sum vertically for each column
    for(size_t j=0; j<num_particles; ++j)
    {
        for(int i=(int)num_particles-2; i>=0; --i)
            lcc_grid[i][j] = lcc_grid[i+1][j] + lcc_grid[i][j];
    }

}

template<typename T>
void RectangleSampler<T>::print_lcc_grid(std::ostream& out) const
{
    // Print the particle locations and lcc_grid
    out << "Particle locations:" << std::endl;
    for(size_t i=0; i<num_particles; ++i)
    {
        for(size_t j=0; j<num_particles; ++j)
            out << particle_locations[i][j] << ' ';
        out << '\n';
    }
    out << '\n';

    out << "LCC grid:" << std::endl;
    for(size_t i=0; i<num_particles; ++i)
    {
        for(size_t j=0; j<num_particles; ++j)
            out << lcc_grid[i][j] << ' ';
        out << '\n';
    }
}

unsigned int lowest_nonzero_value
        (const std::vector<std::vector<unsigned int>>& xs)
{
    // Flatten to 1D and exclude zeroes
    std::vector<double> ys;
    for(size_t i=0; i<xs.size(); ++i)
        for(size_t j=0; j<xs[i].size(); ++j)
            if(xs[i][j] != 0)
                ys.push_back(xs[i][j]);

    // Handle empty-vector case
    if(ys.size() == 0)
        return 0;

    // Find minimum
    return *min_element(ys.begin(), ys.end());
}

} // namespace TwinPeaks2018

