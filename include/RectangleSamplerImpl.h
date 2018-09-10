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
    // Zero the LCC grid
    for(size_t i=0; i<num_particles; ++i)
        for(size_t j=0; j<num_particles; ++j)
            lcc_grid[i][j] = 0;

    // Ranks in terms of the two scalars
    auto rf = ranks(fs);
    auto rg = ranks(gs);
    for(size_t i=0; i<num_particles; ++i)
        lcc_grid[rf[i]][rg[i]] += 1;

/*
    // Print the lcc_grid
    for(size_t i=0; i<num_particles; ++i)
    {
        for(size_t j=0; j<num_particles; ++j)
            std::cout << lcc_grid[i][j] << ' ';
        std::cout << '\n';
    }
*/

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


/*
    // Print the lcc_grid
    for(size_t i=0; i<num_particles; ++i)
    {
        for(size_t j=0; j<num_particles; ++j)
            std::cout << lcc_grid[i][j] << ' ';
        std::cout << '\n';
    }
    exit(0);
*/
}

} // namespace TwinPeaks2018

