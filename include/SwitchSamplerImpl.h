#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include "Config.h"
#include "Utils.h"

namespace TwinPeaks2018
{

// Constructor
template<typename T>
SwitchSampler<T>::SwitchSampler(unsigned int _id, size_t _num_particles)
:id(_id)
,num_particles(_num_particles)
,ln_compression_ratio(log(((double)num_particles - 1.0)/num_particles))
,direction(T::num_scalars)
,threshold(T::num_scalars, -1E300)
,particles(num_particles)
,scalars(num_particles, std::vector<double>(T::num_scalars))
,iteration(0)
{
    if(num_particles <= 1)
        throw std::invalid_argument("Must use two or more particles.");
}


template<typename T>
void SwitchSampler<T>::initialize(RNG& rng)
{
    std::cout << "Generating " << num_particles << " particles...";
    std::cout << std::flush;

    for(size_t i=0; i<num_particles; ++i)
    {
        particles[i].from_prior(rng);
        scalars[i] = particles[i].scalars();
    }
    std::cout << "done." << std::endl;

    // Generate direction
    for(double& d: direction)
        d = exp(3.0*rng.randt2());
    double d_max = *max_element(direction.begin(), direction.end());
    for(double& d: direction)
        d /= d_max;    

    std::cout << "Direction = " << render(direction) << "." << std::endl;
}


template<typename T>
size_t SwitchSampler<T>::find_worst(size_t scalar) const
{
    size_t worst = 0;
    for(size_t i=1; i<num_particles; ++i)
        if(scalars[i][scalar] < scalars[worst][scalar])
            worst = i;
    return worst;
}

template<typename T>
void SwitchSampler<T>::save_particle(size_t k, double ln_prior_mass) const
{
    // Open CSV file
    std::fstream fout("output/particles_info.csv", (iteration==1 && id == 1)?
                                                   (std::ios::out):
                                                   (std::ios::out
                                                            | std::ios::app));

    // Use a good precision
    fout << std::setprecision(12);

    // Print header
    if(iteration == 1 && id == 1)
    {
        fout << "run_id,iteration,ln_prior_mass,";
        for(size_t i=0; i<T::num_scalars; ++i)
        {
            fout << "scalars[" << i << ']';
            if(i != (T::num_scalars - 1))
                fout << ',';
        }
        fout << std::endl;
    }

    // Write the particle info to the file
    fout << id << ',' << iteration << ',';
    fout << ln_prior_mass << ',';
    fout << render(scalars[k], false) << std::endl;
    fout.close();

    // Now do the particle itself
    fout.open("output/particles.csv", (iteration==1 && id == 1)?
                                      (std::ios::out):
                                      (std::ios::out | std::ios::app));
    // Print header
    if(iteration == 1 && id == 1)
        fout << T::description() << std::endl;
    fout << particles[k] << std::endl;
    fout.close();
}

template<typename T>
void SwitchSampler<T>::replace(size_t k, RNG& rng)
{
    std::cout << "    Replacing dead particle..." << std::flush;

    // Choose particle to clone
    int copy;
    while(true)
    {
        copy = rng.rand_int(num_particles);
        if(copy != (int)k)
            break;
    }

    // Clone
    particles[k] = particles[copy];
    scalars[k] = scalars[copy];

    // Do MCMC
    unsigned int accepted = 0;
    for(unsigned int i=0; i<Config::global_config.get_mcmc_steps(); ++i)
    {
        T proposal = particles[k];
        double logH = proposal.perturb(rng);
        auto s = proposal.scalars();

        if((rng.rand() <= exp(logH)) && satisfies_threshold(s))
        {
            particles[k] = proposal;
            scalars[k] = s;
            ++accepted;
        }
    }

    std::cout << "done. ";
    std::cout << "Acceptance ratio = " << accepted << '/';
    std::cout << Config::global_config.get_mcmc_steps() << ".\n" << std::endl;
    std::cout << "done.\n" << std::endl;
}


template<typename T>
bool SwitchSampler<T>::satisfies_threshold(const std::vector<double>& ss) const
{
    for(size_t i=0; i<ss.size(); ++i)
        if(ss[i] <= threshold[i])
            return false;
    return true;
}



template<typename T>
void SwitchSampler<T>::do_iteration(RNG& rng, bool replace_dead_particle)
{
    // Do initialization if necessary
    if(iteration == 0)
    {
        initialize(rng);
        ++iteration;
    }

    // Choose scalar to ascend
    size_t scalar;
    while(true)
    {
        scalar = rng.rand_int(2);
        if(rng.rand() <= direction[scalar])
            break;
    }

    // Find worst particle
    size_t kill = find_worst(scalar);

    // Assign prior mass
    double lnx_left = iteration*ln_compression_ratio;
    double lnx_right = (iteration-1)*ln_compression_ratio;
    double ln_prior_mass = logdiffexp(lnx_right, lnx_left);

    // Print stuff
    std::cout << std::setprecision(12);
    std::cout << "Rep " << id << ", ";
    std::cout << "iteration " << iteration << ".\n";
    std::cout << "    ";
    std::cout << "ln(X) = " << (-(double)iteration/num_particles) << '.';
    std::cout << std::endl;

    // Save to file
    save_particle(kill, ln_prior_mass);

    // Update threshold
    threshold[scalar] = scalars[kill][scalar];
    std::cout << "    New threshold = " << render(threshold) << ".";
    std::cout << std::endl;

    // Replace particle
    if(replace_dead_particle)
        replace(kill, rng);

    ++iteration;
}

template<typename T>
void SwitchSampler<T>::run_to_depth(double depth, RNG& rng)
{
    unsigned int iterations = depth*num_particles;
    for(unsigned int i=0; i<iterations; ++i)
        do_iteration(rng, i != (iterations-1));
}

} // namespace TwinPeaks2018

