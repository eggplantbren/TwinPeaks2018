#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include "Utils.h"

namespace TwinPeaks2018
{

// Constructor
template<typename T>
Sampler<T>::Sampler(size_t _num_particles)
:num_particles(_num_particles)
,ln_compression_ratio(log(((double)num_particles - 1.0)/num_particles))
,particles(num_particles)
,logls(num_particles)
,iteration(0)
,lnz_estimate(-1E300)
{
    if(num_particles <= 1)
        throw std::invalid_argument("Must use two or more particles.");
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
size_t Sampler<T>::find_worst() const
{
    size_t worst = 0;
    for(size_t i=1; i<num_particles; ++i)
        if(logls[i] < logls[worst])
            worst = i;
    return worst;
}

template<typename T>
void Sampler<T>::save_particle(size_t k, double ln_prior_mass) const
{
    // Open CSV file
    std::fstream fout("output/particles_info.csv", (iteration==1)?
                                                   (std::ios::out):
                                                   (std::ios::out
                                                            | std::ios::app));

    // Use a good precision
    fout << std::setprecision(12);

    // Print header
    if(iteration == 1)
        fout << "ln_prior_mass,ln_l" << std::endl;

    // Write the particle info to the file
    fout << ln_prior_mass << ',' << logls[k] << std::endl;
    fout.close();

    // Now do the particle itself
    fout.open("output/particles.csv", (iteration==1)?
                                      (std::ios::out):
                                      (std::ios::out | std::ios::app));
    // Print header
    if(iteration == 1)
        fout << T::description() << std::endl;

    particles[k].print(fout);
    fout << std::endl;

    fout.close();
}

template<typename T>
void Sampler<T>::replace(size_t k, RNG& rng)
{
    std::cout << "    Replacing dead particle..." << std::flush;

    // Save threshold
    double threshold = logls[k];

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
    logls[k] = logls[copy];

    // Do MCMC
    for(unsigned int i=0; i<1000; ++i)
    {
        T proposal = particles[k];
        double logH = proposal.perturb(rng);
        double logl = proposal.log_likelihood();
        if(rng.rand() <= exp(logH) && logl > threshold)
        {
            particles[k] = proposal;
            logls[k] = logl;
        }
    }

    std::cout << "done.\n" << std::endl;
}




template<typename T>
void Sampler<T>::do_iteration(RNG& rng, bool replace_dead_particle)
{
    // Do initialization if necessary
    if(iteration == 0)
    {
        initialize(rng);
        ++iteration;
    }

    // Find worst particle
    size_t kill = find_worst();

    // Update ln(Z) estimate
    double lnx_left = iteration*ln_compression_ratio;
    double lnx_right = (iteration-1)*ln_compression_ratio;
    double ln_prior_mass = logdiffexp(lnx_right, lnx_left);
    lnz_estimate = logsumexp(lnz_estimate, ln_prior_mass + logls[kill]);

    // Print stuff
    std::cout << std::setprecision(12);
    std::cout << "Iteration " << iteration << ".\n";
    std::cout << "    ";
    std::cout << "ln(X) = " << (-(double)iteration/num_particles) << ", ";
    std::cout << "ln(L) = " << logls[kill] << ", ";
    std::cout << "ln(Z) = " << lnz_estimate << '.' << std::endl;

    // Save to file
    save_particle(kill, ln_prior_mass);

    // Replace particle
    if(replace_dead_particle)
        replace(kill, rng);

    ++iteration;
}

template<typename T>
void Sampler<T>::run_to_depth(double depth, RNG& rng)
{
    unsigned int iterations = depth*num_particles;
    for(unsigned int i=0; i<iterations; ++i)
        do_iteration(rng, i != (iterations-1));
}

template<typename T>
double Sampler<T>::get_lnz_estimate() const
{
    return lnz_estimate;
}

} // namespace TwinPeaks2018

