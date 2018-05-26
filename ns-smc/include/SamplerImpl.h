#include <fstream>
#include <iomanip>
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
void Sampler<T>::save_particle(size_t k) const
{
    // Open CSV file
    std::fstream fout("output.txt", (iteration==1)?
                                    (std::ios::out):
                                    (std::ios::out | std::ios::app));

    // Print header
    if(iteration == 1)
        fout << "# iteration logl" << std::endl;

    // Use a good precision
    fout << std::setprecision(12);
    std::cout << std::setprecision(12);
    std::cout << "Iteration " << iteration << ". ln(L) = ";
    std::cout << logls[k] << "." << std::endl;

    // Write the particle to the file
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

    std::cout << "done." << std::endl;
}




template<typename T>
void Sampler<T>::do_iteration(RNG& rng)
{
    // Do initialization if necessary
    if(iteration == 0)
    {
        initialize(rng);
        ++iteration;
    }

    // Find worst particle
    size_t kill = find_worst();

    // Save to file
    save_particle(kill);

    // Replace particle
    replace(kill, rng);

    ++iteration;
}

} // namespace TwinPeaks2018

