#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <sstream>
#include <thread>
#include "Config.h"
#include "Utils.h"

namespace TwinPeaks2018
{

// These are static
template<typename T>
std::mutex SwitchSampler<T>::particles_file_mutex;

template<typename T>
std::mutex SwitchSampler<T>::particles_info_file_mutex;

template<typename T>
std::mutex SwitchSampler<T>::stdout_mutex;

// Constructor
template<typename T>
SwitchSampler<T>::SwitchSampler(unsigned int _id, size_t _num_particles)
:id(_id)
,num_particles(_num_particles)
,ln_compression_ratio(log(((double)num_particles - 1.0)/num_particles))
,direction(T::num_scalars)
,threshold(T::num_scalars, -1E300)
,tiebreakers_threshold(T::num_scalars, 0.0)
,particles(num_particles)
,scalars(num_particles, std::vector<double>(T::num_scalars))
,tiebreakers(num_particles, std::vector<double>(T::num_scalars))
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
        for(double& tb: tiebreakers[i])
            tb = rng.rand();
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
    auto worst_stb = std::tuple<double, double>(scalars[worst][scalar],
                                                tiebreakers[worst][scalar]);

    for(size_t i=1; i<num_particles; ++i)
    {
        auto current_stb = std::tuple<double, double>(scalars[i][scalar],
                                                      tiebreakers[i][scalar]);
        if(is_below(current_stb, worst_stb))
        {
            worst = i;
            worst_stb = current_stb;
        }
    }

    return worst;
}

template<typename T>
bool SwitchSampler<T>::is_below(const std::tuple<double, double>& s_tb1,
                                const std::tuple<double, double>& s_tb2)
{
    // Unpack tuples
    double s1, s2, tb1, tb2;
    std::tie(s1, tb1) = s_tb1;
    std::tie(s2, tb2) = s_tb2;

    if(s1 < s2)
        return true;
    if(s1 == s2 && tb1 < tb2)
        return true;
    return false;
}


template<typename T>
void SwitchSampler<T>::save_particle(size_t k, double ln_prior_mass) const
{
    // Open CSV file
    std::fstream fout("output/particles_info.csv", (iteration==1 && id == 1)?
                                                   (std::ios::out):
                                                   (std::ios::out
                                                            | std::ios::app));

    particles_info_file_mutex.lock();

    // Use a good precision
    fout << std::setprecision(12);

    // Print header
    if(iteration == 1 && id == 1)
    {
        // Column names
        std::vector<std::string> header = {"run_id", "iteration",
                                           "ln_prior_mass"};
        for(size_t i=0; i<T::num_scalars; ++i)
        {
            std::stringstream temp;
            temp << "scalars[" << i << ']';
            header.push_back(temp.str());
        }
        fout << render(header, false) << std::endl;
    }

    // Write the particle info to the file
    fout << id << ',' << iteration << ',';
    fout << ln_prior_mass << ',';
    fout << render(scalars[k], false) << std::endl;
    fout.close();

    particles_info_file_mutex.unlock();

    // Now do the particle itself
    particles_file_mutex.lock();
    fout.open("output/particles.csv", (iteration==1 && id == 1)?
                                      (std::ios::out):
                                      (std::ios::out | std::ios::app));
    // Print header
    if(iteration == 1 && id == 1)
        fout << T::description() << std::endl;
    fout << particles[k] << std::endl;
    fout.close();
    particles_file_mutex.unlock();
}

template<typename T>
void SwitchSampler<T>::replace(size_t k, RNG& rng)
{
    stdout_mutex.lock();
    std::cout << "    Replacing dead particle..." << std::flush;
    stdout_mutex.unlock();

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
    tiebreakers[k] = tiebreakers[copy];

    // Do MCMC
    unsigned int accepted = 0;
    for(unsigned int i=0; i<Config::global_config.get_mcmc_steps(); ++i)
    {
        T proposal = particles[k];
        double logH = proposal.perturb(rng);
        auto ss = proposal.scalars();
        auto tbs = tiebreakers[copy];
        int which = rng.rand_int(tbs.size());
        tbs[which] += rng.randh();
        wrap(tbs[which], 0.0, 1.0);

        if((rng.rand() <= exp(logH)) && satisfies_threshold(ss, tbs))
        {
            particles[k] = proposal;
            scalars[k] = ss;
            tiebreakers[k] = tbs;
            ++accepted;
        }
    }

    stdout_mutex.lock();
    std::cout << "done. ";
    std::cout << "Acceptance ratio = " << accepted << '/';
    std::cout << Config::global_config.get_mcmc_steps() << ".\n" << std::endl;
    std::cout << "done.\n" << std::endl;
    stdout_mutex.unlock();
}


template<typename T>
bool SwitchSampler<T>::satisfies_threshold(const std::vector<double>& ss,
                                           const std::vector<double>& tbs) const
{
    for(size_t i=0; i<ss.size(); ++i)
        if(is_below(std::tuple<double, double>{ss[i], tbs[i]},
                    std::tuple<double, double>{threshold[i],
                                               tiebreakers_threshold[i]}))
            return false;
    return true;
}


template<typename T>
void SwitchSampler<T>::do_iteration(RNG& rng, bool replace_dead_particle)
{
    // Do initialization if necessary
    if(iteration == 0)
    {
        stdout_mutex.lock();
        initialize(rng);
        stdout_mutex.unlock();

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
    stdout_mutex.lock();
    std::cout << std::setprecision(12);
    std::cout << "Rep " << id << ", ";
    std::cout << "iteration " << iteration << ".\n";
    std::cout << "    ";
    std::cout << "ln(X) = " << (-(double)iteration/num_particles) << '.';
    std::cout << std::endl;
    stdout_mutex.unlock();

    // Save to file
    save_particle(kill, ln_prior_mass);

    // Update threshold
    threshold[scalar] = scalars[kill][scalar];
    tiebreakers_threshold[scalar] = tiebreakers[kill][scalar];

    stdout_mutex.lock();
    std::cout << "    New threshold = " << render(threshold) << ".";
    std::cout << std::endl;
    stdout_mutex.unlock();

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

template<typename T>
void do_rep(unsigned int id, RNG& rng)
{
    SwitchSampler<T> sampler(id, Config::global_config.get_num_particles());
    sampler.run_to_depth(Config::global_config.get_depth(), rng);
}

template<typename T>
RNGPool do_batch(unsigned int first_id, unsigned int last_id, RNGPool& rngs)
{
    if(last_id < first_id || rngs.size() < (last_id - first_id + 1))
        throw std::invalid_argument("Invalid input to do_reps.");

    #ifdef NO_THREADS
        int k = 0;
        for(unsigned int i=first_id; i<=last_id; ++i)
        {
            do_rep<T>(i, rngs[k]);
            ++k;
        }
    #else
        // Set up and run the threads
        std::vector<std::thread> threads;
        int k = 0;
        for(unsigned int i=first_id; i<=last_id; ++i)
        {
		    auto func = std::bind(do_rep<T>, i, std::ref(rngs[k]));
            threads.emplace_back(std::thread(func));
            ++k;
        }
        for(auto& thread: threads)
            thread.join();
    #endif

    return rngs;
}

} // namespace TwinPeaks2018

