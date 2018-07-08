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
std::mutex SwitchSampler<T>::files_mutex;

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
    messages << "Rep " << id << ":\n";
    messages << "    Generating " << num_particles << " particles...";

    for(size_t i=0; i<num_particles; ++i)
    {
        particles[i].from_prior(rng);
        scalars[i] = particles[i].scalars();
        for(double& tb: tiebreakers[i])
            tb = rng.rand();
    }

    messages << "done." << std::endl;

    // Generate direction
    for(double& d: direction)
    {
        double t;
        do
        {
            t = rng.randt2();
        }while(std::abs(t) >= 100.0);
        d = exp(t);
    }
    double d_max = *max_element(direction.begin(), direction.end());
    for(double& d: direction)
        d /= d_max;    

    messages << "    Direction = " << render(direction) << ".\n" << std::endl;
    print_messages();
}

template<typename T>
void SwitchSampler<T>::print_messages()
{
    stdout_mutex.lock();
    std::cout << messages.str() << std::flush;
    messages.str(std::string());
    stdout_mutex.unlock();
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
    files_mutex.lock();

    // Open CSV file
    std::fstream fout("output/particles_info.csv",
                      std::ios::out | std::ios::app);

    // Use a good precision
    fout << std::setprecision(12);

    // Write the particle info to the file
    fout << id << ',' << iteration << ',';
    fout << ln_prior_mass << ',';
    fout << render(scalars[k], false) << std::endl;
    fout.close();

    // Now do the particle itself
    fout.open("output/particles.csv", std::ios::out | std::ios::app);
    fout << particles[k] << std::endl;
    fout.close();

    files_mutex.unlock();
}

template<typename T>
void SwitchSampler<T>::replace(size_t k, RNG& rng)
{
    messages << "    Replacing dead particle...";

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

    messages << "done. ";
    messages << "Acceptance ratio = " << accepted << '/';
    messages << Config::global_config.get_mcmc_steps() << '.' << std::endl;
    messages << "done.\n" << std::endl;
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
        initialize(rng);
        ++iteration;
    }

    // Choose scalar to ascend
    size_t scalar;
    while(true)
    {
        scalar = rng.rand_int(T::num_scalars);
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
    messages << std::setprecision(12);
    messages << "Rep " << id << ", ";
    messages << "iteration " << iteration << ".\n";
    messages << "    ";
    messages << "ln(X) = " << (-(double)iteration/num_particles) << '.';
    messages << std::endl;

    // Save to file
    if(rng.rand() <= 1.0/Config::global_config.get_thin()) // Random thinning
        save_particle(kill, ln_prior_mass);

    // Update threshold
    threshold[scalar] = scalars[kill][scalar];
    tiebreakers_threshold[scalar] = tiebreakers[kill][scalar];

    messages << "    New threshold = " << render(threshold) << '.';
    messages << std::endl;

    // Replace particle
    if(replace_dead_particle)
        replace(kill, rng);

    print_messages();
    ++iteration;
}

template<typename T>
void SwitchSampler<T>::run_to_depth(double depth, RNG& rng)
{
    files_mutex.lock();
    std::fstream fout("output/reps.csv",
                                            std::ios::out | std::ios::app);
    fout << id << ',' << Config::global_config.get_mcmc_steps() << std::endl;
    fout.close();
    files_mutex.unlock();

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
    // Backup value of mcmc_steps from config file
    unsigned int orig_mcmc_steps = Config::global_config.get_mcmc_steps();

    // Change the number of mcmc steps, if we're allowing that
    if(Config::global_config.get_variable_mcmc_steps())
    {
        // Pareto (orig_mcmc_steps/2, 2) has a mean of orig_mcmc_steps
        unsigned int x = (unsigned int)(0.5*orig_mcmc_steps
                                        /pow(rngs[0].rand(), 0.5));
        Config::global_config.set_mcmc_steps(x);
    }

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
            threads.emplace_back(func);
            ++k;
        }
        for(auto& thread: threads)
            thread.join();
    #endif

    // Restore original mcmc_steps
    Config::global_config.set_mcmc_steps(orig_mcmc_steps);

    return rngs;
}

template<typename T>
void prepare_output_files()
{
    // Clear the output files and write the headers
    std::fstream fout;
    fout.open("output/particles_info.csv", std::ios::out);

    // Column names
    std::vector<std::string> header = {"rep_id", "iteration",
                                       "ln_prior_mass"};
    for(size_t i=0; i<T::num_scalars; ++i)
    {
        std::stringstream temp;
        temp << "scalars[" << i << ']';
        header.push_back(temp.str());
    }
    fout << render(header, false) << std::endl;
    fout.close();

    fout.open("output/particles.csv", std::ios::out);
    auto desc = T::description();
    if(desc.size() > 0)
        fout << desc << std::endl;
    fout.close();

    fout.open("output/reps.csv", std::ios::out);
    fout << "rep_id,mcmc_steps" << std::endl;
    fout.close();
}


template<typename T>
void run_switch_sampler()
{
    // Prepare the output files
    prepare_output_files<T>();

    // Make a collection of RNGs
    unsigned int num_threads = Config::global_config.get_num_threads();
    unsigned int num_reps = Config::global_config.get_switch_sampler_reps();
    RNGPool rngs = create(num_threads, Config::global_config.get_rng_seed());

    // Compute the number of batches
    unsigned int num_batches = num_reps/num_threads;
    if(num_reps%num_threads != 0)
        ++num_batches;

    for(unsigned int i=0; i<num_batches; ++i)
    {
        unsigned int first = num_threads*i + 1;
        unsigned int last  = num_threads*i + num_threads;
        if(last > num_reps)
            last = num_reps;
        rngs = do_batch<T>(first, last, rngs);
    }
}

} // namespace TwinPeaks2018

