#ifndef TwinPeaks2018_Rosenbrock_h
#define TwinPeaks2018_Rosenbrock_h

#include "RNG.h"
#include <ostream>
#include <string>
#include <vector>

namespace TwinPeaks2018
{

/*
* Show that if the two scalars are identical, you can obtain standard NS-SMC.
* Let all the scalars be (log likelihood)/num_scalars, and use temperatures
* of 1 for all scalars to get the posterior.
*/
class Rosenbrock
{
    // Number of scalars
    public:
        static constexpr size_t num_scalars = 2;

    private:

        // Dimensionality
        static constexpr size_t N = 50;

        // The coordinates
        std::vector<double> xs;

        // The log likelihood
        double log_likelihood() const;

    public:
        // Constructor only gives size of params
        Rosenbrock();

        // Generate the point from the prior
        void from_prior(TwinPeaks2018::RNG& rng);

        // Metropolis-Hastings proposals
        double perturb(TwinPeaks2018::RNG& rng);

        // Scalars
        std::vector<double> scalars() const;

        // Return string with column information
        static std::string description();

        friend std::ostream& operator << (std::ostream& out,
                                          const Rosenbrock& s);
};

std::ostream& operator << (std::ostream& out,
                           const Rosenbrock& s);

} // namespace TwinPeaks2018

#endif

