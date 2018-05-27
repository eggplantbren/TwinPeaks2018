#ifndef TwinPeaks2018_Rosenbrock_h
#define TwinPeaks2018_Rosenbrock_h

#include "RNG.h"
#include <ostream>
#include <string>
#include <vector>

namespace TwinPeaks2018
{

class Rosenbrock
{
    private:

        // Dimensionality
        static constexpr size_t N = 50;

        // The coordinates
        std::vector<double> xs;

    public:
        // Constructor only gives size of params
        Rosenbrock();

        // Generate the point from the prior
        void from_prior(TwinPeaks2018::RNG& rng);

        // Metropolis-Hastings proposals
        double perturb(TwinPeaks2018::RNG& rng);

        // Likelihood function
        double log_likelihood() const;

        // Return string with column information
        static std::string description();

        friend std::ostream& operator << (std::ostream& out,
                                          const Rosenbrock& s);
};

std::ostream& operator << (std::ostream& out,
                           const Rosenbrock& s);

} // namespace TwinPeaks2018

#endif

