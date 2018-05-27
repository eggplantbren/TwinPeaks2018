#ifndef TwinPeaks2018_SpikeSlab_h
#define TwinPeaks2018_SpikeSlab_h

#include "RNG.h"
#include <ostream>
#include <string>
#include <vector>

namespace TwinPeaks2018
{

class SpikeSlab
{
    private:

        // Dimensionality
        static constexpr size_t N = 20;

        // The coordinates
        std::vector<double> xs;

    public:
        // Constructor only gives size of params
        SpikeSlab();

        // Generate the point from the prior
        void from_prior(TwinPeaks2018::RNG& rng);

        // Metropolis-Hastings proposals
        double perturb(TwinPeaks2018::RNG& rng);

        // Likelihood function
        double log_likelihood() const;

        // Print to stream
        void print(std::ostream& out) const;

        // Return string with column information
        static std::string description();
};

} // namespace TwinPeaks2018

#endif

