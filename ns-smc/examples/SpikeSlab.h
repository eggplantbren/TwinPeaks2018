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
        std::string description() const;
};

} // namespace TwinPeaks2018

#endif

