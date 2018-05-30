#ifndef TwinPeaks2018_TwoScalars_h
#define TwinPeaks2018_TwoScalars_h

#include "RNG.h"
#include <ostream>
#include <string>
#include <vector>

namespace TwinPeaks2018
{

class TwoScalars
{
    // Number of scalars
    public:
        static constexpr size_t num_scalars = 2;

    private:

        // Dimensionality
        static constexpr size_t N = 100;

        // The coordinates
        std::vector<double> xs;

        // The two scalars
        double f() const;
        double g() const;

    public:
        // Constructor only gives size of params
        TwoScalars();

        // Generate the point from the prior
        void from_prior(TwinPeaks2018::RNG& rng);

        // Metropolis-Hastings proposals
        double perturb(TwinPeaks2018::RNG& rng);

        // Scalars
        std::vector<double> scalars() const;

        // Return string with column information
        static std::string description();

        friend std::ostream& operator << (std::ostream& out,
                                          const TwoScalars& s);
};

std::ostream& operator << (std::ostream& out,
                           const TwoScalars& s);

} // namespace TwinPeaks2018

#endif

