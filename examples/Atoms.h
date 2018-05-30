#ifndef TwinPeaks2018_Atoms_h
#define TwinPeaks2018_Atoms_h

#include <ostream>
#include <string>
#include <vector>
#include "RNG.h"

namespace TwinPeaks2018
{

class Atoms
{
    public:
        static constexpr size_t num_scalars = 2;

    private:
        static constexpr int num_atoms = 30;
        static constexpr double L = 100.0;

        // Positions
        std::vector<double> x, y, z;

    public:
        Atoms();

        // Generate the point from the prior
        void from_prior(RNG& rng);

        // Metropolis-Hastings proposals
        double perturb(RNG& rng);

        // Getter
        std::vector<double> scalars() const;

        // For CSV header
        static std::string description();

        friend std::ostream& operator << (std::ostream& out, const Atoms& a);
};

// Output to stream
std::ostream& operator << (std::ostream& out, const Atoms& a);

} // namespace TwinPeaks2018

#endif

