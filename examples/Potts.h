#ifndef TwinPeaks2018_Potts_h
#define TwinPeaks2018_Potts_h

#include <ostream>
#include <vector>
#include "RNG.h"

namespace TwinPeaks2018
{

class Potts
{
    public:
        static const size_t num_scalars = 2;

	private:
		static constexpr size_t num_colors = 5;
        static constexpr size_t n = 32;

        // State
		std::vector<std::vector<int>> x;
        std::vector<double> s_values;

		// So we can just update on the fly
		int score;
		int score2;

		void compute_score();
		void compute_scalars();

	public:

		Potts();
		void from_prior(RNG& rng);
		double perturb(RNG& rng);
        std::vector<double> scalars() const;

        static std::string description();

	friend std::ostream& operator << (std::ostream& out, const Potts& p);
};

} // namespace TwinPeaks2018

#endif

