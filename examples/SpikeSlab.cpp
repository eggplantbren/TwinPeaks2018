#include "SpikeSlab.h"
#include "Utils.h"
#include <sstream>

namespace TwinPeaks2018
{

SpikeSlab::SpikeSlab()
:xs(N)
{

}

void SpikeSlab::from_prior(RNG& rng)
{
    for(size_t i=0; i<xs.size(); i++)
        xs[i] = rng.rand();
}

double SpikeSlab::perturb(RNG& rng)
{
    int which = rng.rand_int(xs.size());
    xs[which] += rng.randh();
    wrap(xs[which], 0.0, 1.0);
    return 0.0;
}

double SpikeSlab::log_likelihood() const
{
    // A bunch of constants
    static constexpr double shift = 0.031;
    static constexpr double center = 0.5;
    static constexpr double u = 0.01;
    static constexpr double inv_u = 1.0/u;
    static constexpr double v = 0.1;
    static constexpr double inv_v = 1.0/v;
    static constexpr double log_u = log(u);
    static constexpr double log_v = log(v);
    static constexpr double C = log(1.0/sqrt(2.0*M_PI));
    static constexpr double log_half = log(0.5);

    double logl1 = 0.0;
    double logl2 = 0.0;
    for(double x: xs)
    {
        logl1 += C - log_u - 0.5*pow((x - center - shift)*inv_u, 2);
        logl2 += C - log_v - 0.5*pow((x - center)*inv_v, 2);
    }

    return logsumexp(logl1 + log_half, logl2 + log_half);
}

std::vector<double> SpikeSlab::scalars() const
{
    double value = log_likelihood();
    return { value };
}


std::ostream& operator << (std::ostream& out, const SpikeSlab& s)
{
    out << render(s.xs, false);
    return out;
}

std::string SpikeSlab::description()
{
    std::vector<std::string> colnames;
    for(size_t i=0; i<N; ++i)
    {
        std::stringstream ss;
        ss << "xs[" << i << "]";
        colnames.emplace_back(ss.str());
    }
    return render(colnames, false);
}


} // namespace TwinPeaks2018

