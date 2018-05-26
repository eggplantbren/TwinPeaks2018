#include "SpikeSlab.h"
#include "Utils.h"

namespace TwinPeaks2018
{

SpikeSlab::SpikeSlab()
:xs(20)
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
    static constexpr double u = 0.01;
    static constexpr double v = 0.1;
    static constexpr double u_inv = 1.0/u;
    static constexpr double v_inv = 1.0/v;
    static constexpr double C = log(1.0/sqrt(2*M_PI));
    static constexpr double shift = 0.031;
    static constexpr double log_half = log(0.5);

    double logl1 = xs.size()*(C - log(u));
    double logl2 = xs.size()*(C - log(v));

    for(const double& x: xs)
    {
        logl1 += -0.5*pow((x - shift)*u_inv, 2);
        logl2 += -0.5*pow(x*v_inv, 2);
    }

    return logsumexp(logl1 + log_half, logl2 + log_half);
}

void SpikeSlab::print(std::ostream& out) const
{
    for(const double& x: xs)
        out << x << ' ';
}

std::string SpikeSlab::description() const
{
    return std::string("Each column is one of the 20 parameters.");
}

} // namespace TwinPeaks2018
