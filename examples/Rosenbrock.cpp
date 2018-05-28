#include "Rosenbrock.h"
#include "Utils.h"
#include <sstream>

namespace TwinPeaks2018
{

Rosenbrock::Rosenbrock()
:xs(N)
{

}

void Rosenbrock::from_prior(RNG& rng)
{
    for(double& x: xs)
        x = -10.0 + 20.0*rng.rand();
}


double Rosenbrock::perturb(RNG& rng)
{
    int reps = 1;
    if(rng.rand() < 0.5)
        reps += static_cast<int>(pow(100.0, rng.rand()));

    int which;
    for(int i=0; i<reps; ++i)
    {
        which = rng.rand_int(xs.size());
        xs[which] += 20.0*rng.randh();
        wrap(xs[which], -10.0, 10.0);
    }

	return 0.0;
}

double Rosenbrock::log_likelihood() const
{
    double logl = 0.0;

    for(size_t i=0; i<(xs.size()-1); ++i)
    {
        logl -= 100*pow(xs[i+1] - xs[i]*xs[i], 2);
        logl -= pow(1.0 - xs[i], 2);
    }
    logl *= 2;

    return logl;
}

std::ostream& operator << (std::ostream& out, const Rosenbrock& r)
{
    for(size_t i=0; i<r.xs.size(); ++i)
    {
        out << r.xs[i];
        if(i != (r.xs.size() - 1))
            out << ',';
    }
    return out;
}

std::string Rosenbrock::description()
{
    std::stringstream ss;
    for(size_t i=0; i<N; ++i)
    {
        ss << "xs[" << i << ']';
        if(i != (N - 1))
            ss << ',';
    }
    return ss.str();
}

} // namespace TwinPeaks2018

