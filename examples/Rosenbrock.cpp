#include "Rosenbrock.h"
#include "Utils.h"
#include <iostream>
#include <sstream>

namespace TwinPeaks2018
{

Rosenbrock::Rosenbrock()
:xs(N)
{

}

void Rosenbrock::from_prior(RNG& rng)
{
    for(size_t i=0; i<xs.size(); i++)
        xs[i] = -10.0 + 20.0*rng.rand();
}

double Rosenbrock::perturb(RNG& rng)
{
    int reps = 1;
    if(rng.rand() < 0.5)
        reps += static_cast<int>(pow(50.0, rng.rand()));

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
    double logL = 0.0;

    for(size_t i=0; i<(xs.size()-1); ++i)
    {
        logL -= 100*pow(xs[i+1] - xs[i]*xs[i], 2);
        logL -= pow(1.0 - xs[i], 2);
    }

    logL *= 2;

    return logL;
}

std::vector<double> Rosenbrock::scalars() const
{
    double value = log_likelihood();
    return { value };
}


std::ostream& operator << (std::ostream& out, const Rosenbrock& s)
{
    out << render(s.xs, false);
    return out;
}

std::string Rosenbrock::description()
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

