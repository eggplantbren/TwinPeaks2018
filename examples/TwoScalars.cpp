#include "TwoScalars.h"
#include "Utils.h"
#include <sstream>

namespace TwinPeaks2018
{

TwoScalars::TwoScalars()
:xs(N)
{

}

void TwoScalars::from_prior(RNG& rng)
{
    for(size_t i=0; i<xs.size(); i++)
        xs[i] = rng.rand();
}

double TwoScalars::perturb(RNG& rng)
{
    int which;

    int num;
    if(rng.rand() <= 0.5)
        num = 1;
    else
        num = (int)pow((double)N, rng.rand());
    for(int i=0; i<num; ++i)
    {
        which = rng.rand_int(xs.size());
        xs[which] += rng.randh();
        wrap(xs[which], 0.0, 1.0);
    }

    return 0.0;
}

double TwoScalars::f() const
{
    double result = 0.0;
    for(double x: xs)
        result += -pow((x - 0.5)/0.1, 2);
    return result;
}

double TwoScalars::g() const
{
    double result = 0.0;
    for(double x: xs)
        result += -pow(sin(10*M_PI*x), 2);
    return result;
}

std::vector<double> TwoScalars::scalars() const
{
    return {f(), g()};
}


std::ostream& operator << (std::ostream& out, const TwoScalars& s)
{
    out << render(s.xs, false);
    return out;
}

std::string TwoScalars::description()
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

