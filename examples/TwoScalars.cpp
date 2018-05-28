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
    int which = rng.rand_int(xs.size());
    xs[which] += rng.randh();
    wrap(xs[which], 0.0, 1.0);
    return 0.0;
}

double TwoScalars::f() const
{
    static constexpr double center = 0.3;
    static constexpr double width = 0.01;
    static constexpr double inv_width = 1.0/width;

    double result = 0.0;
    for(double x: xs)
        result += -0.5*pow((x - center)*inv_width, 2);

    return result;
}

double TwoScalars::g() const
{
    static constexpr double width = 0.01;
    static constexpr double inv_width = 1.0/width;

    double result = 0.0;
    for(double x: xs)
        result += -x*inv_width;

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

