#include "Demo.h"
#include "Utils.h"
#include <sstream>

namespace TwinPeaks2018
{

Demo::Demo()
:xs(N)
{

}

void Demo::from_prior(RNG& rng)
{
    for(size_t i=0; i<xs.size(); i++)
        xs[i] = rng.rand();
}

double Demo::perturb(RNG& rng)
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

double Demo::f() const
{
    double result = 0.0;
    for(double x: xs)
        result += -pow((x - 0.5)/0.1, 2);
    return result;
}

double Demo::g() const
{
    double result = 0.0;
    for(double x: xs)
        result += -x/0.1;
    return result;
}

std::vector<double> Demo::scalars() const
{
    return {f(), g()};
}


std::ostream& operator << (std::ostream& out, const Demo& s)
{
    out << render(s.xs, false);
    return out;
}

std::string Demo::description()
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

