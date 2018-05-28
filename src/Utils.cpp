#include "Utils.h"
#include <iomanip>
#include <sstream>

namespace TwinPeaks2018
{

double mod(double y, double x)
{
    assert(x > 0.);
    return (y/x - floor(y/x))*x;
}

void wrap(double& x, double min, double max)
{
    x = TwinPeaks2018::mod(x - min, max - min) + min;
}

int mod(int y, int x)
{
    assert(x > 0);
    if(y >= 0)
        return y - (y/x)*x;
    else
        return (x-1) - TwinPeaks2018::mod(-y-1, x);
}

double logsumexp(double* logv, int n)
{
    assert(n > 1);
    double max = logv[0];
    for(int i=1; i<n; i++)
        if(logv[i] > max)
            max = logv[i];
    double answer = 0;
    // log(sum(exp(logf))   = log(sum(exp(logf - max(logf) + max(logf)))
    //          = max(logf) + log(sum(exp(logf - max(logf)))
    for(int i=0; i<n; i++)
        answer += exp(logv[i] - max);
    answer = max + log(answer);
    return answer;
}

double logsumexp(const std::vector<double>& logv)
{
    int n = static_cast<int>(logv.size());
    //if(n<=1)
    //  cout<<"Warning in logsumexp"<<endl;
    double max = *max_element(logv.begin(), logv.end());
    double answer = 0;
    // log(sum(exp(logf))   = log(sum(exp(logf - max(logf) + max(logf)))
    //          = max(logf) + log(sum(exp(logf - max(logf)))
    for(int i=0; i<n; i++)
        answer += exp(logv[i] - max);
    answer = max + log(answer);
    return answer;
}

double logsumexp(double a, double b)
{
    double x[2] = {a,b};
    return TwinPeaks2018::logsumexp(x, 2);
}

double logdiffexp(double a, double b)
{
    assert(a > b);
    double biggest = a;
    a -= biggest;
    b -= biggest;
    return log(exp(a) - exp(b)) + biggest;
}

std::string render(const std::vector<double>& vec, bool verbose)
{
    std::stringstream ss;
    ss << std::setprecision(12);

    if(verbose)
        ss << "[";
    for(size_t i=0; i<vec.size(); ++i)
    {
        ss << vec[i];
        if(i != (vec.size()-1))
            ss << ',' << ((verbose)?(" "):(""));
    }
    if(verbose)
        ss << ']';

    return ss.str();
}

} // namespace TwinPeaks2018

