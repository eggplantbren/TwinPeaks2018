#ifndef TwinPeaks2018_Utils_h
#define TwinPeaks2018_Utils_h

#include <algorithm>
#include <cmath>
#include <cassert>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

// Useful functions, copied from DNest4

namespace TwinPeaks2018
{

// This is non-standard, gcc supports it but mingw64 doesn't,
// so putting it here
#ifndef M_PI
    constexpr double M_PI = 3.141592653589793;
#endif

double mod(double y, double x);
int mod(int y, int x);
void wrap(double& x, double min, double max);
double logsumexp(double* logv, int n);
double logsumexp(const std::vector<double>& logv);
double logsumexp(double a, double b);
double logdiffexp(double a, double b);

// Render a vector as a string
template<typename T>
std::string render(const std::vector<T>& vec, bool verbose=true)
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

// Argsort from
// http://stackoverflow.com/questions/1577475/c-sorting-and-keeping-track-of-indexes
template <typename T>
std::vector<size_t> argsort(const std::vector<T>& v)
{
	// initialize original index locations
	std::vector<size_t> idx(v.size());
	for(size_t i=0; i<idx.size(); i++)
		idx[i] = i;

	// sort indexes based on comparing values in v
	std::sort(idx.begin(), idx.end(),
		[&v](size_t i1, size_t i2) {return v[i1] < v[i2];});

	return idx;
}

// Calculate ranks
template <typename T>
std::vector<size_t> ranks(const std::vector<T>& v)
{
	// initialize original index locations
	std::vector<size_t> ii = argsort(v);
	std::vector<size_t> r(ii.size());

	for(size_t i=0; i<ii.size(); i++)
		r[ii[i]] = i;
	return r;
}

} // namespace TwinPeaks2018

#endif

