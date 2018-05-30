#include "Atoms.h"
#include "Utils.h"
#include <cmath>
#include <sstream>

namespace TwinPeaks2018
{

Atoms::Atoms()
:x(num_atoms), y(num_atoms), z(num_atoms)
{

}

void Atoms::from_prior(RNG& rng)
{
    for(size_t i=0; i<x.size(); i++)
    {
        x[i] = L*rng.rand();
        y[i] = L*rng.rand();
        z[i] = L*rng.rand();
    }
}

std::vector<double> Atoms::scalars() const
{
    // Scalars
    std::vector<double> ss(2, 0.0);
    for(int i=0; i<num_atoms; ++i)
    {
        for(int j=(i+1); j<num_atoms; ++j)
        {
            double rsq = pow(x[i] - x[j], 2) + pow(z[i] - z[j], 2);
            ss[0] += -4*(pow(1./rsq, 6) - 2.0*pow(1.0/rsq, 3));
        }
    }

    // Polymer potential
    double k = 36*pow(2.0, 2.0/3);
    double c = pow(2.0, 1.0/6);
    double r;
    // First sum
    for(int i=0; i<(num_atoms-1); ++i)
    {
        r = sqrt(pow(x[i] - x[i+1], 2) + pow(y[i] - y[i+1], 2)
                        + pow(z[i] - z[i+1], 2));
        ss[1] += -0.5*k*pow(r - c, 2);
    }

    // Second sum
    double dotprod;
    for(int i=1; i<(num_atoms-1); ++i)
    {
        dotprod = (x[i] - x[i-1])*(x[i+1] - x[i])
                    + (y[i] - y[i-1])*(y[i+1] - y[i])
                    + (z[i] - z[i-1])*(z[i+1] - z[i]);
        ss[1] += -0.5*k*pow(dotprod - 1., 2);
    }

    return ss;
}

double Atoms::perturb(RNG& rng)
{
    int reps = 1;
    if(rng.rand() <= 0.5)
        reps = (int)pow((double)num_atoms*3, rng.rand());

    for(int rep=0; rep<reps; ++rep)
    {
        int i = rng.rand_int(num_atoms);

        int which_coord = rng.rand_int(3);
        double* coord = nullptr;
        if(which_coord == 0)
            coord = &(x[i]);
        else if(which_coord == 1)
            coord = &(y[i]);
        else
            coord = &(z[i]);

        *coord += L*rng.randh();
        wrap(*coord, 0.0, L);
    }

    return 0.0;
}


std::ostream& operator << (std::ostream& out, const Atoms& a)
{
    for(size_t i=0; i<a.x.size(); ++i)
        out << a.x[i] << ",";

    for(size_t i=0; i<a.y.size(); ++i)
        out << a.y[i] << ",";

    for(size_t i=0; i<a.z.size(); ++i)
    {
        out << a.z[i];
        if(i != a.z.size() - 1)
            out << ',';
    }

    return out;
}

std::string Atoms::description()
{
    std::vector<std::string> parts;
    std::stringstream s;

    for(size_t i=0; i<num_atoms; ++i)
    {
        s.str("");
        s << "x[" << i << "],";
        parts.emplace_back(s.str());
    }
    for(size_t i=0; i<num_atoms; ++i)
    {
        s.str("");
        s << "y[" << i << "],";
        parts.emplace_back(s.str());
    }
    for(size_t i=0; i<num_atoms; ++i)
    {
        s.str("");
        s << "z[" << i << "],";
        parts.emplace_back(s.str());
    }

    return render(parts, false);
}

} // namespace TwinPeaks2018

