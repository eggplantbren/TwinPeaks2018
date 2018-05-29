#include "Potts.h"
#include "RNG.h"
#include "Utils.h"
#include <string>
#include <sstream>

namespace TwinPeaks2018
{

Potts::Potts()
:x(n, std::vector<int>(n))
,s_values(num_scalars)
{

}

void Potts::from_prior(RNG& rng)
{
	for(size_t i=0; i<x.size(); i++)
		for(size_t j=0; j<x[i].size(); j++)
			x[i][j] = rng.rand_int(num_colors);

	compute_score();
	compute_scalars();
}

void Potts::compute_score()
{
	score = 0;
	score2 = 0;

	// Coordinates of neighbouring cells
	std::vector<int> ii(4);
    std::vector<int> jj(4);

	for(size_t i=0; i<x.size(); i++)
	{
		for(size_t j=0; j<x[i].size(); j++)
		{
			for(int k=0; k<4; k++)
			{
				ii[k] = i;
				jj[k] = j;
			}

			// Down, up, right, left
			ii[0] = mod(i + 1, static_cast<int>(x.size()));
			ii[1] = mod(i - 1, static_cast<int>(x.size()));
			jj[2] = mod(j + 1, static_cast<int>(x[i].size()));
			jj[3] = mod(j - 1, static_cast<int>(x[i].size()));

			for(int k=0; k<4; k++)
			{
				if(x[i][j] == x[ii[k]][jj[k]])
				{
					score++;
					if(k >= 2)
						score2++;
				}
			}
		}
	}
}


double Potts::perturb(RNG& rng)
{
	int reps = 1;
	if(rng.rand() <= 0.5)
		reps += 1 + rng.rand_int(9);

	// Which cell is being perturbed
	int i, j;

	// Coordinates of neighbouring cells
	std::vector<int> ii(4);
    std::vector<int> jj(4);
	for(int z=0; z<reps; z++)
	{
		i = rng.rand_int(x.size());
		j = rng.rand_int(x[0].size());

		for(int k=0; k<4; k++)
		{
			ii[k] = i;
			jj[k] = j;
		}

		// Down, up, right, left
		ii[0] = mod(i + 1, static_cast<int>(x.size()));
		ii[1] = mod(i - 1, static_cast<int>(x.size()));
		jj[2] = mod(j + 1, static_cast<int>(x[i].size()));
		jj[3] = mod(j - 1, static_cast<int>(x[i].size()));

		// Calculate negative part of delta score
		for(int k=0; k<4; k++)
		{
			if(x[i][j] == x[ii[k]][jj[k]])
			{
				score--;
				if(k >= 2)
					score2--;
			}
		}

		// Perturb the cell
        auto orig = x[i][j];
        do
        {
    		x[i][j] = rng.rand_int(num_colors);
        }while(x[i][j] == orig);

		// Calculate positive part of delta score
		for(int k=0; k<4; k++)
		{
			if(x[i][j] == x[ii[k]][jj[k]])
			{
				++score;
				if(k >= 2)
					++score2;
			}
		}
	}

	compute_scalars();
	return 0.0;
}

void Potts::compute_scalars()
{
	s_values[0] = 0.5*score;
	s_values[1] = 0.5*score2;
}

std::vector<double> Potts::scalars() const
{
    return s_values;
}

std::string Potts::description()
{
    std::vector<std::string> values;
	for(size_t i=0; i<n; i++)
		for(size_t j=0; j<n; j++)
        {
            std::stringstream s;
            s << "x[" << i << "][" << j << ']';
            values.emplace_back(s.str());
        }
    return render(values, false);    
}

std::ostream& operator << (std::ostream& out, const Potts& e)
{
    std::vector<int> values;
	for(size_t i=0; i<e.x.size(); i++)
		for(size_t j=0; j<e.x[i].size(); j++)
			values.push_back(e.x[i][j]);
    out << render(values, false);
	return out;
}

} // namespace TwinPeaks2018

