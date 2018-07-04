import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

def logsumexp(values):
	biggest = np.max(values)
	x = values - biggest
	result = np.log(np.sum(np.exp(x))) + biggest
	return result


def plot_settings():
    """
    Niceify plots
    """
    plt.rcParams["font.family"] = "serif"
    plt.rcParams["font.size"] = 16
    plt.rc("text", usetex=True)

def quantiles(xs):
    """
    Calculate some quantiles.
    """

    # Sort
    s = np.sort(xs)
    N = len(xs)

    probs = [0.0, 0.25, 0.50, 0.75, 0.999999]
    qs = []
    for prob in probs:
        qs.append(s[int(prob*N)])

    return qs

def load_particles_info(mcmc_steps_cutoff=0):
    """
    Load all the particles_info file, and return a data frame containing
    only complete runs (unless there aren't any, in which case it just
    returns whatever it's got).
    """

    particles_info = pd.read_csv("output/particles_info.csv")
    reps = pd.read_csv("output/reps.csv")
    print("Found {n} reps.".format(n=len(reps)))
    if mcmc_steps_cutoff > 0:
        print("Removing reps with mcmc_steps < {cutoff}"\
                .format(cutoff=mcmc_steps_cutoff), end="...")

        # Mark particles and reps as not good
        particles_good = np.zeros(particles_info.shape[0], dtype="bool")
        reps_good = np.zeros(reps.shape[0], dtype="bool")

        # Loop over reps
        for i in range(reps.shape[0]):

            # If it's a good rep, mark all its particles as good
            if reps.loc[i, "mcmc_steps"] >= mcmc_steps_cutoff:
                this_rep = particles_info["rep_id"] == reps["rep_id"][i]
                particles_good[this_rep] = True
                reps_good[i] = True

        # Nullify log-weights of bad reps
        particles_info.loc[particles_good, "ln_w"] = -1E300

        print("done.")
        print("Ended up with {N} good reps after applying mcmc_steps cutoff."\
                .format(N=np.sum(reps_good)))

    return particles_info

#  Old code for using only complete reps
#    if len(completed_reps) == 0:
#        print("There are no completed reps.")
#        return particles_info
#    # Mark completed reps for keeping
#    keep = np.zeros(particles_info.shape[0], dtype="bool")
#    for r in completed_reps:
#        keep[particles_info["rep_id"] == r] = True
#    indices = np.nonzero(keep)[0]
#    return particles_info.iloc[keep, :]

