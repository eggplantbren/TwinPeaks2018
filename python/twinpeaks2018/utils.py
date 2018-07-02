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

def load_particles_info():
    """
    Load all the particles_info file, and return a data frame containing
    only complete runs (unless there aren't any, in which case it just
    returns whatever it's got).
    """

    particles_info = pd.read_csv("output/particles_info.csv")
    reps = pd.read_csv("output/reps.csv")
    print("Found {n} reps.".format(n=len(reps)))
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

