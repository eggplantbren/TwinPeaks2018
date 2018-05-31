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

def load_particles_info():
    """
    Load all the particles_info file, and return a data frame containing
    only complete runs (unless there aren't any, in which case it just
    returns whatever it's got).
    """

    particles_info = pd.read_csv("output/particles_info.csv")
    completed_reps = np.array([np.loadtxt("output/completed_reps.txt")])
    completed_reps = completed_reps.flatten()

    if len(completed_reps) == 0:
        print("There are no completed reps.")
        return particles_info

    print("Found {n} completed reps.\n".format(n=len(completed_reps)))

    # Mark completed reps for keeping
    keep = np.zeros(particles_info.shape[0], dtype="bool")
    for r in completed_reps:
        keep[particles_info["rep_id"] == r] = True
    indices = np.nonzero(keep)[0]
    return particles_info.iloc[keep, :]
