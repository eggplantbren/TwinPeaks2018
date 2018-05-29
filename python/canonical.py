"""
Use the output of a SwitchSampler run to get the properties
of a canonical distribution.
"""

import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
import pandas as pd
from utils import logsumexp


def load_particles_info(filename="../output/particles_info.csv"):
    """
    Load all the particles_info file, and return a data frame containing
    only complete runs (unless there aren't any, in which case it just
    returns whatever it's got).
    """

    particles_info = pd.read_csv(filename)

    completed_reps = np.loadtxt("../output/completed_reps.txt")
    if len(completed_reps) == 0:
        print("There are no completed reps.")
        return particles_info

    print("Found {n} completed reps.".format(n=len(completed_reps)))

    # Mark completed reps for keeping
    keep = np.zeros(particles_info.shape[0], dtype="bool")
    for r in completed_reps:
        keep[particles_info["run_id"] == r] = True
    indices = np.nonzero(keep)[0]
    return particles_info.iloc[keep, :]


def plot_particle_scalars(particles_info, scalars=[0, 1]):
    """
    Plot particle scalars
    """
    assert len(scalars)==2

    # Thin if the data frame is big
    thin = 1
    if particles_info.shape[0] > 30000:
        thin = particles_info.shape[0] // 30000

    # Use random thinning to avoid putting artifacts into the plot
    indices = rng.randint(particles_info.shape[0], size=30000)

    names = ["scalars[{i}]".format(i=scalars[0]),
             "scalars[{j}]".format(j=scalars[1])]

    h = plt.figure()
    plt.plot(particles_info[names[0]][indices],
             particles_info[names[1]][indices],
             ".", alpha=0.2, markersize=1)
    plt.xlabel(names[0])
    plt.ylabel(names[1])
    return h



def get_canonical(particles_info, temperatures=[1.0, 1.0], plot=False):
    """
    Obtain the properties of a single canonical distribution.
    """
    rep_ids = np.arange(min(particles_info["run_id"]),
                        max(particles_info["run_id"] + 1))

    ln_w = particles_info["ln_prior_mass"]\
                   - logsumexp(particles_info["ln_prior_mass"])
    ln_s = np.zeros(particles_info.shape[0])
    for i in range(len(temperatures)):
        ln_s += particles_info["scalars[{i}]".format(i=i)]/temperatures[i]

    ln_prod = ln_w + ln_s
    ln_Z = logsumexp(ln_prod)
    ln_W = ln_prod - ln_Z
    W = np.exp(ln_W)

    result = {}
    result["ln_Z"] = ln_Z
    result["ln_W"] = ln_W
    result["H"] = np.sum(W*(ln_s - ln_Z))
    result["ESS"] = np.exp(-np.sum(W*ln_W))

    if plot:
        plt.plot(logW)
        plt.show()

    return result

def create_canonical(particles_info,
                     result, outfile="../output/canonical_particles.csv"):
    """
    Create and save samples from the canonical distribution.
    Argument: a dictionary as output by get_canonical().
    """
    ln_W = result["ln_W"]
    max_ln_W = np.max(ln_W)

    # Indices of particles
    indices = []
    attempts = 0
    while True:
        attempts += 1
        print(attempts)

        k = rng.randint(len(ln_W))
        p = np.exp(ln_W[k] - max_ln_W)
        if rng.rand() <= p:
            indices.append(k)
        if len(indices) >= int(result["ESS"]):
            break

    indices = np.sort(np.array(indices))


def evaluate_temperature_grid(particles_info, T_min, T_max):
    """
    Evaluate logZ and H on a temperature grid.
    """
    ln_Z = np.empty((51, 51))
    H = np.empty((51, 51))

    T0 = 10.0**np.linspace(np.log10(T_min[0]),
                                        np.log10(T_max[0]), ln_Z.shape[0])
    T1 = 10.0**np.linspace(np.log10(T_min[1]),
                                        np.log10(T_max[1]), ln_Z.shape[1])
    [T0, T1] = np.meshgrid(T0, T1)

    for i in ln_Z.shape[0]:
        for j in ln_Z.shape[1]:
            results = get_canonical(particles_info,
                                    temperatures=[T0[i, j], T1[i, j]])
            ln_Z[i, j] = results["ln_Z"]
            H[i, j] = results["H"]

    plt.imshow(ln_Z, origin="lower")
    plt.show()


if __name__ == "__main__":

    # Load the results
    particles_info = load_particles_info()

    # Compute the properties of a canonical distribution.
    result = get_canonical(particles_info)

    print("ln(Z) = {ln_Z}.".format(ln_Z=result["ln_Z"]))
    print("H = {H} nats.".format(H=result["H"]))
    print("ESS = {ESS}.".format(ESS=result["ESS"]))

    print("\nFor the example, the true value of ln(Z) is " + \
          "-8073.72, and H is 63.7246 nats.")

    h = plot_particle_scalars(particles_info)
    plt.show()

