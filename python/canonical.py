"""
Use the output of a SwitchSampler run to get the properties
of a canonical distribution.
"""

import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
import pandas as pd
from utils import logsumexp

# Load and plot trajectories
particles_info = pd.read_csv("../output/particles_info.csv")

def plot_particle_scalars(scalars=[0, 1]):
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



def get_canonical(temperatures=[1.0, 1.0], plot=False):
    """
    Obtain the properties of a single canonical distribution.
    """

    subset = particles_info # Might need to remove incomplete reps though

    rep_ids = np.arange(min(particles_info["run_id"]),
                        max(particles_info["run_id"] + 1))

    print("Found {reps} switch sampler reps."\
                    .format(reps=np.max(rep_ids)))

    ln_w = subset["ln_prior_mass"]\
                   - logsumexp(subset["ln_prior_mass"])
    ln_s = np.zeros(subset.shape[0])
    for i in range(len(temperatures)):
        ln_s += subset["scalars[{i}]".format(i=i)]/temperatures[i]

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

def create_canonical(result, outfile="../output/canonical_particles.csv"):
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



if __name__ == "__main__":
    result = get_canonical()
    print("ln(Z) = {ln_Z}.".format(ln_Z=result["ln_Z"]))
    print("H = {H} nats.".format(H=result["H"]))
    print("ESS = {ESS}.".format(ESS=result["ESS"]))

    print("\nFor the example, the true value of ln(Z) is " + \
          "-8073.72, and H is 63.7246 nats.")

    h = plot_particle_scalars()
    plt.show()

