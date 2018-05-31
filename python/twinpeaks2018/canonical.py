"""
Use the output of a SwitchSampler run to get the properties
of a canonical distribution.
"""

import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
import pandas as pd
from twinpeaks2018.two_scalars_truth import compute_truth
from twinpeaks2018.utils import logsumexp


def load_particles_info():
    """
    Load all the particles_info file, and return a data frame containing
    only complete runs (unless there aren't any, in which case it just
    returns whatever it's got).
    """

    particles_info = pd.read_csv("output/particles_info.csv")
    completed_reps = np.loadtxt("output/completed_reps.txt")

    if len(completed_reps) == 0:
        print("There are no completed reps.")
        return particles_info

    print("Found {n} completed reps.\n".format(n=len(completed_reps)))

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

    plt.savefig("output/particle_scalars.png", dpi=600)
    print("Saved output/particle_scalars.png")
    plt.show()

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


#def create_canonical(particles_info,
#                     result, outfile="../../output/canonical_particles.csv"):
#    """
#    Create and save samples from the canonical distribution.
#    Argument: a dictionary as output by get_canonical().
#    """
#    ln_W = result["ln_W"]
#    max_ln_W = np.max(ln_W)

#    # Indices of particles
#    indices = []
#    attempts = 0
#    while True:
#        attempts += 1
#        print(attempts)

#        k = rng.randint(len(ln_W))
#        p = np.exp(ln_W[k] - max_ln_W)
#        if rng.rand() <= p:
#            indices.append(k)
#        if len(indices) >= int(result["ESS"]):
#            break

#    indices = np.sort(np.array(indices))


def evaluate_temperature_grid(particles_info, limits, n=51):
    """
    Evaluate logZ and H on a temperature grid.
    """
    T0_min, T0_max, T1_min, T1_max = limits

    # Set up results grids
    ln_Z = np.empty((n, n))
    H = np.empty((n, n))
    S0 = np.empty((n, n))
    S1 = np.empty((n, n))

    print("Computing temperature grid. It takes a while (one dot=one pixel)",
          end="", flush=True)

    # Temperature grids
    T0 = 10.0**np.linspace(np.log10(T0_min),
                                        np.log10(T0_max), ln_Z.shape[0])
    T1 = 10.0**np.linspace(np.log10(T0_min),
                                        np.log10(T0_max), ln_Z.shape[1])
    [T0, T1] = np.meshgrid(T0, T1)

    # Compute over the grid
    for i in range(ln_Z.shape[0]):
        for j in range(ln_Z.shape[1]):
            results = get_canonical(particles_info,
                                    temperatures=[T0[i, j], T1[i, j]])

            # Retrieve normalisation constant and KL divergence
            ln_Z[i, j] = results["ln_Z"]
            H[i, j] = results["H"]

            # Compute expectations
            W = np.exp(results["ln_W"])
            S0[i, j] = np.sum(W*particles_info["scalars[0]"])
            S1[i, j] = np.sum(W*particles_info["scalars[1]"])
            print(".", end="", flush=True)
    print("")

    truth = compute_truth(limits, n)

    # Plot the results
    plt.figure(1, figsize=(8, 8))
    plt.subplot(2, 2, 1)
    plt.imshow(ln_Z, origin="lower", extent=np.log10(limits))
    plt.ylabel("log10(T_1)")
    plt.title("ln_Z")

    plt.subplot(2, 2, 2)
    plt.imshow(H, origin="lower", extent=np.log10(limits))
    plt.ylabel("log10(T_1)")
    plt.title("H")

    plt.subplot(2, 2, 3)
    plt.imshow(ln_Z - truth["ln_Z"],
               origin="lower", extent=np.log10(limits), cmap="coolwarm")
    plt.xlabel("log10(T_0)")
    plt.ylabel("log10(T_1)")
    plt.title("Residuals")

    plt.subplot(2, 2, 4)
    plt.imshow(H - truth["H"],
               origin="lower", extent=np.log10(limits), cmap="coolwarm")
    plt.xlabel("log10(T_0)")
    plt.ylabel("log10(T_1)")
    plt.title("Residuals")

    plt.savefig("output/ln_Z_H.png", dpi=600)
    print("Saved output/ln_Z_h.png")
    plt.show()

    plt.figure(2)
    plt.subplot(1, 2, 1)
    plt.imshow(S0, origin="lower", extent=np.log10(limits))
    plt.xlabel("log10(T_0)")
    plt.ylabel("log10(T_1)")
    plt.title("<S_0>")

    plt.subplot(1, 2, 2)
    plt.imshow(S1, origin="lower", extent=np.log10(limits))
    plt.xlabel("log10(T_0)")
    plt.ylabel("log10(T_1)")
    plt.title("<S_1>")
    plt.savefig("output/expectations_of_scalars.png", dpi=600)
    print("Saved output/expectations_of_scalars.png")
    plt.show()



def showresults():
    """
    This is what you'll usually want to call.
    """

    # Load the results
    particles_info = load_particles_info()

    # Compute the properties of a canonical distribution.
    result = get_canonical(particles_info)

    print("ln(Z) = {ln_Z}.".format(ln_Z=result["ln_Z"]))
    print("H = {H} nats.".format(H=result["H"]))
    print("ESS = {ESS}.\n".format(ESS=result["ESS"]))

    print("For the example, the true value of ln(Z) is " + \
          "-216.865, and H is 129.017 nats.\n")

    plot_particle_scalars(particles_info)
    evaluate_temperature_grid(particles_info, [0.1, 100.0, 0.1, 100.0])

