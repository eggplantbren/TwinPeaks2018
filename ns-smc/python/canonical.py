"""
Use the output of a SwitchSampler run to get the properties
of a canonical distribution.
"""

import dnest4.classic as dn4
import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
import pandas as pd

# Load and plot trajectories
particles_info = pd.read_csv("../output/particles_info.csv")

def plot_trajectories():
    """
    Plot particle gs vs. fs
    """

    # Thin if the data frame is big
    thin = 1
    if particles_info.shape[0] > 100000:
        thin = particles_info.shape[0] // 100000

    indices = np.arange(0, particles_info.shape[0], step=thin)

    h = plt.figure()
    plt.plot(particles_info["f"][indices],
             particles_info["g"][indices], ".", alpha=0.2, markersize=1)
    plt.xlabel("$f$")
    plt.ylabel("$g$")
    return h



def get_canonical(temperatures=[1.0, 1.0], truncate=True, plot=False):
    """
    Obtain the properties of a single canonical distribution.
    """

    run_ids = np.arange(min(particles_info["run_id"]),
                        max(particles_info["run_id"] + 1))
    if truncate and len(run_ids) > 1:
        if np.sum(particles_info["run_id"] == run_ids[0]) != \
           np.sum(particles_info["run_id"] == run_ids[-1]):
            which = np.nonzero(particles_info["run_id"] != run_ids[-1])[0]
            subset = particles_info.iloc[which, :]
        run_ids = run_ids[0:-1]
    else:
        subset = particles_info

    print("Found {reps} switch sampler reps."\
                    .format(reps=run_ids[-1]))

    ln_w = subset["ln_prior_mass"]\
                   - dn4.logsumexp(subset["ln_prior_mass"])
    ln_s = subset["f"]/temperatures[0]\
            + subset["g"]/temperatures[1]
    ln_prod = ln_w + ln_s
    ln_Z = dn4.logsumexp(ln_prod)
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
          "-3318.62, and H is 318.623 nats.")

    h = plot_trajectories()
    plt.show()


