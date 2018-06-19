"""
Use the output of a SwitchSampler run to get the properties
of a canonical distribution.
"""

import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
import pandas as pd
from twinpeaks2018.demo_truth import *
from twinpeaks2018.utils import *

plot_settings()

def plot_particle_scalars(particles_info, scalars=[0, 1]):
    """
    Plot particle scalars
    """
    assert len(scalars)==2

    # Thin if the data frame is big
    thin = 1
    if particles_info.shape[0] > 30000:
        thin = particles_info.shape[0] // 30000

    # Use random thinning to avoid putting aliasing artifacts into the plot
    indices = rng.randint(particles_info.shape[0], size=30000)

    names = ["scalars[{i}]".format(i=scalars[0]),
             "scalars[{j}]".format(j=scalars[1])]

    plt.figure()
    plt.plot(particles_info[names[0]][indices],
             particles_info[names[1]][indices],
             ".", alpha=0.2, markersize=3)
    plt.xlabel("$S_{i}$".format(i=scalars[0]))
    plt.ylabel("$S_{j}$".format(j=scalars[1]))
    x = np.sort(particles_info[names[0]][indices])
    y = np.sort(particles_info[names[1]][indices])
    plt.xlim([x[int(0.05*len(x))], x[-1]])
    plt.ylim([y[int(0.05*len(y))], y[-1]])

    plt.savefig("output/particle_scalars.png", dpi=600)
    print("Saved output/particle_scalars.png")
    plt.show()




def get_canonical(particles_info, temperatures=[1.0, 1.0], plot_and_save=False):
    """
    Obtain the properties of a single canonical distribution.
    """
    rep_ids = np.arange(min(particles_info["rep_id"]),
                        max(particles_info["rep_id"] + 1))

    ln_w = particles_info["ln_prior_mass"]\
                   - logsumexp(particles_info["ln_prior_mass"])
    ln_s = np.zeros(particles_info.shape[0])
    for i in range(len(temperatures)):
        ln_s += particles_info["scalars[{i}]".format(i=i)]/temperatures[i]

    ln_prod = ln_w + ln_s
    ln_Z = logsumexp(ln_prod)
    ln_W = ln_prod - ln_Z
    W = np.exp(ln_W)
    H = np.sum(W*(ln_s - ln_Z))

    result = {}
    result["ln_Z"] = ln_Z
    result["ln_W"] = ln_W
    result["H"] = H
    result["ESS"] = np.exp(-np.sum(W*ln_W))

    # Calculate effective number of reps
    ln_W_rep = np.empty(len(rep_ids))
    k = 0
    for rep_id in rep_ids:
        subset = ln_W[particles_info["rep_id"] == rep_id]
        ln_W_rep[k] = logsumexp(subset)
        k += 1
    W_rep = np.exp(ln_W_rep)
    result["ENR"] = np.exp(-np.sum(W_rep*ln_W_rep))

    if plot_and_save:
        print("ln(Z) = {ln_Z}".format(ln_Z=ln_Z))
        print("H = {H} nats".format(H=H))
        print("Effective sample size = {ESS}".format(ESS=result["ESS"]))
        print("Effective number of reps = {ENR}".format(ENR=result["ENR"]))
        np.savetxt("output/canonical_weights.txt", W)

        plt.figure(figsize=(9, 7))
        plt.subplot(2, 1, 1)
        plt.plot(ln_w, ln_s, ".", markersize=3, alpha=0.5)
        ln_s_sorted = np.sort(ln_s)
        plt.ylim([ln_s_sorted[int(0.05*len(ln_s_sorted))], ln_s_sorted[-1]])
        plt.ylabel("$\\ln(L)$")

        plt.subplot(2,1,2)
        plt.plot(ln_w, W/W.max(), ".", markersize=3, alpha=0.5)
        plt.xlabel("$\\ln(w)$")
        plt.ylabel("$W/W_{\\rm max}$")
        plt.savefig("output/likelihood_curve.png", dpi=600)
        print("Saved output/likelihood_curve.png")
        plt.show()

    return result


def evaluate_temperature_grid(particles_info, limits, n=51, residuals=False):
    """
    Evaluate logZ and H on a temperature grid.
    """
    T0_min, T0_max, T1_min, T1_max = limits

    # Set up results grids
    ln_Z = np.empty((n, n))
    H = np.empty((n, n))
    S0 = np.empty((n, n))
    S1 = np.empty((n, n))
    ENR = np.empty((n, n))

    print("Computing temperature grid. It takes a while (one dot=one pixel)",
          end="", flush=True)

    # Temperature grids
    T0 = 10.0**np.linspace(np.log10(T0_min),
                                        np.log10(T0_max), ln_Z.shape[0])
    T1 = 10.0**np.linspace(np.log10(T1_min),
                                        np.log10(T1_max), ln_Z.shape[1])
    [T0, T1] = np.meshgrid(T0, T1)

    # Compute over the grid
    for i in range(ln_Z.shape[0]):
        for j in range(ln_Z.shape[1]):
            results = get_canonical(particles_info,
                                    temperatures=[T0[i, j], T1[i, j]])

            # Retrieve normalisation constant and KL divergence
            ln_Z[i, j] = results["ln_Z"]
            H[i, j] = results["H"]
            ENR[i, j] = results["ENR"]

            # Compute expectations
            W = np.exp(results["ln_W"])
            S0[i, j] = np.sum(W*particles_info["scalars[0]"])
            S1[i, j] = np.sum(W*particles_info["scalars[1]"])
            print(".", end="", flush=True)
    print("")

    # Plot the results
    plt.figure(1, figsize=(9, 6))
    plt.subplot(1, 2, 1)
    plt.imshow(ln_Z, origin="lower", extent=np.log10(limits))
    plt.xlabel("$\\log_{10}(T_0)$")
    plt.ylabel("$\\log_{10}(T_1)$")
    plt.title("$\\ln(Z)$")

    plt.subplot(1, 2, 2)
    plt.imshow(H, origin="lower", extent=np.log10(limits))
    plt.xlabel("$\\log_{10}(T_0)$")
    plt.title("$H$")

    plt.savefig("output/ln_Z_H.png", dpi=600)
    print("Saved output/ln_Z_h.png")

    plt.figure(2)
    plt.imshow(ENR, origin="lower", extent=np.log10(limits))
    plt.xlabel("$\\log_{10}(T_0)$")
    plt.ylabel("$\\log_{10}(T_1)$")
    plt.title("Effective number of contributing reps")
    plt.savefig("output/ENR.png", dpi=600)
    print("Saved output/ENR.png")
    plt.show()

    if residuals:
        truth = compute_truth(limits, n)

        plt.figure(2, figsize=(9, 6))
        plt.subplot(1, 2, 1)
        resid = ln_Z - truth["ln_Z"]
        biggest = np.max(np.abs(resid))
        plt.imshow(resid,
                   origin="lower", extent=np.log10(limits),
                   vmin=-biggest, vmax=biggest, cmap="coolwarm")
        plt.xlabel("$\\log_{10}(T_0)$")
        plt.ylabel("$\\log_{10}(T_1)$")
        plt.title("$\\ln(Z)$ Residuals")

        plt.subplot(1, 2, 2)
        resid = H - truth["H"]
        biggest = np.max(np.abs(resid))
        plt.imshow(resid,
                   origin="lower", extent=np.log10(limits),
                   vmin=-biggest, vmax=biggest, cmap="coolwarm")
        plt.xlabel("$\\log_{10}(T_0)$")
        plt.title("$H$ Residuals")

        plt.savefig("output/residuals1.png", dpi=600)
        print("Saved output/residuals1.png")
        plt.show()

        plt.figure(3) #, figsize=(9, 6))
#        plt.subplot(1, 2, 1)
        resid = ln_Z - truth["ln_Z"]
        Neff = truth["H"]/resid**2
        Neff[Neff < 1.0] = np.nan
        plt.imshow(np.log10(Neff), origin="lower", extent=np.log10(limits))
        plt.xlabel("$\\log_{10}(T_0)$")
        plt.ylabel("$\\log_{10}(T_1)$")
        plt.title("$\\log_{10}(N_{\\rm eff})$ based on $\\ln(Z)$ Residuals")

#        plt.subplot(1, 2, 2)
#        resid = H - truth["H"]
#        Neff = truth["H"]/resid**2
#        Neff[Neff < 1.0] = np.nan
#        plt.imshow(np.log10(Neff), origin="lower", extent=np.log10(limits))
#        plt.xlabel("$\\log_{10}(T_0)$")
#        plt.title("$\\log_{10} N_{\\rm eff}$ based on $H$ Residuals")

#        plt.savefig("output/residuals2.png", dpi=600)
#        print("Saved output/residuals2.png")
        plt.show()

    plt.figure(3, figsize=(9, 6))
    plt.subplot(1, 2, 1)
    plt.imshow(S0, origin="lower", extent=np.log10(limits))
    plt.xlabel("$\\log_{10}(T_0)$")
    plt.ylabel("$\\log_{10}(T_1)$")
    plt.title("$\\left<S_0\\right>$")

    plt.subplot(1, 2, 2)
    plt.imshow(S1, origin="lower", extent=np.log10(limits))
    plt.xlabel("$\\log_{10}(T_0)$")
    plt.title("$\\left<S_1\\right>$")
    plt.savefig("output/expectations_of_scalars.png", dpi=600)
    print("Saved output/expectations_of_scalars.png")
    plt.show()



def postprocess_two_scalars(specific_temperatures=[1.0, 1.0],
                            temperature_grid_limits=[0.1, 100.0, 0.1, 100.0],
                            demo=False):
    """
    This is what you'll usually want to call. Use specific_temperatures
    to select a canonical distribution of particular interest, and
    use temperature_grid_limits to select a range of temperatures over
    which the grids will be made.
    """

    # Load the results
    particles_info = load_particles_info()

    # Compute the properties of a canonical distribution and the requested
    # temperatures.
    result = get_canonical(particles_info, specific_temperatures, True)

    if demo:
        print("For the demo example, the true value of ln(Z) is " + \
              "-216.865, and H is 129.017 nats.\n")

    plot_particle_scalars(particles_info)
    evaluate_temperature_grid(particles_info, limits=temperature_grid_limits,
                              residuals=demo)

