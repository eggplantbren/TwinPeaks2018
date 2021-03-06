import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
import pandas as pd
from twinpeaks2018.utils import *

plot_settings()

def postprocess_one_scalar(mcmc_steps_cutoff=0, temperature=1.0):
    """
    Postprocessing for one scalar
    """
    particles_info = load_particles_info(mcmc_steps_cutoff)
    ln_w = particles_info["ln_prior_mass"]
    ln_l = particles_info["scalars[0]"]

    # Renormalise prior weights, calculate evidence
    ln_w = ln_w - logsumexp(ln_w)
    ln_Z = logsumexp(ln_w + ln_l/temperature)

    # Posterior weights and information
    ln_W = ln_w + ln_l - ln_Z
    W = np.exp(ln_W)
    H = np.sum(W*(ln_W - ln_w))
    ESS = np.exp(-np.sum(W*ln_W))

    print("ln(Z) = {ln_Z}".format(ln_Z=ln_Z))
    print("H = {H} nats".format(H=H))
    print("Effective sample size = {ESS}".format(ESS=ESS))
    np.savetxt("output/posterior_weights.txt", W)

    plt.figure(figsize=(9, 7))
    plt.subplot(2, 1, 1)

    # Thinning
    if(particles_info.shape[0] > 30000):
        indices = rng.randint(particles_info.shape[0], size=30000)
        ln_w = ln_w[indices]
        ln_l = ln_l[indices]
        W = W[indices]

    plt.plot(ln_w, ln_l, ".", markersize=3, alpha=0.3)
    ln_l_sorted = np.sort(ln_l)
    plt.ylim([ln_l_sorted[int(0.05*len(ln_l_sorted))], ln_l_sorted[-1]])
    plt.ylabel("$\\ln(L)$")

    plt.subplot(2,1,2)
    plt.plot(ln_w, W/W.max(), ".", markersize=3, alpha=0.5)
    plt.xlabel("$\\ln(w)$")
    plt.ylabel("$W/W_{\\rm max}$")
    plt.savefig("output/likelihood_curve.png", dpi=600)
    print("Saved output/likelihood_curve.png")
    plt.show()

