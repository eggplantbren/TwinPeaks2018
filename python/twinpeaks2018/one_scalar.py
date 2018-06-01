import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from twinpeaks2018.utils import *

# Niceify plots
plt.rcParams["font.family"] = "serif"
plt.rcParams["font.size"] = 16
plt.rc("text", usetex=True)

def postprocess_one_scalar(temperature=1.0):
    """
    Postprocessing for one scalar
    """
    particles_info = load_particles_info()
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
    plt.plot(ln_w, ln_l, ".", markersize=1, alpha=0.3)
    ln_l_sorted = np.sort(ln_l)
    plt.ylim([ln_l_sorted[int(0.05*len(ln_l_sorted))], ln_l_sorted[-1]])
    plt.ylabel("$\\ln(L)$")

    plt.subplot(2,1,2)
    plt.plot(ln_w, W/W.max(), ".", markersize=1, alpha=0.5)
    plt.xlabel("$\\ln(w)$")
    plt.ylabel("$W/W_{\\rm max}$")
    plt.savefig("output/likelihood_curve.png", dpi=600)
    print("Saved output/likelihood_curve.png")
    plt.show()

