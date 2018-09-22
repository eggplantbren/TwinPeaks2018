import matplotlib.pyplot as plt
import numpy as np
from twinpeaks2018.utils import *

plot_settings()

def compute_truth(limits, n=21):
    T0_min, T0_max, T1_min, T1_max = limits

    # Set up grids
    ln_Z = np.empty((n, n))
    H = np.empty((n, n))
    x = np.linspace(0.0, 1.0, 20001)
    dx = x[1] - x[0]

    # Temperature grids
    T0 = 10.0**np.linspace(np.log10(T0_min),
                                        np.log10(T0_max), ln_Z.shape[0])
    T1 = 10.0**np.linspace(np.log10(T1_min),
                                        np.log10(T1_max), ln_Z.shape[1])
    [T0, T1] = np.meshgrid(T0, T1)

    # Compute over the grid
    for i in range(ln_Z.shape[0]):
        for j in range(ln_Z.shape[1]):

            ln_y = -((x - 0.5)/0.1)**2 / T0[i, j] - \
                        (x / 0.1) / T1[i, j]
            ln_Y = ln_y - logsumexp(ln_y + np.log(dx))
            Y = np.exp(ln_Y)

            # Normalisation constant and KL divergence
            ln_Z[i, j] = 100*logsumexp(ln_y + np.log(dx))
            H[i, j] = 100*np.sum(Y*ln_Y*dx)

            print(".", end="", flush=True)
    print("")

    return {"ln_Z": ln_Z, "H": H}




