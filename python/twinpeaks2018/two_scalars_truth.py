import matplotlib.pyplot as plt
import numpy as np
from twinpeaks2018.utils import logsumexp

def compute_truth(limits, n=51, plot=False):
    T0_min, T0_max, T1_min, T1_max = limits

    # Set up grids
    ln_Z = np.empty((n, n))
    H = np.empty((n, n))
    x = np.linspace(0.0, 1.0, 100001)
    dx = x[1] - x[0]

    # Temperature grids
    T0 = 10.0**np.linspace(np.log10(T0_min),
                                        np.log10(T0_max), ln_Z.shape[0])
    T1 = 10.0**np.linspace(np.log10(T0_min),
                                        np.log10(T0_max), ln_Z.shape[1])
    [T0, T1] = np.meshgrid(T0, T1)

    # Compute over the grid
    for i in range(ln_Z.shape[0]):
        for j in range(ln_Z.shape[1]):

            ln_y = -((x - 0.5)/0.1)**2 / T0[i, j] - \
                        (np.sin(10*np.pi*x))**2 / T1[i, j]
            ln_Y = ln_y - logsumexp(ln_y)
            Y = np.exp(ln_Y)

            # Normalisation constant and KL divergence
            ln_Z[i, j] = 100*logsumexp(ln_y + np.log(dx))
            H[i, j] = 100*np.sum(Y*ln_Y*dx)

            print(".", end="", flush=True)
    print("")

    if plot:
        # Plot the results
        limits = [0.1, 100.0, 0.1, 100.0]
        result = compute_truth(limits)

        plt.figure(figsize=(8, 6))
        plt.subplot(1, 2, 1)
        plt.imshow(result["ln_Z"], origin="lower", extent=np.log10(limits))
        plt.xlabel("log10(T_0)")
        plt.ylabel("log10(T_1)")
        plt.title("ln_Z")

        plt.subplot(1, 2, 2)
        plt.imshow(result["H"], origin="lower", extent=np.log10(limits))
        plt.xlabel("log10(T_0)")
        plt.ylabel("log10(T_1)")
        plt.title("H")
        plt.show()

    return {"ln_Z": ln_Z, "H": H}




