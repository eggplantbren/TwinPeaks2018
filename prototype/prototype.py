"""
A sandbox for testing ideas
"""
import matplotlib.pyplot as plt
from numba import jit
import numpy as np
import numpy.random as rng

def generate(n):
    """
    Generate some initial particles.
    """
    return { "xs": rng.rand(n), "ys": rng.rand(n),
             "us": rng.rand(n) }

@jit
def compute_uccs(particles):
    """
    Compute the non-integer UCCs.
    """
    uccs = np.empty(len(particles["xs"]))
    for i in range(len(particles["xs"])):
        above = (particles["xs"] > particles["xs"][i]) & \
                (particles["ys"] > particles["ys"][i])
        uccs[i] = np.sum(above) + particles["us"][i]
    return uccs

if __name__ == "__main__":
    num_particles = 100
    rng.seed(0)
    particles = generate(num_particles)
    uccs = compute_uccs(particles)

    # Plot initial particles
    plt.figure(figsize=(7, 6))
    plt.scatter(particles["xs"], particles["ys"], s=5.0*uccs,
                marker="o", alpha=0.3)
    plt.axis([0, 1, 0, 1])
    plt.show()

