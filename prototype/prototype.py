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
def compute_particle_uccs(particles):
    """
    Compute the non-integer UCCs.
    """
    uccs = np.empty(len(particles["xs"]), dtype="float64")
    for i in range(len(particles["xs"])):
        uccs[i] = compute_ucc(particles["xs"][i],
                              particles["ys"][i],
                              particles)
    uccs += particles["us"]
    return uccs



def compute_ucc(x, y, particles):
    """
    Compute the integer UCC at the given position
    """
    above = (particles["xs"] > x) & \
            (particles["ys"] > y)
    return np.sum(above)

@jit
def ucc_grid(particles):
    """
    Evaluate integer UCC on a grid
    """
    # The grid
    xx = np.linspace(0.0, 1.0, 201)
    yy = np.linspace(0.0, 1.0, 201)
    [xx, yy] = np.meshgrid(xx, yy[::-1])

    # Compute uccs
    uccs = np.zeros(xx.shape, dtype="int64")
    for i in range(xx.shape[0]):
        for j in range(xx.shape[1]):
            uccs[i, j] = compute_ucc(xx[i, j], yy[i, j], particles)

    # Pack into a dictionary
    return { "xx": xx, "yy": yy, "uccs": uccs}

if __name__ == "__main__":
    num_particles = 1

    # Particles and their real-valued UCCs
    particles = generate(num_particles)
    uccs = compute_particle_uccs(particles)
    worst_ucc = uccs.max()

    # Integer UCCs on a grid
    grid = ucc_grid(particles)
    print((grid["uccs"] >= worst_ucc).sum()*0.005**2)

    # Plot initial particles and grid
    plt.figure(figsize=(7, 6))
    plt.imshow(grid["uccs"], extent=[0.0, 1.0, 0.0, 1.0],
               cmap="Oranges")
    plt.scatter(particles["xs"], particles["ys"],
                s=10.0*uccs, marker="o", color="k", alpha=0.5)
    plt.axis([0, 1, 0, 1])
    plt.show()

