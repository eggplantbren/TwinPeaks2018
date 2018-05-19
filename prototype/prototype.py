"""
A sandbox for testing ideas
"""
import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng

# Generate some initial particles
def generate(n):
    """
    Generate some initial particles.
    """
    return { "xs": rng.rand(n), "ys": rng.rand(n) }

def find_worst(particles):
    """
    Find indices of the worst particles.
    """
    x_min, y_min = particles["xs"].min(), particles["ys"].min()
    ix = np.nonzero(particles["xs"] == x_min)[0]
    iy = np.nonzero(particles["ys"] == y_min)[0]
    return { "x" : x_min, "y" : y_min,\
             "ix": ix,    "iy": iy }

# How many particles to use in demo
N = 10
particles = generate(N)
worst = find_worst(particles)

# Plot initial particles
plt.figure(figsize=(7, 6))
plt.plot(particles["xs"], particles["ys"], "o", alpha=0.3)
plt.axvline(worst["x"], color="k", alpha=0.3)
plt.axhline(worst["y"], color="k", alpha=0.3)
plt.axis([0, 1, 0, 1])
plt.show()

