"""
A sandbox for testing ideas
"""
import dnest4
import matplotlib.pyplot as plt
from numba import jit
import numpy as np
import numpy.random as rng

class Sampler:
    """
    The current state of a sampler.
    """

    def __init__(self, num_particles=1000):
        self.num_particles = num_particles

    def from_prior(self):
        self.particles = rng.rand(self.num_particles, 2)
        self.tbs = rng.rand(self.num_particles)

        self.shadow_particles = rng.rand(self.num_particles, 2)
        self.shadow_tbs = rng.rand(self.num_particles)
        self.calculate_uccs()


    def calculate_ucc(self, x, y):
        """
        Calculate the UCC at the given position.
        """
        ucc = 0
        for i in range(self.shadow_particles.shape[0]):
            if (x < self.shadow_particles[i, 0]) &\
               (y < self.shadow_particles[i, 1]):
                ucc += 1
        return ucc

    def calculate_uccs(self):
        self.uccs = np.empty(self.num_particles)
        for i in range(self.num_particles):
            self.uccs[i] = self.calculate_ucc(self.particles[i, 0],
                                              self.particles[i, 1])

    def update(self):

        # Find worst particle
        uccs_tbs = [(self.uccs[i], self.tbs[i])\
                            for i in range(self.num_particles)]
        worst = 0
        for i in range(self.num_particles):
            if uccs_tbs[i] > uccs_tbs[worst]:
                worst = i

        # Result of iteration
        result = self.particles[worst, :]

        # Threshold
        threshold = uccs_tbs[worst]

        # Replace particle
        while True:
            copy = rng.randint(self.num_particles)
            if copy != worst:
                break

        # Copy survivor
        self.particles[worst, :] = self.particles[copy, :]
        self.tbs[worst] = self.tbs[copy]

        # Do MCMC
        k = worst # Alias

        self.particles[k, :], self.tbs[k], self.uccs[k] = \
                self.explore(self.particles[k, :], self.tbs[k], self.uccs[k],
                                threshold)

        # Check for any bad shadow particles
        replace = []
        for i in range(self.shadow_particles.shape[0]):
            ucc = self.calculate_ucc(self.shadow_particles[i, 0],
                                     self.shadow_particles[i, 1])
            pair = (ucc, self.shadow_tbs[i])
            if pair > threshold:
                particle = self.shadow_particles[i, :].copy()
                tb = self.shadow_tbs[i].copy()
                particle, tb, ucc = self.explore(particle,
                                                 tb, ucc,
                                                 threshold)
                replace.append((i, particle, tb))

        for r in replace:
            i, particle, tb = r
            self.shadow_particles[i, :] = particle
            self.shadow_tbs[i] = tb

        return result

    def explore(self, particle, tb, ucc, threshold):

        for i in range(1000):
            proposal = particle.copy()
            proposal[0] = dnest4.wrap(proposal[0] + dnest4.randh(), 0.0, 1.0)
            proposal[1] = dnest4.wrap(proposal[1] + dnest4.randh(), 0.0, 1.0)
            proposal_tb = dnest4.wrap(tb + dnest4.randh(), 0.0, 1.0)
            proposal_ucc = self.calculate_ucc(proposal[0], proposal[1])

            if (proposal_ucc, proposal_tb) < threshold:
                particle = proposal
                tb = proposal_tb
                ucc = proposal_ucc

        return [particle, tb, ucc]

if __name__ == "__main__":
    sampler = Sampler(100)
    sampler.from_prior()

#    plt.scatter(sampler.shadow_particles[:,0], sampler.shadow_particles[:,1],
#                marker="o", color="k", alpha=0.2)
    for i in range(0, 500):
        result = sampler.update()
        plt.scatter(result[0], result[1], marker=".", color="r", alpha=0.3)
        print(i+1)

    plt.show()

    
