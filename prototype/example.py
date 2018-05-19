"""
An example model
"""
import numpy as np
import numpy.random as rng

class Particle:
    """
    An object is a particle in the parameter space.
    """

    # Dimensionality
    n = 10
    num_scalars = 2

    def __init__(self):
        """
        Constructor
        """
        self.xs = np.empty(Particle.n)
        self.compute_scalars()

    def from_prior(self):
        """
        Generate from prior
        """
        self.xs = rng.rand(Particle.n)
        self.compute_scalars()

    def compute_scalars(self):
        """
        Calculate the scalars
        """
        self.scalars = np.empty(2)
        self.scalars[0] = 
        self.scalars[1] = 

if __name__ == "__main__":
    x = Particle()
    x.from_prior()
    print(x)

