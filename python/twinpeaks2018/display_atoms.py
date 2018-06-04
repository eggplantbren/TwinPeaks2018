import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import numpy.random as rng
import pandas as pd
import os

def display_atoms(num=100):

    # Remove old figures
    os.system("rm output/atoms*.png")

    # Load the particles
    particles = pd.read_csv("output/particles.csv")
    particle_xs = np.array(particles.iloc[:, 0:30])
    particle_ys = np.array(particles.iloc[:, 30:60])
    particle_zs = np.array(particles.iloc[:, 60:90])

    # Set up 3D axes
    fig = plt.figure(figsize=(8, 8))
    for i in range(num):
        k = rng.randint(particles.shape[0])

        plt.clf()
        ax = fig.add_subplot(111, projection="3d")
        ax.scatter(particle_xs[k, :], particle_ys[k, :], particle_zs[k, :])
        ax.set_xlim([0.0, 100.0])
        ax.set_ylim([0.0, 100.0])
        ax.set_zlim([0.0, 100.0])
        plt.savefig("output/atoms" + "%0.6d"%(i+1) + ".png")
        print("Saved " + "output/atoms" + "%0.6d"%(i+1) + ".png")

