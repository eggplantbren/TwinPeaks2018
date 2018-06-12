import matplotlib.pyplot as plt
import numpy as np
import numpy.random as rng
import pandas as pd
import os

def display_potts(num=100):

    # Remove old figures
    os.system("rm output/potts*.png")

    # Load the particles
    particles = pd.read_csv("output/particles.csv")
    n = int(np.sqrt(particles.shape[1]))

    for i in range(num):
        k = rng.randint(particles.shape[0])

        plt.clf()
        img = np.array(particles.iloc[k, :]).reshape((n, n))
        plt.imshow(img)
        plt.savefig("output/potts" + "%0.6d"%(i+1) + ".png")
        print("Saved " + "output/potts" + "%0.6d"%(i+1) + ".png")

