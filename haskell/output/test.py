import matplotlib.pyplot as plt
import pandas as pd

particles = pd.read_csv("output.csv", header=2)
plt.scatter(particles.iloc[:,0], particles.iloc[:,1],
           s=particles.iloc[:,2], marker="o", alpha=0.2)
plt.show()

