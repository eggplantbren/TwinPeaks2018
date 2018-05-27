import dnest4.classic as dn4
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Load and plot trajectories
particles_info = pd.read_csv("../output/particles_info.csv")
plt.plot(particles_info.iloc[:,1],
         particles_info.iloc[:,2], ".", alpha=0.2, markersize=1)
plt.show()

# Temperatures
temperatures = [10.0, 20.0]
logw = particles_info["ln_prior_mass"]
logw = logw - dn4.logsumexp(logw)
logW = logw + particles_info["f"]/temperatures[0]\
            + particles_info["g"]/temperatures[1]
logZ = dn4.logsumexp(logW)
print(logZ) # -88.0867665401428
logW = logW - logZ

plt.plot(logW)
plt.show()

