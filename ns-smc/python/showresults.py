import dnest4.classic as dn4
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Load and plot trajectories
particles_info = pd.read_csv("../output/particles_info.csv")
plt.plot(particles_info["f"],
         particles_info["g"], ".", alpha=0.2, markersize=1)
plt.show()

run_ids = np.arange(min(particles_info["run_id"]),
                    max(particles_info["run_id"] + 1))

# Temperatures
temperatures = [10.0, 20.0]
logw = particles_info["ln_prior_mass"].copy()
logw = logw - dn4.logsumexp(logw)
logW = logw + particles_info["f"]/temperatures[0]\
            + particles_info["g"]/temperatures[1]
logZ = dn4.logsumexp(logW)
print(logZ) # -88.0867665401428
logW = logW - logZ

#logzs = []
#for run_id in run_ids:
#    which = np.nonzero(particles_info["run_id"] == run_id)[0]
#    subset = particles_info.iloc[which, :]
#    logw = subset["ln_prior_mass"].copy()
#    logW = logw + subset["f"]/temperatures[0]\
#                + subset["g"]/temperatures[1]
#    logzs.append(dn4.logsumexp(logW))

#print(dn4.logsumexp(logzs) - np.log(len(logzs)))

plt.plot(logW)
plt.show()

