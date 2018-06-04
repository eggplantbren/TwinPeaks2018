import matplotlib.pyplot as plt
import numpy as np
import twinpeaks2018 as tp

# Compute the true partition function
limits = [1E-3, 1E3, 1E-3, 1E3]
truth = tp.example_truth.compute_truth(limits=limits, n=101)

# Plot the results
plt.figure(1, figsize=(9, 6))
plt.subplot(1, 2, 1)
plt.imshow(truth["ln_Z"], origin="lower", extent=np.log10(limits))
plt.xlabel("$\\log_{10}(T_1)$")
plt.ylabel("$\\log_{10}(T_2)$")
plt.title("$\\ln(Z)$")

plt.subplot(1, 2, 2)
plt.imshow(truth["H"], origin="lower", extent=np.log10(limits))
plt.xlabel("$\\log_{10}(T_1)$")
plt.title("$H$")

plt.savefig("figures/demo_truth.pdf")
print("Saved figures/demo_truth.pdf")

