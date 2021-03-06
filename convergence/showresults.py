import matplotlib.pyplot as plt
import yaml

f = open("demo.yaml")
data = yaml.load(f, Loader=yaml.SafeLoader)
f.close()

xs = [run["reps"] for run in data["runs"]]
ys = [run["mean_abs_error_lnZ"] for run in data["runs"]]

plt.semilogx(xs, ys, "o", alpha=0.3, markersize=4)
plt.xlabel("Number of repetitions")
plt.ylabel("mean_abs_error_lnZ")
plt.show()

