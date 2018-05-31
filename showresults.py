import twinpeaks2018

# Infer whether we're dealing with one or two scalars!
f = open("output/particles_info.csv")
columns = (f.readline()).split(",")
f.close()

if len(columns) == 5:
    # demo=True makes extra plots based on the ground truth of the demo model.
    # If you leave it in while using another model, you'll get some additional
    # output that will be spurious. You can just ignore it, though.
    twinpeaks2018.postprocess_two_scalars(demo=True)

elif len(columns) == 4:
    # One-scalar version
    twinpeaks2018.postprocess_one_scalar()

else:
    print("Unexpected number of columns in output/particles_info.csv.")
    print("You may be running a model with 3 or more scalars, in which case "\
                + "you're on your own.")
