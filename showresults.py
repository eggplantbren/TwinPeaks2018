import twinpeaks2018

# demo=True makes extra plots based on the ground truth of the demo model.
# If you leave it in while using another model, you'll get some additional
# output that will be spurious. You can just ignore it, though.
twinpeaks2018.postprocess_two_scalars(demo=True)

# One-scalar version
# twinpeaks2018.postprocess_one_scalar()

