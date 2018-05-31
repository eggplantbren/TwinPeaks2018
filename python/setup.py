import setuptools

setuptools.setup(
    name="twinpeaks2018",
    version="0.0.1",
    author="Brendon J. Brewer",
    author_email="brendon.brewer@gmail.com",
    description="The Python package accompanying TwinPeaks2018",
    long_description="The Python package accompanying TwinPeaks2018",
    url="https://github.com/eggplantbren/TwinPeaks2018",
    packages=setuptools.find_packages(),
    classifiers=(
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
    ),
    install_requires=["matplotlib", "numpy", "pandas"]
)

