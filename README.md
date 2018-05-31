TwinPeaks2018
=============
[![Build Status](https://travis-ci.org/eggplantbren/TwinPeaks2018.svg?branch=master)](https://travis-ci.org/eggplantbren/TwinPeaks2018)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A new and (hopefully) correct implementation of TwinPeaks
using the SwitchSampling idea.

(c) 2018 Brendon J. Brewer.

LICENSE: GNU General Public License version 3. See the LICENSE
file for details.

# Dependencies

For the C++ to compile, you'll need:

* [yaml-cpp](https://github.com/jbeder/yaml-cpp) and its dependency
  [boost](https://boost.org)

You can probably get these from your operating system's package manager.
You'll also need the C++ header files, which are sometimes put into a
separate package with the suffix -dev or -devel. For example, on Ubuntu, this
will do the trick:

```
sudo apt-get install libboost-all-dev libyaml-cpp-dev
```

On the Python side, you'll need:

* Python 3 and some packages (numpy, matploblib, and pandas).
  [Anaconda](https://www.anaconda.com)
  is a good Python distribution that includes these. Otherwise they might come
  with (or be installable through) Python 3 as provided by your OS.
* LaTeX and [dvipng](https://sourceforge.net/projects/dvipng/) so that LaTeX
  can be used with matplotlib.

# Compilation

First, compile the C++:

```make```

Then install the python package:

```
cd python
python setup.py install
```

# Acknowledgements

The authors of [this paper](https://arxiv.org/abs/1805.03924).
