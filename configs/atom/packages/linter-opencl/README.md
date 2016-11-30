[![Build Status](https://travis-ci.org/BenSolus/linter-opencl.svg?branch=master)](https://travis-ci.org/BenSolus/linter-opencl)

# linter-opencl package

Linter plugin for [Linter](https://github.com/AtomLinter/Linter), provides an interface to the build functionality of OpenCL.

## Important info for user of hybrid graphics!

On systems with hybrid graphics, OpenCL will compile and lint for the integrated graphics card by default. While Windows users just need to set the platform index of the desired platform, Linux users who wants to use the dedicated graphics card need to enable the '''hybridGraphics''' option and provide the path to a GPU offloader like '''optirun''' to enable linting for those devices.

## Installation

1. Install [Python](https://www.python.org) and [PyOpenCL](https://mathema.tician.de/software/pyopencl/)
2. Install [linter](https://github.com/steelbrain/linter) and [linter-opencl](https://github.com/BenSolus/linter-opencl)
3. (Configure the path to Python and set OpenCL Vendor and Platfrom Index in preferences.)
4. Go linting!

## Afterword
OpenCL is a complex beast. There are lots of different settings provided for building kernels on devices and the format of the log differs depending on the vendor which provides OpenCL. Due to this fact and my limited resources (of testing platforms) there will be uncatched cases and issues so feedback will be appreciated.
