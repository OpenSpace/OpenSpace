![OpenSpace Logo](/data/openspace-horiz-logo-crop.png)
[OpenSpace](http://openspaceproject.com) is an open source, non-commercial, and freely available interactive data visualization software designed to visualize the entire known universe and portray our ongoing efforts to investigate the cosmos.  Bringing the latest techniques from data visualization research to the general public, OpenSpace supports interactive presentation of dynamic data from observations, simulations, and space mission planning and operations.  The software works on multiple operating systems (Windows, Linux, MacOS) with an extensible architecture capable of powering both personal computers and also high resolution tiled displays and planetarium domes.  In addition, OpenSpace enables simultaneous connections across the globe creating opportunity for shared experiences among audiences worldwide.  The target audience of the software reaches from the general public who wishes to explore our universe, enthusiasts interested in hacking the underlying components in OpenSpace to create unique experiences, informal science institutions wishing to create a low-cost, yet powerful exhibition piece, but also scientists desiring to visualize their datasets in a contextualized, powerful software.

[![License](https://img.shields.io/badge/License-MIT-purple.svg?style=flat-square)](LICENSE)
[![Download](https://img.shields.io/github/v/tag/OpenSpace/OpenSpace?label=Version&color=maroon&style=flat-square)](https://www.openspaceproject.com/installation)
![Size](https://img.shields.io/github/repo-size/OpenSpace/OpenSpace?style=flat-square&color=red)

[![System Paper](https://img.shields.io/badge/System%20Paper-10.1109%2FTVCG.2019.2934259-blue?style=flat-square)](https://doi.org/10.1109/TVCG.2019.2934259)
[![GlobeBrowsing Paper](https://img.shields.io/badge/GlobeBrowsing%20Paper-https%3A%2F%2Fdoi.org%2F10.1109%2FTVCG.2017.2743958-blue?style=flat-square)](https://doi.org/10.1109/TVCG.2017.2743958)

![Contributors](https://img.shields.io/github/contributors/OpenSpace/OpenSpace?style=flat-square)
![Commits](https://img.shields.io/github/commit-activity/m/OpenSpace/OpenSpace?color=green&style=flat-square)

![Image](https://docs.openspaceproject.com/en/latest/_static/images/collection.jpg)

# Background
OpenSpace started as a collaboration between Sweden's [Linköping University](https://immvis.github.io) (LiU) and the [American Museum of Natural History](https://www.amnh.org) (AMNH).  Development of the software began several years ago through a close collaboration with NASA Goddard's [Community Coordinated Modeling Center](https://ccmc.gsfc.nasa.gov) (CCMC) to model space weather forecasting and continued with visualizations of NASA's New Horizons mission to Pluto and ESA's Rosetta mission to 67P/Churyumov-Gerasimenko.  This promising set of preliminary work provided a foundation for continued funding from NASA, the Swedish eScience Research Centre, and the Knut and Alice Wallenberg foundation, which has extended the collaboration to include the University of Utah's [Scientific Computing and Imaging](https://www.sci.utah.edu) (SCI) Institute, [New York University](https://www.nyu.edu)'s Tandon School of Engineering, multiple informal science institutions across the world, and multiple, international vendors.

![Image](https://docs.openspaceproject.com/en/latest/_static/images/presentation.jpg)

# Features
Some of the high-level features supported in OpenSpace are:
 - AMNH's Digital Universe catalog of extrasolar datasets (stars, galaxies, quasars, ...)
 - High-resolution planetary images for major objects in the solar system (Earth, Moon, Mars, Venus, ...)
 - Animated 3D models representing space missions (ISS, New Horizons, JWST, ...)
 - Support for custom profiles with arbitrary user-defined content
 - Ability to drive any type of display environment (flat screen, multi-projector, planetariums, ...)
 - Lua and JavaScript interface into the engine allowing highly customized controls
 - Native support to export an interactive sessions as individual frames for video export
 - much much more (see our [Changelog](http://wiki.openspaceproject.com/docs/general/releases))

OpenSpace requires at least support for [OpenGL](https://www.opengl.org/) version 3.3, some custom components require at least version 4.2.

![Image](https://docs.openspaceproject.com/en/latest/_static/images/display-systems.jpg)

# Getting Started
This repository contains the source code and example profiles for OpenSpace, but does not contain any data.  To build and install the application, please check out the [GitHub Wiki](https://github.com/OpenSpace/OpenSpace/wiki).  Here, you will find two pages, a [build instruction](https://github.com/OpenSpace/OpenSpace/wiki/Compiling) for all operating systems and then additional instructions for [Windows](https://github.com/OpenSpace/OpenSpace/wiki/Compiling-Windows), [Linux (Ubuntu)](https://github.com/OpenSpace/OpenSpace/wiki/Compiling-Ubuntu), and [MacOS](https://github.com/OpenSpace/OpenSpace/wiki/Compiling-MacOS). Please note that the Apple Silicon series of chips do not support OpenGL natively and Metal 2 does not support `double` precision accuracy (see [here](https://developer.apple.com/metal/Metal-Shading-Language-Specification.pdf) Section 2.1), therefore only the Intel processors for MacOS are supported and maintained.

Requirements for compiling are:
 - CMake version 3.25 or above
 - C++ compiler supporting C++20/C++23 (MSVC 19.39, GCC13, Clang17, AppleClang 15.0.0)
 - [Boost](http://www.boost.org/)
 - [Qt](http://www.qt.io/download)

Feel free to create issues for missing features, bug reports, or compile problems or contact us via [email](mailto:openspace@amnh.org?subject=OpenSpace:).  Regarding any issues, you are very welcome on our [Slack support channel](https://openspacesupport.slack.com) to which you can freely [sign-up](https://join.slack.com/t/openspacesupport/shared_invite/zt-24uhn3wvo-gCGHgjg2m9tHzKUEb_FyMQ).

![Image](https://docs.openspaceproject.com/en/latest/_static/images/himalaya-nkpg-dome.jpg)

# License
The contents of this repository provided under an [MIT license](https://github.com/OpenSpace/OpenSpace/blob/master/LICENSE.md).

# Support
OpenSpace is supported by the following institutions:

![Image](https://docs.openspaceproject.com/en/latest/_static/logos/sponsors.png)
