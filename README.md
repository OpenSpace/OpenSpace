![OpenSpace Logo](/data/openspace-horiz-logo-crop.png)

<p align="center">
  <em>An open-source interactive data visualization software designed to visualize the entire known universe</em>
</p>

<p align="center">
  <a href="https://docs.openspaceproject.com"><b>Docs</b></a> · <a href="https://join.slack.com/t/openspacesupport/shared_invite/zt-24uhn3wvo-gCGHgjg2m9tHzKUEb_FyMQ"><b>Slack</b></a> · <a href="http://openspaceproject.com"><b>Website</b></a> · <a href="https://www.youtube.com/@OpenSpaceProj"><b>YouTube</b></a>
</p>

<p align="center">
  <a href="LICENSE.md"><img src="https://img.shields.io/badge/License-MIT-purple.svg?style=flat-square" alt="License"></a>
  <a href="https://www.openspaceproject.com/installation"><img src="https://img.shields.io/github/v/tag/OpenSpace/OpenSpace?label=Version&color=maroon&style=flat-square" alt="Download"></a>
  <a href="https://join.slack.com/t/openspacesupport/shared_invite/zt-24uhn3wvo-gCGHgjg2m9tHzKUEb_FyMQ"><img src="https://img.shields.io/badge/slack-join?style=social&logo=slack" alt="Slack"></a>
</p>


[OpenSpace](http://openspaceproject.com) is an open source, non-commercial, and freely available interactive data visualization software designed to visualize the entire known universe and portray our ongoing efforts to investigate the cosmos. Bringing the latest techniques from data visualization research to the general public, OpenSpace supports interactive presentation of dynamic data from observations, simulations, and space mission planning and operations. The software works on multiple operating systems (Windows, Linux, MacOS) with an extensible architecture capable of powering both personal computers and also high resolution tiled displays and planetarium domes. In addition, OpenSpace enables simultaneous connections across the globe creating opportunity for shared experiences among audiences worldwide. The target audience of the software reaches from the general public who wishes to explore our universe, enthusiasts interested in hacking the underlying components in OpenSpace to create unique experiences, informal science institutions wishing to create a low-cost, yet powerful exhibition piece, but also scientists desiring to visualize their datasets in a contextualized, powerful software.


[![System Paper](https://img.shields.io/badge/System%20Paper-10.1109%2FTVCG.2019.2934259-blue?style=flat-square)](https://doi.org/10.1109/TVCG.2019.2934259)
[![GlobeBrowsing Paper](https://img.shields.io/badge/GlobeBrowsing%20Paper-https%3A%2F%2Fdoi.org%2F10.1109%2FTVCG.2017.2743958-blue?style=flat-square)](https://doi.org/10.1109/TVCG.2017.2743958)

![Contributors](https://img.shields.io/github/contributors/OpenSpace/OpenSpace?style=flat-square)
![Commits](https://img.shields.io/github/commit-activity/m/OpenSpace/OpenSpace?color=green&style=flat-square)

![Image](https://docs.openspaceproject.com/latest/_static/images/collection.jpg)


<h1 align="center">Background</h1>
OpenSpace started as a collaboration between Sweden's <a href="https://immvis.github.io">Linköping University</a> (LiU) and the <a href="https://www.amnh.org">American Museum of Natural History</a>. Development of the software began several years ago through a close collaboration with NASA Goddard's <a href="https://ccmc.gsfc.nasa.gov">Community Coordinated Modeling Center</a> (CCMC) to model space weather forecasting and continued with visualizations of NASA's New Horizons mission to Pluto and ESA's Rosetta mission to 67P/Churyumov-Gerasimenko. This promising set of preliminary work provided a foundation for continued funding from NASA, the Swedish eScience Research Centre, and the Knut and Alice Wallenberg foundation, which has extended the collaboration to include the University of Utah's <a href="https://www.sci.utah.edu">Scientific Computing and Imaging</a> (SCI) Institute, <a href="https://www.nyu.edu">New York University</a>'s Tandon School of Engineering, multiple informal science institutions across the world, and multiple, international vendors.

![Image](https://docs.openspaceproject.com/latest/_static/images/presentation.jpg)


<h1 align="center">Features</h1>
Some of the high-level features supported in OpenSpace are:

  - AMNH's Digital Universe catalog of extrasolar datasets (stars, galaxies, quasars, ...)
  - High-resolution planetary images for major objects in the solar system (Earth, Moon, Mars, Venus, ...)
  - Animated 3D models representing space missions (ISS, New Horizons, JWST, ...)
  - Support for custom profiles with arbitrary user-defined content
  - Ability to drive any type of display environment (flat screen, multi-projector, planetariums, ...)
  - Lua, JavaScript, and Python interfaces into the engine allowing highly customized controls
  - Native support to export an interactive sessions as individual frames for video export
  - much much more (see our [Changelog](https://github.com/OpenSpace/OpenSpace/releases/))

OpenSpace requires at least support for [OpenGL](https://www.opengl.org/) version 3.3, some custom components require at least version 4.2.

![Image](https://docs.openspaceproject.com/latest/_static/images/display-systems.jpg)


<h1 align="center">Getting Started</h1>
This repository contains the source code and example profiles for OpenSpace, but does not contain any data. To build and install the application, please check out the <a href="https://docs.openspaceproject.com">Documentation</a>. Here, you will find the build instructions for all operating systems. Please note that the Apple Silicon series of chips do not support OpenGL natively and Metal 2 does not support `double` precision accuracy (see the <a href="https://developer.apple.com/metal/Metal-Shading-Language-Specification.pdf">specification</a> Section 2.1), therefore only the Intel processors for MacOS are supported and maintained.

Requirements for compiling are:

  - CMake version 3.25 or above
  - C++ compiler supporting C++20/C++23 (Visual Studio 2022 17.11, GCC13, Clang17, AppleClang 15.0.0)
  - <a href="https://www.qt.io/download">Qt</a>


<h2 align="center">:bulb: Asking Questions :bulb:</h2>
Feel free to create issues for missing features, bug reports, or compile problems or contact us via <a href="mailto:support@openspaceproject.com?subject=OpenSpace:">email</a>. Regarding any issues, you are very welcome on our <a href="https://openspacesupport.slack.com">Slack support channel</a> to which you can freely <a href="https://join.slack.com/t/openspacesupport/shared_invite/zt-24uhn3wvo-gCGHgjg2m9tHzKUEb_FyMQ">sign-up</a>.

<h2 align="center">:heart: Contributing :heart:</h2>
Any contributions to the software are very welcome and can take a multitude of forms, from reporting a bug, fixing bugs, creating new content, writing new features, and even creating and sharing images and videos you have made with the software. Please feel free to share anything you want in the #showcase channel on the Slack.

![Image](https://docs.openspaceproject.com/latest/_static/images/himalaya-nkpg-dome.jpg)

<p align="center">
  <h3>OpenSpace activity</h3>
  <img src="https://repobeats.axiom.co/api/embed/8e7f51f0116f558d420b7caccb10840a746a659f.svg" alt="Repobeats analytics image OpenSpace repository">
  <h3>OpenSpace User Interface activity</h3>
  <img src="https://repobeats.axiom.co/api/embed/65a17767719d8a61268b34add1e603fbd4a24156.svg" alt="Repobeats analytics image OpenSpace-WebGui repository">
</p>


<h1 align="center">License</h1>
OpenSpace is under an permissive <a href="https://github.com/OpenSpace/OpenSpace/blob/master/LICENSE.md">MIT license</a>. The license files contains for more detail, but the short version is that you can use OpenSpace for commercial or non-commercial purposes as long as you give credit.


<h1 align="center">Support</h1>
OpenSpace is grateful for the support from the following institutions:

<p align="center">
  <img src="https://docs.openspaceproject.com/latest/_static/logos/sponsors.png" alt="Supporters">
</p>
