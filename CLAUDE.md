# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

OpenSpace is an open-source interactive data visualization software designed to visualize the known universe. It's a C++ application using OpenGL 3.3+ with a modular architecture, Lua scripting interface, and support for multiple display environments (flat screens, multi-projector, planetariums).

## Build System

### Core Build Tools
- **CMake 3.25+** is the primary build system
- **C++20/C++23** compiler required (Visual Studio 2022 17.11+, GCC13+, Clang17+, AppleClang 15.0.0+)
- **Qt** framework required for GUI components

### Common Build Commands
```bash
# Configure step (only do this when changes are made to the CMake build system)
cmake . --preset windows-min --fresh
# Build for RelWithDebInfo configuration always
cmake --build build --config RelWithDebInfo -j
```

### Code Generation
OpenSpace uses a custom code generation system:
- The `codegen-tool` processes modules and src directories
- Code generation runs automatically during build via the `run_codegen` target
- Generated files have `_codegen.cpp` suffix

## Code Architecture

### Core Engine Structure
The main engine components are located in `src/`:

- **`engine/`** - Core engine systems (OpenSpaceEngine, Configuration, ModuleEngine, SyncEngine)
- **`rendering/`** - Render engine, renderables, dashboard, screen-space renderables
- **`scene/`** - Scene graph, assets, profiles, transformations (rotation, scale, translation)
- **`navigation/`** - Camera navigation (OrbitalNavigator, PathNavigator, KeyframeNavigator)
- **`interaction/`** - Input handling, action management, key bindings, session recording
- **`scripting/`** - Lua script engine and bindings
- **`network/`** - Parallel connection and peer systems
- **`properties/`** - Property system (scalar, vector, matrix, list properties)

### Module System
OpenSpace has a modular architecture with modules in `modules/`:

#### Complete Module Reference

**atmosphere** - Provides realistic atmospheric rendering with Rayleigh and Mie scattering, supporting various atmospheric parameters, shadows, and deferred rendering for planetary bodies.

**audio** - Manages spatial and non-spatial audio playback using the SoLoud library, supporting 3D positioning, volume control, looping, and listener parameters for immersive sound experiences.

**base** - Core foundational module that provides essential rendering primitives like spheres, planes, trails, labels, coordinate axes, and screen space elements, along with basic transformations and lighting systems.

**cefwebgui** - Integrates Chromium Embedded Framework to provide web-based user interfaces, allowing HTML/CSS/JavaScript GUIs to be rendered within the OpenSpace environment.

**debugging** - Provides debugging tools and visualizations including performance statistics, frame information display, and debug rendering capabilities for development and troubleshooting.

**digitaluniverse** - Renders large-scale astronomical datasets from the Digital Universe catalog, displaying cosmic structures and phenomena as mesh-based visualizations.

**exoplanets** - Visualizes exoplanet data including orbital discs, habitable zones, host star properties, and comparison tools for exploring discovered planets beyond our solar system.

**fieldlines** - Renders magnetic or electric field lines from vector field data, supporting various seeding methods and field line tracing algorithms for scientific visualization.

**fieldlinessequence** - Extends fieldlines module to handle time-varying field line sequences, allowing animation of changing magnetic field structures over time.

**fitsfilereader** - Reads and processes FITS (Flexible Image Transport System) files commonly used in astronomy, supporting both image data and tabular data extraction.

**gaia** - Handles massive star catalogs from the Gaia space mission, using octree spatial structures for efficient rendering of millions of stars with proper filtering and culling.

**galaxy** - Renders galaxy structures using both volume rendering and point-based methods, supporting Milky Way visualization with stellar distributions and dust lanes.

**globebrowsing** - Provides interactive 3D globe rendering with multi-resolution tiling, layer management, and WMS server integration for displaying geographic and planetary data.

**imgui** - Implements immediate-mode GUI using Dear ImGui library, providing various interface components for property editing, scene management, and system controls.

**iswa** - Integrates with Integrated Space Weather Analysis system to visualize space weather data including solar wind, magnetic fields, and other heliospheric phenomena.

**kameleon** - Provides wrapper functionality for Kameleon library, enabling access to space physics simulation data and magnetospheric modeling results.

**kameleonvolume** - Extends kameleon module with volume rendering capabilities, allowing 3D visualization of magnetospheric and ionospheric simulation data.

**multiresvolume** - Implements multi-resolution volume rendering with adaptive level-of-detail, brick management, and error-based selection for large-scale volumetric datasets.

**opensoundcontrol** - Enables Open Sound Control (OSC) protocol communication for external device integration and remote control of OpenSpace parameters.

**postprocessing** - Provides post-processing rendering effects pipeline, allowing application of visual filters and enhancements to the final rendered output.

**server** - Implements server functionality for network communication, enabling distributed rendering and remote control capabilities.

**skybrowser** - Provides sky browsing functionality for navigating and exploring celestial coordinates and astronomical object catalogs.

**space** - Core space-related functionality including orbital mechanics, Kepler elements, ephemeris data handling, and SPICE toolkit integration for spacecraft trajectories.

**spacecraftinstruments** - Manages spacecraft instrument data visualization, handling coordinate frame transformations and instrument-specific rendering capabilities.

**spout** - Integrates Spout framework for real-time texture sharing between applications on Windows, enabling external video input/output capabilities.

**statemachine** - Implements state machine functionality for managing complex application states and transitions during presentations or automated sequences.

**sync** - Handles data synchronization and repository management for downloading and updating astronomical datasets and resources.

**telemetry** - Collects and manages telemetry data for monitoring system performance, usage statistics, and operational metrics.

**touch** - Provides multi-touch input handling and gesture recognition for interactive displays and touch-enabled devices.

**toyvolume** - Simple volume rendering module for testing and demonstration purposes, providing basic volumetric visualization capabilities.

**video** - Handles video file playback and streaming for incorporating multimedia content into space visualizations.

**volume** - Core volume rendering infrastructure providing transfer functions, raw volume data handling, and fundamental volumetric rendering capabilities.

**webbrowser** - Integrates web browser functionality for displaying web content and online resources within the OpenSpace environment.

**webgui** - Provides web-based graphical user interface through HTTP server, enabling browser-based control and interaction with OpenSpace.

#### Module Structure Pattern
Each module follows the standard pattern:
- `{module}module.h/cpp` - Main module class
- `include.cmake` - CMake configuration
- `rendering/` - Module-specific renderables
- `shaders/` - GLSL shaders

### Asset System
- **Assets** (`.asset` files) define scene content and data sources
- **Profiles** (`.profile` files) define complete scenes with assets and settings
- **Tasks** (`.task` files) define data processing and conversion operations

### Shader Architecture
- Shaders located in `shaders/` directory and module-specific `shaders/` subdirectories
- Uses HGLSL (header GLSL) for shared shader code
- PowerScaling system for astronomical distances in `shaders/PowerScaling/`

## Development Workflow

### Testing
```bash
# Build and run tests (if OPENSPACE_HAVE_TESTS=ON)
cmake --build build --target OpenSpaceTest --config RelWithDebInfo
./bin/RelWithDebInfo/OpenSpaceTest
```

### Running the Application
```bash
# From build directory or using built executable
./bin/RelWithDebInfo/OpenSpace
# Or with specific profile
./bin/RelWithDebInfo/OpenSpace --profile data/profiles/default.profile
```

### Module Development
When creating or modifying modules:
1. Each module must have a `{ModuleName}Module` class inheriting from `OpenSpaceModule`
2. Use the code generation system for Lua bindings (`_codegen.cpp` files)
3. Follow the existing module structure with CMakeLists.txt and include.cmake
4. Register renderables in the module's factory system

### Scripting
- Lua scripts for configuration and interaction
- Core scripts in `scripts/` directory
- Module-specific scripts in `modules/{module}/scripts/`
- Lua bindings auto-generated via codegen system

## Important Conventions

### Code Organization
- Header files typically use `.h` extension
- Implementation files use `.cpp` extension
- Generated code files use `_codegen.cpp` suffix
- Lua binding files use `_lua.inl` and `_lua_codegen.cpp` suffixes

### Build Targets
- Main application: `OpenSpace`
- Code generation: `run_codegen` (runs automatically)
- Tests: `OpenSpaceTest` (if enabled)
- Module libraries: `openspace-module-{modulename}`

### Configuration
- Main config: `openspace.cfg`
- User settings: `settings.json`
- Window/display configs in `config/` directory
- builds may take 10-15 minutes

## Rendering
- OpenSpace rendering is managed by SGCT: @rendering.md
  - In the Draw step, `RenderEngine::render()` is called, rendering objects in the scene, such as globes (planets) and stars
  - In the Draw2D step, `RenderEngine::renderOverlays()` is called. These are UI elements 