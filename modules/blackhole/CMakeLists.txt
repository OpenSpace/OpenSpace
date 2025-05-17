##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2025                                                                #
#                                                                                        #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
#                                                                                        #
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
#                                                                                        #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
##########################################################################################

include(${PROJECT_SOURCE_DIR}/support/cmake/module_definition.cmake)

# Define Header Files
set(HEADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/blackholemodule.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableblackhole.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/viewport.h
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/kdtree.h
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/schwarzschild.h
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/kerr.h
)

# Define Source Files
set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/blackholemodule.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableblackhole.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/viewport.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/kdtree.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/schwarzschild.cu
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/kerr.cu
)

# Define Shader Files
set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/schwarzschild_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/schwarzschild_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/kerr_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/kerr_fs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/stars.glsl
)

# Define Shader Files
set(CUDA_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/schwarzschild.h
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/schwarzschild.cu
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/kerr.h
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/kerr.cu
)

# Organize Files into Groups
source_group("Header Files" FILES ${HEADER_FILES})
source_group("Source Files" FILES ${SOURCE_FILES})
source_group("Shader Files" FILES ${SHADER_FILES})
source_group("CUDA Files" FILES ${CUDA_FILES})

set(CMAKE_CUDA_PROPAGATE_HOST_FLAGS OFF CACHE BOOL "" FORCE)

# Create OpenSpace Module (merged with CUDA)
create_new_module(
  "BlackHole"
  black_hole_module
  STATIC
  ${HEADER_FILES}
  ${SOURCE_FILES}
  ${SHADER_FILES}
  ${CUDA_FILES}
)

# Ensure CUDA is found
enable_language(CUDA)
find_package(CUDAToolkit REQUIRED)
include_directories(${CUDAToolkit_INCLUDE_DIRS})

# Get the initial COMPILE_OPTIONS
get_target_property(initial_compile_options openspace-module-blackhole COMPILE_OPTIONS)

# Set COMPILE_OPTIONS conditionally for C++ sources
set_target_properties(openspace-module-blackhole PROPERTIES
  COMPILE_OPTIONS "$<$<COMPILE_LANGUAGE:CXX>:${initial_compile_options}>"
)

if(NOT DEFINED CMAKE_CUDA_ARCHITECTURES)
  set(CMAKE_CUDA_ARCHITECTURES native)
endif()

# Tell the module to link against CUDA runtime/toolkit
target_link_libraries(openspace-module-blackhole
  PRIVATE CUDA::cudart CUDA::toolkit
)
