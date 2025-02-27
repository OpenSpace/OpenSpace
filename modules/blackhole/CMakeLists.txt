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
)

# Define Source Files
set(SOURCE_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/blackholemodule.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderableblackhole.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/rendering/viewport.cpp
)

# Define Shader Files
set(SHADER_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/blackhole_vs.glsl
  ${CMAKE_CURRENT_SOURCE_DIR}/shaders/blackhole_fs.glsl
)

# Organize Files into Groups
source_group("Header Files" FILES ${HEADER_FILES})
source_group("Source Files" FILES ${SOURCE_FILES})
source_group("Shader Files" FILES ${SHADER_FILES})

# Create OpenSpace Module
create_new_module(
  "BlackHole"
  black_hole_module
  STATIC
  ${HEADER_FILES}
  ${SOURCE_FILES}
  ${SHADER_FILES}
)

project(blackhole_cuda_prj CUDA)
find_package(CUDA REQUIRED)
# Define CUDA Files
set(CUDA_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/blackhole_cuda.cu
  ${CMAKE_CURRENT_SOURCE_DIR}/cuda/blackhole_cuda.h
)
source_group("CUDA Files" FILES ${CUDA_FILES})

find_package(CUDAToolkit REQUIRED)
include_directories(${CUDAToolkit_INCLUDE_DIRS})

if(NOT DEFINED CMAKE_CUDA_ARCHITECTURES)
    set(CMAKE_CUDA_ARCHITECTURES native)
endif()

# Debug Messages
message("CMAKE_CUDA_ARCHITECTURES: ${CMAKE_CUDA_ARCHITECTURES}")
message("CMAKE_CUDA_LIBRARY_ROOT: ${CUDAToolkit_LIBRARY_ROOT}")
message("CMAKE_CUDA_INCLUDE_DIRS: ${CUDAToolkit_INCLUDE_DIRS}")
message("CMAKE_CUDA_LIBRARY_DIR: ${CUDAToolkit_LIBRARY_DIR}")

message("ABC: ${CMAKE_CXX_FLAGS}")
message("ABC: ${CMAKE_CUDA_FLAGS}")

# Create CUDA Library
add_library(blackhole_cuda STATIC ${CUDA_FILES})
set_target_properties(blackhole_cuda PROPERTIES CUDA_SEPARABLE_COMPILATION ON)
set_target_properties(blackhole_cuda PROPERTIES CUDA_RESOLVE_DEVICE_SYMBOLS ON)
target_link_libraries(blackhole_cuda PRIVATE CUDA::toolkit CUDA::cudart)

target_link_libraries(openspace-module-blackhole PRIVATE blackhole_cuda)


get_target_property(_compile_flags blackhole_cuda COMPILE_FLAGS)
message("DEF: ${_compile_flags}")
