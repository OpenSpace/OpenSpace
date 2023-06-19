##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2023                                                                #
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

set(HEADER_FILES
  rendering/renderablefieldlinessequence.h
  util/fieldlinesstate.h
  util/commons.h
  util/kameleonfieldlinehelper.h
  rendering/renderablecutplane.h
  util/gameravolumeslicer.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  rendering/renderablefieldlinessequence.cpp
  rendering/renderablecutplane.cpp
  util/fieldlinesstate.cpp
  util/commons.cpp
  util/kameleonfieldlinehelper.cpp
  util/gameravolumeslicer.cpp

)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/fieldlinessequence_vs.glsl
  shaders/fieldlinessequence_fs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "FieldlinesSequence"
  fieldlinessequence_module
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

#########################################################################################
# General settings
#########################################################################################

# Set FieldlineSequence paths
set(FIELDLINESEQUENCE_BASE_DIR "${PROJECT_SOURCE_DIR}/modules/fieldlinessequence")
set(FIELDLINESEQUENCE_SRC_DIR "${FIELDLINESEQUENCE_BASE_DIR}/src")
set(FIELDLINESEQUENCE_EXT_DIR "${FIELDLINESEQUENCE_BASE_DIR}/ext")

#########################################################################################
# External Third-party software options
#########################################################################################

#boost settings
set(Boost_USE_STATIC_LIBS ON)
set(Boost_USE_MULTITHREADED ON)  
set(Boost_USE_STATIC_RUNTIME ON) 

# If libraries needs to be built, make them shared
option(BUILD_SHARED_LIBS "Build Shared Libraries" ON)

# HDF5 options
option(HDF5_USE_STATIC_LIBRARIES "Use HDF5 static libraries" OFF)
option(FIELDLINESEQUENCE_USE_HDF5 "Build FieldlineSequence with HDF5 library" OFF)

# Highfive options
option(HIGHFIVE_USE_STATIC_LIBS "Use HighFive static libraries" OFF)
option(FIELDLINESEQUENCE_USE_HIGHFIVE "Build HighFive with zlib" OFF)

#########################################################################################
# External Third-party software
#########################################################################################

# Boost
find_package(Boost REQUIRED)
set(CCMC_DEPENDENT_LIBS ${CCMC_DEPENDENT_LIBS} ${Boost_LIBRARIES})

# HDF5
find_package(HDF5 COMPONENTS C CXX)
set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIR}")
set (LINK_LIBS ${LINK_LIBS} ${HDF5_C_${LIB_TYPE}_LIBRARY})

set (LIB_TYPE STATIC) # or SHARED
string(TOLOWER ${LIB_TYPE} SEARCH_TYPE)
if (FIELDLINESEQUENCE_USE_HDF5)

  if (HDF5_FOUND)
    message(STATUS "HDF5 Found: Building FieldlineSequence with HDF 5")
    add_definitions(-DFLS_HAVE_HDF5)
  # set(CCMC_DEPENDENT_LIBS ${CCMC_DEPENDENT_LIBS} ${HDF5_LIBRARIES})
  else ()
    add_definitions(-DFLS_HAVE_HDF5)
    message(STATUS "Building HDF5 locally")
   set(HDF5_ROOT ${FIELDLINESEQUENCE_EXT_DIR}/hdf5)
  set(HDF5_EXTERNALLY_CONFIGURED 1)
  set(HDF5_BUILD_CPP_LIB ON CACHE BOOL "Build HDF5 C++ Library" FORCE)
   # Add HDF5 submodule as a subdirectory
    add_subdirectory(${HDF5_ROOT})
    set(HDF5_INCLUDE_DIR ${HDF5_ROOT} ${HDF5_ROOT}/src ${HDF5_ROOT}/c++/src)
    set(HDF5_LIBRARIES hdf5)
    set(HDF5_LIBRARIES hdf5_cpp)

  endif ()
 endif ()

# HIGHFIVE
cmake_minimum_required(VERSION 3.1 FATAL_ERROR)

set(CMAKE_CXX_STANDARD 11)
add_definitions(-DHIGHFIVE_EXAMPLES=OFF)
set(HighFive_DIR "${FIELDLINESEQUENCE_EXT_DIR}/HighFive")

if(FIELDLINESEQUENCE_USE_HIGHFIVE)
   add_subdirectory("${HighFive_DIR}")
    target_link_libraries(${fieldlinessequence_module} PRIVATE HighFive) 
    #set_target_properties(HighFive PROPERTIES FOLDER "External")
    set(CCMC_DEPENDENT_LIBS ${CCMC_DEPENDENT_LIBS} ${HighFive_LIBRARIES})
endif ()

