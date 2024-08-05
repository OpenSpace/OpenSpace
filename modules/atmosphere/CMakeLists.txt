##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2024                                                                #
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
  rendering/atmospheredeferredcaster.h
  rendering/renderableatmosphere.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  rendering/atmospheredeferredcaster.cpp
  rendering/renderableatmosphere.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/atmosphere_common.glsl
  shaders/atmosphere_deferred_vs.glsl
  shaders/atmosphere_deferred_fs.glsl
  shaders/calculation_gs.glsl
  shaders/calculation_vs.glsl
  shaders/deltaE_calc_fs.glsl
  shaders/deltaJ_calc_fs.glsl
  shaders/deltaS_calc_fs.glsl
  shaders/deltaS_sup_calc_fs.glsl
  shaders/inScattering_calc_fs.glsl
  shaders/inScattering_sup_calc_fs.glsl
  shaders/irradiance_calc_fs.glsl
  shaders/irradiance_final_fs.glsl
  shaders/irradiance_sup_calc_fs.glsl
  shaders/transmittance_calc_fs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Atmosphere"
  atmosphere_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
