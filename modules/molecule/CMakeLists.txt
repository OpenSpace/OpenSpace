##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
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
  moleculemodule.h
  src/renderablemolecule.h
  src/renderablesimulationbox.h
  src/cache.h
  src/coloring.h
  src/def.h
  src/loader.h
  src/postprocessing.h
  src/util.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  moleculemodule.cpp
  src/renderablemolecule.cpp
  src/renderablesimulationbox.cpp
  src/cache.cpp
  src/coloring.cpp
  src/loader.cpp
  src/postprocessing.cpp
  src/util.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
  shaders/billboard_vs.glsl
  shaders/billboard_fs.glsl
  shaders/blit_fs.glsl
  shaders/blit_depth_fs.glsl
  shaders/deferred_shading_fs.glsl
  shaders/dof_fs.glsl
  shaders/dof_halfres_prepass_fs.glsl
  shaders/fxaa_fs.glsl
  shaders/linearize_depth_fs.glsl
  shaders/quad_vs.glsl
  shaders/sharpen_fs.glsl
  shaders/ssao_blur_fs.glsl
  shaders/ssao_fs.glsl
  shaders/temporal_aa_fs.glsl
  shaders/tonemap_aces_fs.glsl
  shaders/tonemap_exposure_gamma_fs.glsl
  shaders/tonemap_filmic_fs.glsl
  shaders/tonemap_passthrough_fs.glsl
  shaders/vel_neighbormax_fs.glsl
  shaders/vel_tilemax_fs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
  "Molecule"
  molecule_module
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

begin_dependency("mold")
find_package(mold CONFIG REQUIRED)
target_link_libraries(${molecule_module} PRIVATE mold::mdlib)

# mold's OpenGL backend compiles its GLSL at runtime and, per the overlay port's
# runtime-shader-path.patch, looks for it at <exe_dir>/../modules/molecule/shaders/mold
# (or one level further up for a Windows multi-config build tree). The shaders ship inside
# the vcpkg package, so copy them into that location. This is a configure-time copy rather
# than a build step because the source is an immutable installed package.
file(COPY "${MOLD_SHADER_DIR}/" DESTINATION "${CMAKE_CURRENT_SOURCE_DIR}/shaders/mold")
end_dependency("mold")
