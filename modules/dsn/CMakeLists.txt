##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2018                                                                #
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

include(${OPENSPACE_CMAKE_EXT_DIR}/module_definition.cmake)

set(HEADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecone.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablestationfov.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesignals.h
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabledsnlabels.h
    ${CMAKE_CURRENT_SOURCE_DIR}/translation/radectranslation.h
    ${CMAKE_CURRENT_SOURCE_DIR}/managers/signalmanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/managers/radecmanager.h
    ${CMAKE_CURRENT_SOURCE_DIR}/managers/datafilehelper.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablecone.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablestationfov.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderablesignals.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rendering/renderabledsnlabels.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/translation/radectranslation.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/managers/signalmanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/managers/radecmanager.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/managers/datafilehelper.cpp
)
source_group("Source Files" FILES ${SOURCE_FILES})


set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderablesignals_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderablesignals_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderablestationfov_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderablestationfov_vs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderablecone_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/renderablecone_vs.glsl  
)
source_group("Shader Files" FILES ${SHADER_FILES})

create_new_module(
    "Dsn"
    dsn_module
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)
