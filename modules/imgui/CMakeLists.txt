##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2017                                                                #
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

# include(GenerateExportHeader)

set(HEADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/include/gui.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guicomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guifilepathcomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guiglobebrowsingcomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guihelpcomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guiorigincomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guiperformancecomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guiparallelcomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guipropertycomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guitimecomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/guiiswacomponent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/imgui_include.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/renderproperties.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/src/gui.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/gui_lua.inl
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guicomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guifilepathcomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guiglobebrowsingcomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guihelpcomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guiorigincomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guiperformancecomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guiparallelcomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guipropertycomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guitimecomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/guiiswacomponent.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/renderproperties.cpp

)
source_group("Source Files" FILES ${SOURCE_FILES})

set(SHADER_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gui_fs.glsl
    ${CMAKE_CURRENT_SOURCE_DIR}/shaders/gui_vs.glsl
)
source_group("Shader Files" FILES ${SHADER_FILES})

# set(openspace-module-imgui_LIBRARY_MODE SHARED)

create_new_module(
    "ImGUI"
    imgui_module
    STATIC
    ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

include_external_library(${imgui_module} Imgui ${CMAKE_CURRENT_SOURCE_DIR}/ext/imgui)

# generate_export_header(${imgui_module} EXPORT_FILE_NAME ${CMAKE_BINARY_DIR}/_generated/include/imgui_export.h)
