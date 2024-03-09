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
  soundmodule.h
)
source_group("Header Files" FILES ${HEADER_FILES})

set(SOURCE_FILES
  soundmodule.cpp
  soundmodule_lua.inl
)
source_group("Source Files" FILES ${SOURCE_FILES})

create_new_module(
  "Sound"
  sound_module
  STATIC
  ${HEADER_FILES} ${SOURCE_FILES} ${SHADER_FILES}
)

begin_dependency("FMOD")
add_library(fmod SHARED IMPORTED)
target_include_directories(fmod SYSTEM INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/core/inc)
set_target_properties(fmod PROPERTIES IMPORTED_IMPLIB ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/core/lib/fmod_vc.lib)
set_target_properties(fmod PROPERTIES IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/core/lib/fmod.dll)
set_target_properties(fmod PROPERTIES IMPORTED_IMPLIB ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/core/lib/fmodL_vc.lib)
set_target_properties(fmod PROPERTIES IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/core/lib/fmodL.dll)

# target_include_directories(fmod SYSTEM PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/studio/inc)
# set_target_properties(fmod PROPERTIES IMPORTED_IMPLIB ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/studio/lib/fmodstudio_vc.lib)
# set_target_properties(fmod PROPERTIES IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/studio/lib/fmodstudio.dll)
# set_target_properties(fmod PROPERTIES IMPORTED_IMPLIB ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/studio/lib/fmodstudioL_vc.lib)
# set_target_properties(fmod PROPERTIES IMPORTED_LOCATION ${CMAKE_CURRENT_SOURCE_DIR}/ext/fmod/studio/lib/fmodstudioL.dll)

target_link_libraries(openspace-module-sound PRIVATE fmod)
end_dependency("FMOD")
