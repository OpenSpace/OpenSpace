##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2020                                                                #
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

include(${OPENSPACE_CMAKE_EXT_DIR}/set_openspace_compile_settings.cmake)

set(HEADER_FILES
  include/additionalscripts.h
  include/assets.h
  include/assettreeitem.h
  include/assettreemodel.h
  include/camera.h
  include/deltatimes.h
  include/filesystemaccess.h
  include/keybindings.h
  include/launcherwindow.h
  include/marknodes.h
  include/meta.h
  include/osmodules.h
  include/ostime.h
  include/profileedit.h
  include/properties.h
  include/ui_launcherwindow.h
  include/ui_osmodules.h
  include/ui_ostime.h
  include/ui_profileedit.h
  include/ui_properties.h
)

set(SOURCE_FILES
  src/additionalscripts.cpp
  src/assets.cpp
  src/assettreeitem.cpp
  src/assettreemodel.cpp
  src/camera.cpp
  src/deltatimes.cpp
  src/filesystemaccess.cpp
  src/keybindings.cpp
  src/launcherwindow.cpp
  src/marknodes.cpp
  src/meta.cpp
  src/osmodules.cpp
  src/ostime.cpp
  src/profileedit.cpp
  src/properties.cpp
)

find_package(Qt5 COMPONENTS Widgets REQUIRED)

set(MOC_FILES "")
qt5_wrap_cpp(
  MOC_FILES
  include/additionalscripts.h
  include/assets.h
  include/assettreemodel.h
  include/camera.h
  include/deltatimes.h
  include/keybindings.h
  include/launcherwindow.h
  include/marknodes.h
  include/meta.h
  include/osmodules.h
  include/ostime.h
  include/profileedit.h
  include/properties.h
)

set(RESOURCE_FILES "")
qt5_add_resources(RESOURCE_FILES resources/resources.qrc)

add_library(openspace-ui-launcher STATIC ${HEADER_FILES} ${SOURCE_FILES} ${MOC_FILES} ${RESOURCE_FILES})
set_openspace_compile_settings(openspace-ui-launcher)
target_include_directories(openspace-ui-launcher PUBLIC include)
target_link_libraries(openspace-ui-launcher PUBLIC Qt5::Core Qt5::Gui Qt5::Widgets openspace-core)

