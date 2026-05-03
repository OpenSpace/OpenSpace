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

include(${PROJECT_SOURCE_DIR}/support/cmake/application_definition.cmake)

set(HEADER_FILES
  include/asseteditorwidget.h
  include/contentslistwidget.h
  include/dependencieswidget.h
  include/documentation.h
  include/documentationpanel.h
  include/editorpanel.h
  include/identifierregistry.h
  include/jasset.h
  include/mainwindow.h
  include/metadatawidget.h
  include/path.h
  include/scenegraphnodeeditor.h
  include/sidepanel.h
  include/welcomedialog.h
  include/form/collapsiblesection.h
  include/form/colorwidget.h
  include/form/matrixwidget.h
  include/form/schemaformwidget.h
  include/form/searchdropdown.h
  include/schema/assetschema.h
)

set(SOURCE_FILES
  src/asseteditorwidget.cpp
  src/contentslistwidget.cpp
  src/dependencieswidget.cpp
  src/documentationpanel.cpp
  src/editorpanel.cpp
  src/identifierregistry.cpp
  src/jasset.cpp
  src/mainwindow.cpp
  src/metadatawidget.cpp
  src/path.cpp
  src/scenegraphnodeeditor.cpp
  src/sidepanel.cpp
  src/welcomedialog.cpp
  src/form/collapsiblesection.cpp
  src/form/colorwidget.cpp
  src/form/matrixwidget.cpp
  src/form/schemaformwidget.cpp
  src/form/searchdropdown.cpp
  src/schema/assetschema.cpp
)

find_package(Qt6 COMPONENTS Core Widgets REQUIRED)
qt6_wrap_cpp(MOC_FILES ${HEADER_FILES})
qt6_add_resources(RESOURCE_FILES resources/resources.qrc)

create_new_application(AssetBuilder
  main.cpp
  ${HEADER_FILES}
  ${SOURCE_FILES}
  ${MOC_FILES}
  ${RESOURCE_FILES}
)

target_include_directories(AssetBuilder PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)

target_link_libraries(AssetBuilder PRIVATE
  openspace-core
  openspace-module-collection
  Qt6::Core
  Qt6::Gui
  Qt6::Widgets
)

target_precompile_headers(AssetBuilder PRIVATE
  <QObject>
  <QWidget>
  <QDialog>
  <QJsonDocument>
  <QJsonObject>
  <QJsonArray>
  <filesystem>
  <optional>
  <string>
  <vector>
)

