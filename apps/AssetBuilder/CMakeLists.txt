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
    include/mainwindow.h
    include/welcomedialog.h
    include/asseteditorwidget.h
    include/sidepanel.h
    include/contentslistwidget.h
    include/dependencieswidget.h
    include/metadatawidget.h
    include/documentationpanel.h
    include/editorpanel.h
    include/scenegraphnodeeditor.h
    include/identifierregistry.h
    include/scrollanchor.h
    include/utils.h
    include/form/collapsiblesection.h
    include/form/schemaformwidget.h
    include/form/searchdropdown.h
    include/form/matrixwidget.h
    include/form/colorwidget.h
    include/documentation.h
    include/schema/assetschema.h
    include/jasset.h
)

set(SOURCE_FILES
    src/mainwindow.cpp
    src/welcomedialog.cpp
    src/jasset.cpp
    src/asseteditorwidget.cpp
    src/sidepanel.cpp
    src/contentslistwidget.cpp
    src/dependencieswidget.cpp
    src/metadatawidget.cpp
    src/documentationpanel.cpp
    src/editorpanel.cpp
    src/scenegraphnodeeditor.cpp
    src/identifierregistry.cpp
    src/scrollanchor.cpp
    src/utils.cpp
    src/form/collapsiblesection.cpp
    src/form/schemaformwidget.cpp
    src/form/schemaformwidget_flat.cpp
    src/form/schemaformwidget_table.cpp
    src/form/searchdropdown.cpp
    src/form/matrixwidget.cpp
    src/form/colorwidget.cpp
    src/schema/assetschema.cpp
)

# Qt version detection
find_package(QT NAMES Qt6 Qt5 COMPONENTS Core Widgets REQUIRED)
set(MOC_FILES "")
set(RESOURCE_FILES "")

if (${QT_VERSION_MAJOR} EQUAL 5)
    find_package(Qt5 COMPONENTS Core Widgets REQUIRED)
    qt5_wrap_cpp(MOC_FILES ${HEADER_FILES})
    qt5_add_resources(RESOURCE_FILES resources/resources.qrc)
elseif (${QT_VERSION_MAJOR} EQUAL 6)
    find_package(Qt6 COMPONENTS Core Widgets REQUIRED)
    qt6_wrap_cpp(MOC_FILES ${HEADER_FILES})
    qt6_add_resources(RESOURCE_FILES resources/resources.qrc)
elseif (NOT DEFINED QT_VERSION_MAJOR)
    message(FATAL_ERROR "Unable to find Qt version")
else ()
    message(FATAL_ERROR "Unsupported Qt version")
endif ()

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
    Qt${QT_VERSION_MAJOR}::Core
    Qt${QT_VERSION_MAJOR}::Gui
    Qt${QT_VERSION_MAJOR}::Widgets
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

