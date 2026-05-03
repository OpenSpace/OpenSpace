/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include "mainwindow.h"
#include "schema/assetschema.h"
#include <openspace/engine/configuration.h>
#include <openspace/engine/settings.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <QApplication>
#include <QFile>
#include <QMessageBox>

using namespace openspace;

int main(int argc, char* argv[]) {
    ghoul::logging::LogManager::initialize(
        ghoul::logging::LogLevel::Debug,
        ghoul::logging::LogManager::ImmediateFlush::Yes
    );

    ghoul::filesystem::FileSystem::initialize();

    // Register the path of the executable, to make it possible to find other files in the
    // same directory
    FileSys.registerPathToken(
        "${BIN}",
        std::filesystem::current_path() / std::filesystem::path(argv[0]).parent_path(),
        ghoul::filesystem::FileSystem::Override::Yes
    );

    std::filesystem::path cfgPath = findConfiguration();
    Configuration configuration = loadConfigurationFromFile(cfgPath, findSettings());
    registerPathTokens(configuration);


    QApplication app = QApplication(argc, argv);

    QApplication::setApplicationName("AssetBuilder");
    QApplication::setApplicationVersion("1.0.0");
    QApplication::setOrganizationName("OpenSpace");

    if (QFile style = QFile(":/qss/assetbuilder.qss");  style.open(QFile::ReadOnly)) {
        app.setStyleSheet(QLatin1String(style.readAll()));
    }

    AssetSchema::instance();

    MainWindow window;
    window.showMaximized();
    return app.exec();
}
