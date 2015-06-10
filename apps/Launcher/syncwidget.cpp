/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include "syncwidget.h"

#include <openspace/engine/downloadmanager.h>

#include <ghoul/ghoul.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>

#include <QDebug>
#include <QDir>
#include <QString>

namespace {
}

SyncWidget::SyncWidget(QWidget* parent) 
    : QWidget(parent)
{
    setFixedSize(500, 500);

    ghoul::initialize();

    openspace::DownloadManager::initialize("http://openspace.itn.liu.se/request.cgi");
}

SyncWidget::~SyncWidget() {
    openspace::DownloadManager::deinitialize();
    ghoul::deinitialize();
}

void SyncWidget::setSceneFile(QString scene) {
    clear();

    qDebug() << scene;

    ghoul::Dictionary sceneDictionary;
    ghoul::lua::loadDictionaryFromFile(
        scene.toStdString(),
        sceneDictionary
    );

    ghoul::Dictionary modules;
    bool success = sceneDictionary.getValue<ghoul::Dictionary>("Modules", modules);
    qDebug() << success;

    QStringList modulesList;
    for (int i = 1; i < modules.size(); ++i) {
        std::string module = modules.value<std::string>(std::to_string(i));
        modulesList.append(QString::fromStdString(module));
    }
    qDebug() << modulesList;

    QDir sceneDir(scene);
    sceneDir.cdUp();
    for (QString module : modulesList) {
        QString moduleFile = sceneDir.absoluteFilePath(module + "/" + module + ".mod");
        QString dataFile = sceneDir.absoluteFilePath(module + "/" + module + ".data");

        qDebug() << module;
        qDebug() << moduleFile << QFileInfo(moduleFile).exists();
        qDebug() << dataFile << QFileInfo(dataFile).exists();

        if (QFileInfo(dataFile).exists()) {
            ghoul::Dictionary dataDictionary;
            ghoul::lua::loadDictionaryFromFile(dataFile.toStdString(), dataDictionary);

            ghoul::Dictionary directFiles;
            ghoul::Dictionary torrentFiles;

            bool found = dataDictionary.getValue<ghoul::Dictionary>("Files", directFiles);
            if (found) {
                QStringList files;
                for (int i = 1; i < directFiles.size(); ++i) {
                    std::string f = directFiles.value<std::string>(std::to_string(i));
                    files.append(QString::fromStdString(f));
                }
                handleDirectFiles(module, files);
            }
            
            found = dataDictionary.getValue<ghoul::Dictionary>("Torrents", torrentFiles);
            if (found) {
                QStringList torrents;
                for (int i = 1; i < torrentFiles.size(); ++i) {
                    std::string f = torrentFiles.value<std::string>(std::to_string(i));
                    torrents.append(QString::fromStdString(f));
                }
                handleTorrentFiles(module, torrents);
            }
        }
    }
}

void SyncWidget::clear() {


}

void SyncWidget::handleDirectFiles(QString module, QStringList files) {
    qDebug() << files;
}

void SyncWidget::handleTorrentFiles(QString module, QStringList torrents) {
    qDebug() << torrents;
}
