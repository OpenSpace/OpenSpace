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
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>

#include <QCheckBox>
#include <QDebug>
#include <QDir>
#include <QGridLayout>
#include <QGroupBox>
#include <QPushButton>
#include <QString>
#include <QVBoxLayout>

namespace {
    const int nColumns = 3;

    const int DownloadApplicationVersion = 1;

    const std::string FileDownloadKey = "FileDownload";
    const std::string FileRequestKey = "FileRequest";
    const std::string TorrentFilesKey = "TorrentFiles";

    const std::string UrlKey = "URL";
    const std::string DestinationKey = "Destination";
    const std::string IdentifierKey = "Identifier";
    const std::string VersionKey = "Version";
}

SyncWidget::SyncWidget(QWidget* parent) 
    : QWidget(parent)
    , _sceneLayout(nullptr)
{
    setFixedSize(500, 500);

    QBoxLayout* layout = new QVBoxLayout;
    {
        QGroupBox* sceneBox = new QGroupBox;
        _sceneLayout = new QGridLayout;
        sceneBox->setLayout(_sceneLayout);
        layout->addWidget(sceneBox);
    }
    {
        QPushButton* syncButton = new QPushButton("Synchronize Scenes");
        QObject::connect(
            syncButton, SIGNAL(clicked(bool)),
            this, SLOT(syncButtonPressed())
        );

        layout->addWidget(syncButton);
    }

    setLayout(layout);

    ghoul::initialize();
    openspace::DownloadManager::initialize("http://openspace.itn.liu.se/data/request", DownloadApplicationVersion);
}

SyncWidget::~SyncWidget() {
    openspace::DownloadManager::deinitialize();
    ghoul::deinitialize();
}

void SyncWidget::setSceneFiles(QMap<QString, QString> sceneFiles) {
    _sceneFiles = std::move(sceneFiles);
    qDebug() << _sceneFiles;
    QStringList keys = _sceneFiles.keys();
    qDebug() << keys;
    for (int i = 0; i < keys.size(); ++i) {
        const QString& sceneName = keys[i];

        QCheckBox* checkbox = new QCheckBox(sceneName);

        _sceneLayout->addWidget(checkbox, i / nColumns, i % nColumns);
    }
}

void SyncWidget::setModulesDirectory(QString modulesDirectory) {
    _modulesDirectory = std::move(modulesDirectory);
}

void SyncWidget::clear() {

}

void SyncWidget::handleDirectFiles(QString module, DirectFiles files) {
    qDebug() << "Direct Files";
    for (const DirectFile& f : files) {
        qDebug() << f.url << " -> " << f.destination;

        DlManager.downloadFile(
            f.url.toStdString(),
            fullPath(module, f.destination).toStdString(),
            [](const ghoul::filesystem::File& f) { qDebug() << QString::fromStdString(f.filename()) << "finished";}
        );
    }
}

void SyncWidget::handleFileRequest(QString module, FileRequests files) {
    qDebug() << "File Requests";
    for (const FileRequest& f : files) {
        qDebug() << f.identifier << " (" << f.version << ")" << " -> " << f.destination;
        DlManager.downloadRequestFiles(
            f.identifier.toStdString(),
            fullPath(module, f.destination).toStdString(),
            f.version,
            [](const ghoul::filesystem::File& f) { qDebug() << "finished"; }
        );


    }
}

void SyncWidget::handleTorrentFiles(QString module, TorrentFiles files) {
    qDebug() << "Torrent Files";
    for (const TorrentFile& f : files) {
        qDebug() << f.file;
    }
}

void SyncWidget::syncButtonPressed() {
    clear();

    for (const QString& scene : selectedScenes()) {
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
        for (int i = 1; i <= modules.size(); ++i) {
            std::string module = modules.value<std::string>(std::to_string(i));
            modulesList.append(QString::fromStdString(module));
        }
        qDebug() << modulesList;

        QDir sceneDir(scene);
        sceneDir.cdUp();
        for (QString module : modulesList) {
            QString dataFile = sceneDir.absoluteFilePath(module + "/" + module + ".data");

            qDebug() << module;
            qDebug() << dataFile << QFileInfo(dataFile).exists();

            if (QFileInfo(dataFile).exists()) {
                ghoul::Dictionary dataDictionary;
                ghoul::lua::loadDictionaryFromFile(dataFile.toStdString(), dataDictionary);

                ghoul::Dictionary directDownloadFiles;
                ghoul::Dictionary fileRequests;
                ghoul::Dictionary torrentFiles;

                bool found = dataDictionary.getValue<ghoul::Dictionary>(FileDownloadKey, directDownloadFiles);
                if (found) {
                    DirectFiles files;
                    //QStringList files;
                    for (int i = 1; i <= directDownloadFiles.size(); ++i) {
                        ghoul::Dictionary d = directDownloadFiles.value<ghoul::Dictionary>(std::to_string(i));
                        std::string url = d.value<std::string>(UrlKey);
                        std::string dest = d.value<std::string>(DestinationKey);

                        files.append({
                            QString::fromStdString(url),
                            QString::fromStdString(dest)
                        });
                    }
                    handleDirectFiles(module, files);
                }

                found = dataDictionary.getValue<ghoul::Dictionary>(FileRequestKey, fileRequests);
                if (found) {
                    FileRequests files;
                    for (int i = 1; i <= fileRequests.size(); ++i) {
                        ghoul::Dictionary d = fileRequests.value<ghoul::Dictionary>(std::to_string(i));
                        std::string url = d.value<std::string>(IdentifierKey);
                        std::string dest = d.value<std::string>(DestinationKey);
                        int version = static_cast<int>(d.value<double>(VersionKey));


                        files.append({
                            QString::fromStdString(url),
                            QString::fromStdString(dest),
                            version
                        });
                    }
                    handleFileRequest(module, files);
                }

                found = dataDictionary.getValue<ghoul::Dictionary>(TorrentFilesKey, torrentFiles);
                if (found) {
                    TorrentFiles torrents;
                    for (int i = 1; i <= torrentFiles.size(); ++i) {
                        std::string f = torrentFiles.value<std::string>(std::to_string(i));

                        torrents.append({QString::fromStdString(f)});
                    }
                    handleTorrentFiles(module, torrents);
                }
            }
        }
    }
}

QStringList SyncWidget::selectedScenes() const {
    QStringList result;
    int nChildren = _sceneLayout->count();
    for (int i = 0; i < nChildren; ++i) {
        QWidget* w = _sceneLayout->itemAt(i)->widget();
        QCheckBox* c = static_cast<QCheckBox*>(w);
        if (c->isChecked()) {
            QString t = c->text();
            result.append(_sceneFiles[t]);
        }
    }
    qDebug() << result;
    return result;
}

QString SyncWidget::fullPath(QString module, QString destination) const {
    return _modulesDirectory + "/" + module + "/" + destination;
}
