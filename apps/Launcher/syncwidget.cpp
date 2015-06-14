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

#include "infowidget.h"

#include <openspace/engine/downloadmanager.h>

#include <ghoul/ghoul.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/lua/ghoul_lua.h>

#include <QApplication>
#include <QCheckBox>
#include <QDebug>
#include <QDir>
#include <QFileInfo>
#include <QGridLayout>
#include <QGroupBox>
#include <QPushButton>
#include <QScrollArea>
#include <QString>
#include <QThread>
#include <QTimer>
#include <QVBoxLayout>

#include <libtorrent/entry.hpp>
#include <libtorrent/bencode.hpp>
#include <libtorrent/session.hpp>
#include <libtorrent/alert_types.hpp>

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

    const QString DefaultSceneName = "default.scene";
}

SyncWidget::SyncWidget(QWidget* parent, Qt::WindowFlags f) 
    : QWidget(parent, f)
    , _sceneLayout(nullptr)
    , _session(new libtorrent::session)
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

    {
        QWidget* downloadBox = new QGroupBox;
        downloadBox->setMinimumSize(450, 1000);
        _downloadLayout = new QVBoxLayout;
        downloadBox->setLayout(_downloadLayout);

        QScrollArea* area = new QScrollArea;
        area->setWidget(downloadBox);

        layout->addWidget(area);
    }

    setLayout(layout);

    ghoul::initialize();
    openspace::DownloadManager::initialize("http://openspace.itn.liu.se/data/request", DownloadApplicationVersion);

    libtorrent::error_code ec;
    _session->listen_on(std::make_pair(20285, 20285), ec);
    if (ec) {
        qDebug() << "Failed to open socket: " << QString::fromStdString(ec.message());
        _session = nullptr;
        return;
    }
    _session->start_upnp();
    _session->start_dht();
    

    QTimer* timer = new QTimer(this);
    QObject::connect(timer, SIGNAL(timeout()), this, SLOT(handleTimer()));
    timer->start(100);
}

SyncWidget::~SyncWidget() {
    openspace::DownloadManager::deinitialize();
    ghoul::deinitialize();
    delete _session;
}

void SyncWidget::setSceneFiles(QMap<QString, QString> sceneFiles) {
    _sceneFiles = std::move(sceneFiles);
    qDebug() << _sceneFiles;
    QStringList keys = _sceneFiles.keys();
    qDebug() << keys;
    for (int i = 0; i < keys.size(); ++i) {
        const QString& sceneName = keys[i];

        QCheckBox* checkbox = new QCheckBox(sceneName);

        if (sceneName == DefaultSceneName)
            checkbox->setChecked(true);

        _sceneLayout->addWidget(checkbox, i / nColumns, i % nColumns);
    }
}

void SyncWidget::setModulesDirectory(QString modulesDirectory) {
    _modulesDirectory = std::move(modulesDirectory);
}

void SyncWidget::clear() {

}

void SyncWidget::handleDirectFiles() {
    qDebug() << "Direct Files";
    for (const DirectFile& f : _directFiles) {
        InfoWidget* w = new InfoWidget(f.destination);
        _downloadLayout->addWidget(w);
        //_stringInfoWidgetMap[f.destination] = w;

        qDebug() << f.url << " -> " << f.destination;

        auto finishedCallback =
            [w](const ghoul::filesystem::File& f) {
                qDebug() << QString::fromStdString(f.filename()) << "finished";
                delete w;
                qApp->processEvents();
            };
        auto progressCallback =
            [w](const ghoul::filesystem::File& f, float progress) {
                qDebug() << QString::fromStdString(f.filename()) << ": " << progress;
                w->update(progress);
                qApp->processEvents();
            };

        DlManager.downloadFile(
            f.url.toStdString(),
            fullPath(f.module, f.destination).toStdString(),
            finishedCallback,
            progressCallback
        );
    }
}

void SyncWidget::handleFileRequest() {
    qDebug() << "File Requests";
    for (const FileRequest& f : _fileRequests) {
        InfoWidget* w = new InfoWidget(f.identifier, -1);
        _downloadLayout->addWidget(w);

        auto finishedCallback =
            [w](const ghoul::filesystem::File& f) {
            qDebug() << QString::fromStdString(f.filename()) << "finished";
            delete w;
            qApp->processEvents();
        };

        auto progressCallback =
            [w](const ghoul::filesystem::File& f, float progress) {
            qDebug() << QString::fromStdString(f.filename()) << ": " << progress;
            w->update(progress);
            qApp->processEvents();
        };

        qDebug() << f.identifier << " (" << f.version << ")" << " -> " << f.destination;

        std::string identifier =  f.identifier.toStdString();
        std::string path = fullPath(f.module, f.destination).toStdString();
        int version = f.version;
        DlManager.downloadRequestFiles(
            identifier,
            path,
            version,
            finishedCallback,
            progressCallback
        );
    }
}

void SyncWidget::handleTorrentFiles() {
    qDebug() << "Torrent Files";
    for (const TorrentFile& f : _torrentFiles) {
        QString file = QString::fromStdString(absPath(fullPath(f.module, f.file).toStdString()));
        qDebug() << file;

        if (!QFileInfo(file).exists()) {
            qDebug() << file << " does not exist";
            continue;
        }

        libtorrent::error_code ec;
        libtorrent::add_torrent_params p;
        p.save_path = absPath(fullPath(f.module, ".").toStdString());
        qDebug() << QString::fromStdString(p.save_path);
        p.ti = new libtorrent::torrent_info(file.toStdString(), ec);
        p.name = f.file.toStdString();
        p.storage_mode = libtorrent::storage_mode_allocate;
        p.auto_managed = true;
        if (ec) {
            qDebug() << QString::fromStdString(ec.message());
            continue;
        }
        libtorrent::torrent_handle h = _session->add_torrent(p, ec);
        if (ec) {
            qDebug() << QString::fromStdString(ec.message());
            continue;
        }

        libtorrent::size_type s = h.status().total_wanted;
        InfoWidget* w = new InfoWidget(f.file, h.status().total_wanted);
        _downloadLayout->addWidget(w);
        _torrentInfoWidgetMap[h] = w;
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
                    for (int i = 1; i <= directDownloadFiles.size(); ++i) {
                       if (!directDownloadFiles.hasKeyAndValue<ghoul::Dictionary>(std::to_string(i))) {
                           qDebug() << QString::fromStdString(FileDownloadKey) << " is not a dictionary";
                           continue;
                        }
                        ghoul::Dictionary d = directDownloadFiles.value<ghoul::Dictionary>(std::to_string(i));
                        if (!directDownloadFiles.hasKeyAndValue<std::string>(UrlKey)) {
                            qDebug() << "No '" << QString::fromStdString(UrlKey);
                        }
                        std::string url = d.value<std::string>(UrlKey);
                        if (!directDownloadFiles.hasKeyAndValue<std::string>(DestinationKey)) {
                            qDebug() << "No '" << QString::fromStdString(DestinationKey);
                        }
                        std::string dest = d.value<std::string>(DestinationKey);

                        _directFiles.append({
                            module,
                            QString::fromStdString(url),
                            QString::fromStdString(dest)
                        });
                    }
                }

                found = dataDictionary.getValue<ghoul::Dictionary>(FileRequestKey, fileRequests);
                if (found) {
                    for (int i = 1; i <= fileRequests.size(); ++i) {
                        ghoul::Dictionary d = fileRequests.value<ghoul::Dictionary>(std::to_string(i));
                        std::string url = d.value<std::string>(IdentifierKey);
                        std::string dest = d.value<std::string>(DestinationKey);
                        int version = static_cast<int>(d.value<double>(VersionKey));


                        _fileRequests.append({
                            module,
                            QString::fromStdString(url),
                            QString::fromStdString(dest),
                            version
                        });
                    }
                }

                found = dataDictionary.getValue<ghoul::Dictionary>(TorrentFilesKey, torrentFiles);
                if (found) {
                    for (int i = 1; i <= torrentFiles.size(); ++i) {
                        std::string f = torrentFiles.value<std::string>(std::to_string(i));

                        _torrentFiles.append({
                            module,
                            QString::fromStdString(f)
                        });
                    }
                }
            }
        }
    }

    //// Make the lists unique
    //{
    //    QSet<DirectFile> s = _directFiles.toSet();
    //    _directFiles = QList<DirectFile>::fromSet(s);
    //}
    //{
    //    QSet<FileRequest> s = _fileRequests.toSet();
    //    _fileRequests = QList<FileRequest>::fromSet(s);
    //}
    //{
    //    QSet<TorrentFile> s = _torrentFiles.toSet();
    //    _torrentFiles = QList<TorrentFile>::fromSet(s);
    //}

    handleDirectFiles();
    handleFileRequest();
    handleTorrentFiles();
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

void SyncWidget::handleTimer() {
    using namespace libtorrent;

    //_session->post_torrent_updates();
    //libtorrent::session_settings settings = _session->settings();

    //qDebug() << "Session";
    //qDebug() << "nPeers: " << _session->status().num_peers;
    //qDebug() << "DHT: " << _session->is_dht_running();
    //qDebug() << "Incoming TCP" << settings.enable_incoming_tcp;
    //qDebug() << "Outgoing TCP" << settings.enable_outgoing_tcp;
    //qDebug() << "Incoming UTP" << settings.enable_incoming_utp;
    //qDebug() << "Outgoing UTP" << settings.enable_outgoing_utp;
    //qDebug() << "===";

    //qDebug() << "Alerts";
    //std::deque<alert*> alerts;
    //_session->pop_alerts(&alerts);
    //for (alert* a : alerts) {
    //    qDebug() << QString::fromStdString(a->message());

    //    //if (a->category() == alert::status_notification) {
    //    //    state_update_alert* sua = static_cast<state_update_alert*>(a);
    //    //    for (torrent_status s )
    //    //}
    //}
    //qDebug() << "===";


    std::vector<torrent_handle> handles = _session->get_torrents();
    for (torrent_handle h : handles) {
        torrent_status s = h.status();
        InfoWidget* w = _torrentInfoWidgetMap[h];

        w->update(static_cast<int>(s.total_wanted_done));

    //    qDebug() << "Name: " << QString::fromStdString(h.name());
    //    //torrent_status s = h.status();

    //    qDebug() << "Error: " << QString::fromStdString(s.error);

    //    qDebug() << "Total Wanted: " << s.total_wanted;
    //    qDebug() << "Total Wanted Done: " << s.total_wanted_done;
    //    qDebug() << "Has Incoming: " << s.has_incoming;
    //    qDebug() << "Connect Candidates: " << s.connect_candidates;
    //    qDebug() << "Last Seen Complete: " << s.last_seen_complete;
    //    qDebug() << "List Peers: " << s.list_peers;
    //    qDebug() << "List Seeds: " << s.list_seeds;
    //    qDebug() << "Num Pieces: " << s.num_pieces;
    //    qDebug() << "Download Rate: " << s.download_rate;
    //    qDebug() << "List Seeds: " << s.list_seeds;
    //    qDebug() << "Paused: " << s.paused;
    //    qDebug() << "Progress: " << s.progress;

    //    qDebug() << "";
    }
}
