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

#ifndef __SYNCWIDGET_H__
#define __SYNCWIDGET_H__

#include <QWidget>

#include <QMap>

#include <libtorrent/torrent_handle.hpp>

//#include <thread>

class QBoxLayout;
class QGridLayout;

class InfoWidget;

namespace libtorrent {
    class session;
    struct torrent_handle;
}

class SyncWidget : public QWidget {
Q_OBJECT
public:
    SyncWidget(QWidget* parent, Qt::WindowFlags f = 0);
    ~SyncWidget();
    
    void setSceneFiles(QMap<QString, QString> sceneFiles);
    void setModulesDirectory(QString modulesDirectory);

private slots:
    void syncButtonPressed();
    void handleTimer();

private:
    struct DirectFile {
        QString module;
        QString url;
        QString destination;
    };

    struct FileRequest {
        QString module;
        QString identifier;
        QString destination;
        int version;
    };

    struct TorrentFile {
        QString module;
        QString file;
    };

    void clear();
    QStringList selectedScenes() const;

    QString fullPath(QString module, QString destination) const;

    void handleDirectFiles();
    void handleFileRequest();
    void handleTorrentFiles();

    QMap<QString, QString>  _sceneFiles;
    QString _modulesDirectory;
    QGridLayout* _sceneLayout;
    QBoxLayout* _downloadLayout;

    libtorrent::session* _session;
    QMap<libtorrent::torrent_handle, InfoWidget*> _torrentInfoWidgetMap;

    QList<DirectFile> _directFiles;
    QList<FileRequest> _fileRequests;
    QList<TorrentFile> _torrentFiles;
};

#endif // __SYNCWIDGET_H__
