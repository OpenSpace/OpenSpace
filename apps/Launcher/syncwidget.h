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

class QGridLayout;

class SyncWidget : public QWidget {
Q_OBJECT
public:
    SyncWidget(QWidget* parent);
    ~SyncWidget();
    
    void setSceneFiles(QMap<QString, QString>  sceneFiles);

private slots:
    void syncButtonPressed();

private:
    void clear();
    QStringList selectedScenes() const;

    void handleDirectFiles(QString module, QStringList files);
    void handleTorrentFiles(QString module, QStringList torrents);

    QMap<QString, QString>  _sceneFiles;
    QGridLayout* _sceneLayout;
};

#endif // __SYNCWIDGET_H__
