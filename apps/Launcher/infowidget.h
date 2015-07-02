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

#ifndef __INFOWIDGET_H__
#define __INFOWIDGET_H__

//#include <QWidget>
#include <QGroupBox>

#include <openspace/engine/downloadmanager.h>

#include <libtorrent/torrent_handle.hpp>

class QLabel;
class QProgressBar;

class InfoWidget : public QGroupBox {
Q_OBJECT
public:
    InfoWidget(QString name, int totalBytes = -1);

    void update(openspace::DownloadManager::FileFuture* f);
    void update(libtorrent::torrent_status s);

    void error(QString message);

private:
    InfoWidget(const InfoWidget& rhs) = delete;

    QLabel* _name;
    QLabel* _bytes;
    QProgressBar* _progress;
    QLabel* _messagesLeft;
    QLabel* _messagesCenter;
    QLabel* _messagesRight;

    int _totalBytes;
};

#endif // __INFOWIDGET_H__
