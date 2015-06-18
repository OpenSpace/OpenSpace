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

#include "infowidget.h"

#include "syncwidget.h"

#include <QGridLayout>
#include <QLabel>
#include <QProgressBar>

InfoWidget::InfoWidget(QString name, int totalBytes)
    : QGroupBox(nullptr)
    , _name(nullptr)
    , _bytes(nullptr)
    , _progress(nullptr)
    , _messages(nullptr)
    , _totalBytes(totalBytes)
{
    setFixedHeight(100);

    QGridLayout* layout = new QGridLayout;
    layout->setVerticalSpacing(0);
    layout->setHorizontalSpacing(10);
    layout->setContentsMargins(0, 0, 0, 0);

    _name = new QLabel(name);
    layout->addWidget(_name, 0, 0);

    _bytes = new QLabel("");
    layout->addWidget(_bytes, 1, 0);

    _progress = new QProgressBar;
    layout->addWidget(_progress, 1, 1);

    _messages = new QLabel("");
    layout->addWidget(_messages, 2, 0, 1, 2);

    setLayout(layout);
}

void InfoWidget::update(openspace::DownloadManager::FileFuture* f) {
    _bytes->setText(
        QString("%1 / %2")
        .arg(f->currentSize)
        .arg(f->totalSize)
    );
    _progress->setValue(static_cast<int>(f->progress * 100));

    if (f->errorMessage.empty()) {
        QString t = "Time remaining %1 s";
        _messages->setText(t.arg(static_cast<int>(f->secondsRemaining)));
    }
    else {
        _messages->setText(QString::fromStdString(f->errorMessage));
    }
}

void InfoWidget::update(libtorrent::torrent_status s) {
    _bytes->setText(
        QString("%1 / %2")
        .arg(s.total_wanted_done)
        .arg(s.total_wanted)
    );
    float progress = static_cast<float>(s.total_wanted_done) / s.total_wanted;
    _progress->setValue(static_cast<int>(progress * 100));

    if (s.error.empty()) {
        int bytesPerSecond = s.download_rate;
        long long remainingBytes = s.total_wanted - s.total_wanted_done;
        if (remainingBytes > 0) {
            float seconds = static_cast<float>(remainingBytes) / remainingBytes;

            //auto now = time(NULL);
            //auto transferTime = now - s.added_time;
            //auto estimatedTime = transferTime / progress;
            //auto timeRemaining = estimatedTime - transferTime;
            QString t = "Time remaining %1 s";
            _messages->setText(t.arg(static_cast<int>(seconds)));
        }
        else
            _messages->setText("");
    }
    else {
        _messages->setText(QString::fromStdString(s.error));
    }

}

void InfoWidget::error(QString message) {
    _messages->setText(message);
}
