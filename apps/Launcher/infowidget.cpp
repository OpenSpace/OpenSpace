/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
    , _messagesLeft(nullptr)
    , _messagesCenter(nullptr)
    , _messagesRight(nullptr)
    , _totalBytes(totalBytes)
{
    setFixedHeight(100);

    QGridLayout* layout = new QGridLayout;
    layout->setHorizontalSpacing(10);

    _name = new QLabel(name);
    _name->setObjectName("Name");
    //_name->setMaximumWidth(300);
    _name->setFixedWidth(450);
    layout->addWidget(_name, 0, 0, 1, 2);
    layout->setRowStretch(1, 10);

    _bytes = new QLabel("");
    _bytes->setObjectName("Bytes");
    layout->addWidget(_bytes, 2, 0);

    _progress = new QProgressBar;
    _progress->setTextVisible(false);
    _progress->setFixedWidth(285);
    layout->addWidget(_progress, 2, 1);

    _messagesLeft = new QLabel("");
    _messagesLeft->setObjectName("MessageLeft");
    _messagesCenter = new QLabel("");
    _messagesCenter->setObjectName("MessageCenter");
    _messagesRight = new QLabel("");
    _messagesRight->setObjectName("MessageRight");

    layout->addWidget(_messagesLeft, 3, 0, 1, 2);
    layout->addWidget(_messagesCenter, 3, 0, 1, 2, Qt::AlignCenter);
    layout->addWidget(_messagesRight, 3, 0, 1, 2, Qt::AlignRight);

    setLayout(layout);
}

void InfoWidget::update(std::shared_ptr<openspace::DownloadManager::FileFuture> f) {
    _bytes->setText(
        QString("%1 / %2")
        .arg(f->currentSize)
        .arg(f->totalSize)
    );
    _progress->setValue(static_cast<int>(f->progress * 100));

    if (f->errorMessage.empty()) {
        QString t = "Time remaining %1 s";
        _messagesLeft->setText(t.arg(static_cast<int>(f->secondsRemaining)));
    }
    else {
        _messagesLeft->setText(QString::fromStdString(f->errorMessage));
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
        if (bytesPerSecond > 0 && remainingBytes > 0) {
            float seconds = static_cast<float>(remainingBytes) / bytesPerSecond;

            QString left = "Remaining: %1 s";
            _messagesLeft->setText(left.arg(static_cast<int>(seconds)));

            QString center = "Peers: %1 (%2) | Seeds: %3 (%4)";
            _messagesCenter->setText(center.arg(s.num_peers).arg(s.list_peers).arg(s.num_seeds).arg(s.list_seeds));

            QString right = "%1 KiB/s";
            _messagesRight->setText(right.arg(bytesPerSecond / 1024));
        }
        else
            _messagesLeft->setText("");
    }
    else {
        _messagesLeft->setText(QString::fromStdString(s.error));
    }

}

void InfoWidget::error(QString message) {
    _messagesLeft->setText(message);
}
