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

#include <QGridLayout>
#include <QLabel>
#include <QProgressBar>

InfoWidget::InfoWidget(QString name, int totalBytes)
    : QWidget(nullptr)
    , _name(nullptr)
    , _bytes(nullptr)
    , _progress(nullptr)
    , _messages(nullptr)
    , _totalBytes(totalBytes)
{
    QGridLayout* layout = new QGridLayout;

    _name = new QLabel(name);
    layout->addWidget(_name, 0, 0);

    _bytes = new QLabel("");
    layout->addWidget(_bytes, 0, 1);

    _progress = new QProgressBar;
    layout->addWidget(_progress, 0, 2);

    _messages = new QLabel("");
    layout->addWidget(_messages, 1, 0, 1, 3);

    setLayout(layout);

    update(0);
}

void InfoWidget::update(int currentBytes) {
    _bytes->setText(
        QString("%1 / %2")
        .arg(currentBytes)
        .arg(_totalBytes)
    );

    float progress = static_cast<float>(currentBytes) / static_cast<float>(_totalBytes);
    _progress->setValue(static_cast<int>(progress * 100));
}

void InfoWidget::error(QString message) {
    _messages->setText(message);
}
