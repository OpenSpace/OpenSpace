/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include "sgctedit/sgctedit.h"

SgctEdit::SgctEdit(QWidget* parent, std::vector<sgct::config::Window>& windowList,
                   sgct::config::Cluster& cluster, const QList<QScreen*>& screenList)
    : QDialog(parent)
    , _cluster(cluster)
    , _windowList(windowList)
{
    systemMonitorConfiguration(screenList);
    setWindowTitle("Window Configuration Editor");
    createWidgets();
}

void SgctEdit::systemMonitorConfiguration(const QList<QScreen*>& screenList) {
    size_t nScreensManaged = std::min(screenList.length(), 2);
    for (unsigned int s = 0; s < static_cast<unsigned int>(nScreensManaged); ++s) {
        int actualWidth = std::max(
            screenList[s]->size().width(),
            screenList[s]->availableGeometry().width()
        );
        int actualHeight = std::max(
            screenList[s]->size().height(),
            screenList[s]->availableGeometry().height()
        );
        _monitorSizeList.push_back({
            screenList[s]->availableGeometry().x(),
            screenList[s]->availableGeometry().y(),
            actualWidth,
            actualHeight
        });
    }
    _nMaxWindows = (_monitorSizeList.size() == 1) ? 3 : 4;
}

void SgctEdit::createWidgets() {
    QVBoxLayout* layoutMainV = new QVBoxLayout(this);
    QHBoxLayout* layoutMainH = new QHBoxLayout;
    _orientationWidget = new Orientation();
    {
        _monBox = std::make_shared<MonitorBox>(
            _monitorWidgetSize,
            _monitorSizeList,
            _nMaxWindows,
            _colorsForWindows
        );
        QHBoxLayout* layoutMonBox = new QHBoxLayout;
        layoutMonBox->addStretch(1);
        layoutMonBox->addWidget(_monBox.get());
        layoutMonBox->addStretch(1);
        layoutMainV->addLayout(layoutMonBox);
        _monBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        _monBox->setFixedSize(_monitorWidgetSize.width(), _monitorWidgetSize.height());
        addDisplayLayout(layoutMainH);
    }
    {
        layoutMainV->addLayout(layoutMainH);
        _orientationWidget->addButtonToLayout(layoutMainV);
        _fileSupportWidget = new FileSupport(
            layoutMainV,
            _monitorSizeList,
            _displayWidget,
            _orientationWidget,
            _windowList,
            _cluster,
            [this](bool accepted) {
                if (accepted) {
                    _saveSelected = true;
                    accept();
                }
                else {
                    reject();
                }
            }
        );
    }
}

void SgctEdit::addDisplayLayout(QHBoxLayout* layout) {
    _displayLayout = new QVBoxLayout;
    _displayWidget = std::make_shared<Display>(
        _monBox,
        _monitorSizeList,
        _nMaxWindows,
        _colorsForWindows
    );
    _displayFrame = new QFrame;
    _displayLayout->addWidget(_displayWidget.get());
    _displayFrame->setLayout(_displayLayout);
    _displayFrame->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    layout->addWidget(_displayFrame);
}

bool SgctEdit::wasSaved() const {
    return _saveSelected;
}

std::string SgctEdit::saveFilename() {
    return _fileSupportWidget->saveFilename();
}

SgctEdit::~SgctEdit() {
    delete _orientationWidget;
    delete _fileSupportWidget;
    delete _displayLayout;
}
