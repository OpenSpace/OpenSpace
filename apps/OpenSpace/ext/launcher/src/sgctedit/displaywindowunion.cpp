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

#include "sgctedit/displaywindowunion.h"

#include <ghoul/fmt.h>
#include "sgctedit/monitorbox.h"
#include "sgctedit/windowcontrol.h"
#include <QApplication>
#include <QMainWindow>
#include <QScreen>
#include <string>

DisplayWindowUnion::DisplayWindowUnion(const std::vector<QRect>& monitorSizeList,
                                       unsigned int nMaxWindows,
                                       const std::array<QColor, 4>& windowColors,
                                       QWidget* parent)
    : QWidget(parent)
    , _monitorResolutions(monitorSizeList)
    , _nMaxWindows(nMaxWindows)
    , _windowColors(windowColors)
{
    createWidgets();
    showWindows();
}

void DisplayWindowUnion::createWidgets() {
    // Add all window controls (some will be hidden from GUI initially)
    for (unsigned int i = 0; i < _nMaxWindows; ++i) {
        if (_nWindowsAllocated >= _nMaxWindows) {
            return;
        }

        const unsigned int monitorNumForThisWindow =
            (_nMaxWindows > 3 && _nWindowsAllocated >= 2) ? 1 : 0;

        WindowControl* ctrl = new WindowControl(
            monitorNumForThisWindow,
            _nWindowsAllocated,
            _monitorResolutions,
            _windowColors[_nWindowsAllocated],
            this
        );
        _windowControl.push_back(ctrl);

        connect(
            ctrl, &WindowControl::windowChanged,
            this, &DisplayWindowUnion::windowChanged
        );

        connect(
            ctrl, &WindowControl::webGuiChanged,
            [this](unsigned int winIndex) {
                for (unsigned int w = 0; w < _nMaxWindows; ++w) {
                    if (w != winIndex) {
                        _windowControl[w]->uncheckWebGuiOption();
                    }
                }
            }
        );

        emit windowChanged(
            monitorNumForThisWindow,
            _nWindowsAllocated,
            _windowControl.back()->dimensions()
        );

        _nWindowsAllocated++;
    }


    


    QVBoxLayout* layout = new QVBoxLayout;
    {
        QHBoxLayout* layoutMonButton = new QHBoxLayout;

        _addWindowButton = new QPushButton("Add Window");
        _addWindowButton->setToolTip(QString::fromStdString(fmt::format(
            "Add a window to the configuration (up to {} windows allowed)", _nMaxWindows
        )));
        _addWindowButton->setFocusPolicy(Qt::NoFocus);
        connect(
            _addWindowButton, &QPushButton::clicked,
            this, &DisplayWindowUnion::addWindow
        );

        _removeWindowButton = new QPushButton("Remove Window");
        _removeWindowButton->setFocusPolicy(Qt::NoFocus);
        _removeWindowButton->setToolTip(
            "Remove window from the configuration (at least one window is required)"
        );
        connect(
            _removeWindowButton, &QPushButton::clicked,
            this, &DisplayWindowUnion::removeWindow
        );
        layoutMonButton->addWidget(_removeWindowButton);
        layoutMonButton->addStretch(1);
        layoutMonButton->addWidget(_addWindowButton);
        layout->addLayout(layoutMonButton);
    }
    QHBoxLayout* layoutWindows = new QHBoxLayout;
    layout->addStretch();

    for (unsigned int i = 0; i < _nMaxWindows; ++i) {
        layoutWindows->addWidget(_windowControl[i]);
        if (i < (_nMaxWindows - 1)) {
            QFrame* frameForNextWindow = new QFrame;
            frameForNextWindow->setFrameShape(QFrame::VLine);
            _frameBorderLines.push_back(frameForNextWindow);
            layoutWindows->addWidget(frameForNextWindow);
        }
    }
    _nWindowsDisplayed = 0;
    layout->addLayout(layoutWindows);
    setLayout(layout);
}

std::vector<WindowControl*> DisplayWindowUnion::windowControls() const {
    return _windowControl;
}

unsigned int DisplayWindowUnion::nWindows() const {
    return _nWindowsDisplayed;
}

void DisplayWindowUnion::addWindow() {
    if (_nWindowsDisplayed < _nMaxWindows) {
        _windowControl[_nWindowsDisplayed]->resetToDefaults();
        _nWindowsDisplayed++;
        showWindows();
    }
}

void DisplayWindowUnion::removeWindow() {
    if (_nWindowsDisplayed > 1) {
        _nWindowsDisplayed--;
        showWindows();
    }
}

void DisplayWindowUnion::showWindows() {
    for (size_t i = 0; i < _windowControl.size(); ++i) {
        _windowControl[i]->setVisible(i < _nWindowsDisplayed);
    }
    for (size_t i = 0; i < _frameBorderLines.size(); ++i) {
        _frameBorderLines[i]->setVisible(i < (_nWindowsDisplayed - 1));
    }
    _removeWindowButton->setEnabled(_nWindowsDisplayed > 1);
    _addWindowButton->setEnabled(_nWindowsDisplayed != _nMaxWindows);
    for (WindowControl* w : _windowControl) {
        w->showWindowLabel(_nWindowsDisplayed > 1);
    }
    emit nWindowsChanged(_nWindowsDisplayed);
}
