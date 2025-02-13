/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "sgctedit/windowcontrol.h"
#include <ghoul/format.h>
#include <QColor>
#include <QFrame>
#include <QPushButton>
#include <QVBoxLayout>
#include <array>
#include <string>

namespace {
    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;
} // namespace

DisplayWindowUnion::DisplayWindowUnion(const std::vector<QRect>& monitorSizeList,
                                       int nMaxWindows,
                                       const std::array<QColor, 4>& windowColors,
                                       int nWindows, QWidget* parent)
    : QWidget(parent)
{
    createWidgets(nMaxWindows, monitorSizeList, windowColors);
    showWindows();

    for (int i = 0; i < nWindows; i++) {
        addWindow();
    }
}

void DisplayWindowUnion::initialize(const std::vector<QRect>& monitorSizeList,
                                    const sgct::config::Cluster& cluster)
{
    const size_t nWindows = cluster.nodes.front().windows.size();
    const size_t existingWindowsControlSize = _windowControls.size();
    for (size_t i = 0; i < nWindows; i++) {
        const sgct::config::Window& w = cluster.nodes.front().windows[i];
        WindowControl* wCtrl = _windowControls[i];
        if (i < existingWindowsControlSize && wCtrl) {
            unsigned int monitorNum = 0;
            if (w.monitor.has_value()) {
                monitorNum = static_cast<unsigned int>(*w.monitor);
                if (monitorNum > (monitorSizeList.size() - 1)) {
                    monitorNum = 0;
                }
            }
            unsigned int posX = 0;
            unsigned int posY = 0;
            wCtrl->setMonitorSelection(monitorNum);
            if (w.pos.has_value()) {
                posX = w.pos->x;
                posY = w.pos->y;
                // Convert offsets to coordinates relative to the selected monitor bounds,
                // since window offsets are stored n the sgct config file relative to the
                // coordinates of the total "canvas" of all displays
                if (monitorSizeList.size() > monitorNum) {
                    posX -= monitorSizeList[monitorNum].x();
                    posY -= monitorSizeList[monitorNum].y();
                }
            }
            const QRectF newDims = QRectF(posX, posY, w.size.x, w.size.y);
            wCtrl->setDimensions(newDims);
            if (w.name.has_value()) {
                wCtrl->setWindowName(*w.name);
            }
            if (w.isDecorated.has_value()) {
                wCtrl->setDecorationState(*w.isDecorated);
            }
            wCtrl->setSpoutOutputState(w.spout.has_value() && w.spout->enabled);
        }
        std::visit(overloaded{
            [&](const sgct::config::CylindricalProjection& p) {
                if (p.quality.has_value() && p.heightOffset.has_value()) {
                    wCtrl->setProjectionCylindrical(*p.quality, *p.heightOffset);
                }
            },
            [&](const sgct::config::EquirectangularProjection& p) {
                if (p.quality.has_value()) {
                    wCtrl->setProjectionEquirectangular(*p.quality);
                }
            },
            [&](const sgct::config::FisheyeProjection& p) {
                if (p.quality.has_value()) {
                    wCtrl->setProjectionFisheye(*p.quality);
                }
            },
            [&](const sgct::config::PlanarProjection& p) {
                wCtrl->setProjectionPlanar(
                    std::abs(p.fov.left) + std::abs(p.fov.right),
                    std::abs(p.fov.up) + std::abs(p.fov.down)
                );
            },
            [&](const sgct::config::SphericalMirrorProjection& p) {
                if (p.quality.has_value()) {
                    wCtrl->setProjectionSphericalMirror(*p.quality);
                }
            },
            [&](const sgct::config::NoProjection&) {},
            [&](const sgct::config::ProjectionPlane&) {},
            [&](const sgct::config::CubemapProjection&) {},
            },
            w.viewports.back().projection
        );
    }
}

void DisplayWindowUnion::createWidgets(int nMaxWindows,
                                       const std::vector<QRect>& monitorResolutions,
                                       std::array<QColor, 4> windowColors)
{
    // Add all window controls (some will be hidden from GUI initially)
    for (int i = 0; i < nMaxWindows; i++) {
        const int monitorNumForThisWindow =
            (monitorResolutions.size() > 1 && i >= 2) ? 1 : 0;

        WindowControl* ctrl = new WindowControl(
            monitorNumForThisWindow,
            i,
            monitorResolutions,
            windowColors[i],
            this
        );
        _windowControls.push_back(ctrl);

        connect(
            ctrl, &WindowControl::windowChanged,
            this, &DisplayWindowUnion::windowChanged
        );
    }

    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSizeConstraint(QLayout::SizeConstraint::SetMinimumSize);

    {
        QBoxLayout* layoutMonButton = new QHBoxLayout;
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

        _addWindowButton = new QPushButton("Add Window");
        _addWindowButton->setToolTip(QString::fromStdString(std::format(
            "Add a window to the configuration (up to {} windows allowed)", nMaxWindows
        )));
        _addWindowButton->setFocusPolicy(Qt::NoFocus);
        connect(
            _addWindowButton, &QPushButton::clicked,
            this, &DisplayWindowUnion::addWindow
        );
        layoutMonButton->addWidget(_addWindowButton);
        layout->addLayout(layoutMonButton);
    }

    QFrame* line = new QFrame;
    line->setFrameShape(QFrame::HLine);
    line->setFrameShadow(QFrame::Sunken);
    layout->addWidget(line);

    QBoxLayout* layoutWindows = new QHBoxLayout;
    layoutWindows->setContentsMargins(0, 0, 0, 0);
    layoutWindows->setSpacing(0);
    for (int i = 0; i < nMaxWindows; i++) {
        layoutWindows->addWidget(_windowControls[i]);
        if (i < (nMaxWindows - 1)) {
            QFrame* frameForNextWindow = new QFrame;
            frameForNextWindow->setFrameShape(QFrame::VLine);
            _frameBorderLines.push_back(frameForNextWindow);
            layoutWindows->addWidget(frameForNextWindow);
        }
    }
    layout->addLayout(layoutWindows);
    layout->addStretch();
}

std::vector<WindowControl*> DisplayWindowUnion::activeWindowControls() const {
    std::vector<WindowControl*> res;
    res.reserve(_nWindowsDisplayed);
    for (unsigned int i = 0; i < _nWindowsDisplayed; i++) {
        res.push_back(_windowControls[i]);
    }
    return res;
}

void DisplayWindowUnion::addWindow() {
    if (_nWindowsDisplayed < _windowControls.size()) {
        _windowControls[_nWindowsDisplayed]->resetToDefaults();
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

unsigned int DisplayWindowUnion::numWindowsDisplayed() const {
    return _nWindowsDisplayed;
}

void DisplayWindowUnion::showWindows() {
    for (size_t i = 0; i < _windowControls.size(); i++) {
        _windowControls[i]->setVisible(i < _nWindowsDisplayed);
    }
    for (size_t i = 0; i < _frameBorderLines.size(); i++) {
        _frameBorderLines[i]->setVisible(i < (_nWindowsDisplayed - 1));
    }
    _removeWindowButton->setEnabled(_nWindowsDisplayed > 1);
    _addWindowButton->setEnabled(_nWindowsDisplayed != _windowControls.size());
    for (WindowControl* w : _windowControls) {
        w->showWindowLabel(_nWindowsDisplayed > 1);
    }
    emit nWindowsChanged(_nWindowsDisplayed);
}
