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

#include "sgctedit/filesupport.h"

FileSupport::FileSupport(QVBoxLayout* parentLayout, std::vector<QRect>& monitorList,
                         Display* display, Orientation* orientation,
                         std::vector<sgct::config::Window>& windowList,
                         sgct::config::Cluster& cluster, std::function<void(bool)> cb)
    : _displayWidget(display)
    , _orientationWidget(orientation)
    , _monitors(monitorList)
    , _cluster(cluster)
    , _windowList(windowList)
    , _finishedCallback(cb)
{
    QVBoxLayout* layoutFullVertical = new QVBoxLayout();
    _lineFilename = new QLineEdit();
    _lineFilename->setFixedWidth(190);
    {
        QHBoxLayout* layoutFilename = new QHBoxLayout();
        QLabel* labelFilename = new QLabel();
        labelFilename->setText("Filename: ");
        layoutFilename->addStretch(1);
        layoutFilename->addWidget(labelFilename);
        layoutFilename->addWidget(_lineFilename);
        layoutFilename->addStretch(1);
        layoutFullVertical->addLayout(layoutFilename);
    }
    _saveButton = new QPushButton("Save");
    _saveButton->setToolTip("Save global orientation changes");
    connect(_saveButton, SIGNAL(released()), this, SLOT(save()));
    _saveButton->setEnabled(false);
    _cancelButton = new QPushButton("Cancel");
    _cancelButton->setToolTip("Cancel global orientation changes");
    connect(_cancelButton, SIGNAL(released()), this, SLOT(cancel()));
    {
        QHBoxLayout* layoutButtonBox = new QHBoxLayout;
        layoutButtonBox->addStretch(1);
        layoutButtonBox->addWidget(_saveButton);
        layoutButtonBox->addWidget(_cancelButton);
        layoutFullVertical->addLayout(layoutButtonBox);
    }
    parentLayout->addLayout(layoutFullVertical);
    connect(_lineFilename, SIGNAL(textEdited(const QString&)), this,
        SLOT(filenameEdited(const QString&)));
}

void FileSupport::saveCluster() {
    if (_orientationWidget) {
        sgct::config::Scene initScene;
        initScene.orientation = _orientationWidget->orientationValue();
        if (_cluster.nodes.size() == 0) {
            _cluster.nodes.push_back(sgct::config::Node());
        }
        _cluster.masterAddress = "localhost";
        _cluster.nodes.back().address = "localhost";
        _cluster.nodes.back().port = 20401;
        _cluster.scene = std::move(initScene);
        _cluster.firmSync = _orientationWidget->vsyncValue();
    }
}

void FileSupport::saveUser() {
    if (_orientationWidget) {
        sgct::config::User user;
        user.eyeSeparation = 0.065;
        user.position = {0.0, 0.0, 4.0};
        _cluster.users.push_back(user);
    }
}

bool FileSupport::isWindowFullscreen(unsigned int monitorIdx, sgct::ivec2 wDims) {
    sgct::ivec2 mDims = {_monitors[monitorIdx].width(), _monitors[monitorIdx].height()};
    return ((mDims.x == wDims.x) && (mDims.y == wDims.y));
}

bool FileSupport::findGuiWindow(unsigned int& foundWindowIndex) {
    unsigned int windowIndex = 0;
    for (unsigned int w = 0; w < _displayWidget->nWindows(); ++w) {
        if (_displayWidget->windowControls()[w]->isGuiWindow()) {
            foundWindowIndex = windowIndex;
            return true;
        }
        windowIndex++;
    }
    return false;
}

void FileSupport::saveWindows() {
    unsigned int windowIndex = 0;
    for (unsigned int w = 0; w < _displayWidget->nWindows(); ++w) {
        WindowControl* wCtrl = _displayWidget->windowControls()[w];
        _windowList.push_back(sgct::config::Window());
        saveWindowsViewports();
        saveProjectionInformation(
            wCtrl->isSpoutSelected(),
            wCtrl->projectionSelectedIndex(),
            wCtrl,
            _windowList.back().viewports.back()
        );
        saveWindowsDimensions(wCtrl);
        _windowList.back().isDecorated = wCtrl->isDecorated();
        saveWindowsFullScreen(wCtrl);
        saveWindowsWebGui(windowIndex);
        if (!wCtrl->windowName().empty()) {
            _windowList.back().name = wCtrl->windowName();
        }
        _windowList.back().id = windowIndex++;
    }
}

void FileSupport::saveWindowsViewports() {
    _windowList.back().viewports.push_back(sgct::config::Viewport());
    _windowList.back().viewports.back().isTracked = true;
    _windowList.back().viewports.back().position = {0.0, 0.0};
    _windowList.back().viewports.back().size = {1.0, 1.0};
}

void FileSupport::saveWindowsDimensions(WindowControl* wCtrl) {
    _windowList.back().size = wCtrl->windowSize();
    _windowList.back().pos = {
        _monitors[wCtrl->monitorNum()].x() + wCtrl->windowPos().x,
        _monitors[wCtrl->monitorNum()].y() + wCtrl->windowPos().y,
    };
}

void FileSupport::saveWindowsWebGui(unsigned int wIdx) {
    _windowList.back().draw2D = true;
    _windowList.back().draw3D = true;
    _windowList.back().viewports.back().isTracked = true;
    unsigned int webGuiWindowIndex;
    bool isOneOfWindowsSetAsWebGui = findGuiWindow(webGuiWindowIndex);
    if (isOneOfWindowsSetAsWebGui) {
        if (wIdx == webGuiWindowIndex) {
            _windowList.back().viewports.back().isTracked = false;
            _windowList.back().tags.push_back("GUI");
        }
        _windowList.back().draw2D = (wIdx == webGuiWindowIndex);
        _windowList.back().draw3D = !(_windowList.back().draw2D.value());
    }
}

void FileSupport::saveWindowsFullScreen(WindowControl* wCtrl) {
    bool isFullScreen = isWindowFullscreen(
        wCtrl->monitorNum(),
        wCtrl->windowSize()
    );
    if (isFullScreen) {
        _windowList.back().isFullScreen = true;
    }
}

void FileSupport::saveProjectionInformation(bool isSpoutSelected, int projectionIndex,
                             WindowControl* winControl, sgct::config::Viewport& viewport)
{
    if (isSpoutSelected) {
        saveProjection_Spout(projectionIndex, winControl, viewport);
    }
    else {
        saveProjection_NonSpout(projectionIndex, winControl, viewport);
    }
}

void FileSupport::saveProjection_Spout(int projectionIndex, WindowControl* winControl,
                                                         sgct::config::Viewport& viewport)
{
    sgct::config::SpoutOutputProjection projection;
    switch(projectionIndex) {
        case WindowControl::ProjectionIndeces::Fisheye:
            projection.mapping
                = sgct::config::SpoutOutputProjection::Mapping::Fisheye;
            break;

        case WindowControl::ProjectionIndeces::Equirectangular:
        default:
            projection.mapping
                = sgct::config::SpoutOutputProjection::Mapping::Equirectangular;
            break;
    }
    projection.quality = winControl->qualitySelectedValue();
    projection.mappingSpoutName = "OpenSpace";
    viewport.projection = std::move(projection);
}

void FileSupport::saveProjection_NonSpout(int projectionIndex, WindowControl* winControl,
                                                         sgct::config::Viewport& viewport)
{
    switch(projectionIndex) {
        case WindowControl::ProjectionIndeces::Fisheye:
            {
                sgct::config::FisheyeProjection projection;
                projection.quality = winControl->qualitySelectedValue();
                projection.fov = 180.0;
                projection.tilt = 0.0;
                viewport.projection = std::move(projection);
            }
            break;

        case WindowControl::ProjectionIndeces::Spherical_Mirror:
            {
                sgct::config::SphericalMirrorProjection projection;
                projection.quality = winControl->qualitySelectedValue();
                viewport.projection = std::move(projection);
            }
            break;

        case WindowControl::ProjectionIndeces::Cylindrical:
            {
                sgct::config::CylindricalProjection projection;
                projection.quality = winControl->qualitySelectedValue();
                projection.heightOffset = winControl->heightOffset();
                viewport.projection = std::move(projection);
            }
            break;

        case WindowControl::ProjectionIndeces::Equirectangular:
            {
                sgct::config::EquirectangularProjection projection;
                projection.quality = winControl->qualitySelectedValue();
                viewport.projection = std::move(projection);
            }
            break;

        case WindowControl::ProjectionIndeces::Planar:
        default:
            {
                // The negative values for left & down are according to sgct's convention
                sgct::config::PlanarProjection projection;
                projection.fov.right = winControl->fovH() / 2.0;
                projection.fov.left = -projection.fov.right;
                projection.fov.up = winControl->fovV() / 2.0;
                projection.fov.down = -projection.fov.up;
                viewport.projection = std::move(projection);
            }
            break;
    }
}

void FileSupport::filenameEdited(const QString& newString) {
    _saveButton->setEnabled(!newString.isEmpty());
}

std::string FileSupport::saveFilename() {
    return _lineFilename->text().toStdString();
}

void FileSupport::save() {
    saveCluster();
    saveWindows();
    saveUser();
    _finishedCallback(true);
}

void FileSupport::cancel() {
    _finishedCallback(false);
}
