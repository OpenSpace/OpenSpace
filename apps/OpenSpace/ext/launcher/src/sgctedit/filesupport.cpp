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
                         std::shared_ptr<Display> display, Orientation* orientation,
                         std::vector<sgct::config::Window>& windowList,
                         sgct::config::Cluster& cluster, std::function<void(bool)> cb)
    : _displayWidget(display)
    , _orientationWidget(orientation)
    , _monitors(monitorList)
    , _cluster(cluster)
    , _windowList(windowList)
    , _finishedCallback(cb)
{
    QVBoxLayout* layoutFullVertical = new QVBoxLayout;
    _lineFilename = new QLineEdit;
    _lineFilename->setFixedWidth(190);
    {
        QHBoxLayout* layoutFilename = new QHBoxLayout;
        QLabel* labelFilename = new QLabel;
        QString fileTip = "Enter a filename for this custom configuration to be saved "
            "as a .json file in the user/config/ directory";
        labelFilename->setToolTip(fileTip);
        _lineFilename->setToolTip(fileTip);
        labelFilename->setText("Filename: ");
        layoutFilename->addStretch(1);
        layoutFilename->addWidget(labelFilename);
        layoutFilename->addWidget(_lineFilename);
        layoutFilename->addStretch(1);
        layoutFullVertical->addLayout(layoutFilename);
    }
    _saveButton = new QPushButton("Save");
    _saveButton->setToolTip("Save global orientation changes");
    connect(_saveButton, &QPushButton::released, this, &FileSupport::save);
    _saveButton->setEnabled(false);
    _cancelButton = new QPushButton("Cancel");
    _cancelButton->setToolTip("Cancel global orientation changes");
    connect(_cancelButton, &QPushButton::released, this, &FileSupport::cancel);
    {
        QHBoxLayout* layoutButtonBox = new QHBoxLayout;
        layoutButtonBox->addStretch(1);
        layoutButtonBox->addWidget(_saveButton);
        layoutButtonBox->addWidget(_cancelButton);
        layoutFullVertical->addLayout(layoutButtonBox);
    }
    parentLayout->addLayout(layoutFullVertical);
    connect(_lineFilename, &QLineEdit::textEdited, this, &FileSupport::filenameEdited);
}

void FileSupport::saveCluster() {
    if (_orientationWidget) {
        sgct::config::Scene initScene;
        initScene.orientation = _orientationWidget->orientationValue();
        _cluster.nodes.clear();
        sgct::config::Node tmpNode;
        tmpNode.address = "localhost";
        tmpNode.port = 20401;
        _cluster.nodes.push_back(tmpNode);
        _cluster.masterAddress = "localhost";
        _cluster.scene = std::move(initScene);
        _cluster.firmSync = _orientationWidget->vsyncValue();
    }
}

void FileSupport::saveUser() {
    if (_orientationWidget) {
        sgct::config::User user;
        user.eyeSeparation = 0.065f;
        user.position = {0.0f, 0.0f, 4.0f};
        _cluster.users.push_back(user);
    }
}

bool FileSupport::isWindowFullscreen(unsigned int monitorIdx, sgct::ivec2 wDims) {
    return (_monitors[monitorIdx].width() == wDims.x &&
            _monitors[monitorIdx].height() == wDims.y);
}

std::optional<unsigned int> FileSupport::findGuiWindow() {
    unsigned int windowIndex = 0;
    for (unsigned int w = 0; w < _displayWidget->nWindows(); ++w) {
        if (_displayWidget->windowControls()[w]->isGuiWindow()) {
            return std::optional<unsigned int>(windowIndex);
        }
        windowIndex++;
    }
    return std::nullopt;
}

void FileSupport::saveWindows() {
    unsigned int windowIndex = 0;
    for (unsigned int w = 0; w < _displayWidget->nWindows(); ++w) {
        std::shared_ptr<WindowControl> wCtrl = _displayWidget->windowControls()[w];
        sgct::config::Window tmpWindow = saveWindowsDimensions(wCtrl);
        tmpWindow.viewports.push_back(generateViewport());
        tmpWindow.viewports.back().projection = saveProjectionInformation(wCtrl);
        tmpWindow.isDecorated = wCtrl->isDecorated();
        tmpWindow.isFullScreen = isWindowFullscreen(
            wCtrl->monitorNum(),
            wCtrl->windowSize()
        );
        saveWindowsWebGui(windowIndex, tmpWindow);
        if (!wCtrl->windowName().empty()) {
            tmpWindow.name = wCtrl->windowName();
        }
        tmpWindow.id = windowIndex++;
        _windowList.push_back(tmpWindow);
    }
}

sgct::config::Viewport FileSupport::generateViewport() {
    sgct::config::Viewport vp;
    vp.isTracked = true;
    vp.position = {0.f, 0.f};
    vp.size = {1.f, 1.f};
    return vp;
}

sgct::config::Window FileSupport::saveWindowsDimensions(
                                                     std::shared_ptr<WindowControl> wCtrl)
{
    sgct::config::Window tmpWindow;
    tmpWindow.size = wCtrl->windowSize();
    tmpWindow.pos = {
        _monitors[wCtrl->monitorNum()].x() + wCtrl->windowPos().x,
        _monitors[wCtrl->monitorNum()].y() + wCtrl->windowPos().y,
    };
    return tmpWindow;
}

void FileSupport::saveWindowsWebGui(unsigned int wIdx, sgct::config::Window& win) {
    win.viewports.back().isTracked = true;
    std::optional<unsigned int> webGuiWindowIndex = findGuiWindow();
    bool isOneOfWindowsSetAsWebGui = webGuiWindowIndex.has_value();
    if (isOneOfWindowsSetAsWebGui) {
        if (wIdx == webGuiWindowIndex.value()) {
            win.viewports.back().isTracked = false;
            win.tags.push_back("GUI");
        }
        win.draw2D = (wIdx == webGuiWindowIndex.value());
        win.draw3D = !(win.draw2D.value());
    }
}

ProjectionOptions FileSupport::saveProjectionInformation(
                                                std::shared_ptr<WindowControl> winControl)
{
    if (winControl->isSpoutSelected()) {
        return saveProjectionSpout(winControl);
    }
    else {
        return saveProjectionNoSpout(winControl);
    }
}

ProjectionOptions FileSupport::saveProjectionSpout(
                                                std::shared_ptr<WindowControl> winControl)
{
    sgct::config::SpoutOutputProjection projection;
    switch(winControl->projectionSelectedIndex()) {
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
    return projection;
}

ProjectionOptions FileSupport::saveProjectionNoSpout(
                                                std::shared_ptr<WindowControl> winControl)
{
    switch(winControl->projectionSelectedIndex()) {
        case WindowControl::ProjectionIndeces::Fisheye:
            {
                sgct::config::FisheyeProjection projection;
                projection.quality = winControl->qualitySelectedValue();
                projection.fov = 180.f;
                projection.tilt = 0.f;
                return projection;
            }
            break;

        case WindowControl::ProjectionIndeces::SphericalMirror:
            {
                sgct::config::SphericalMirrorProjection projection;
                projection.quality = winControl->qualitySelectedValue();
                return projection;
            }
            break;

        case WindowControl::ProjectionIndeces::Cylindrical:
            {
                sgct::config::CylindricalProjection projection;
                projection.quality = winControl->qualitySelectedValue();
                projection.heightOffset = winControl->heightOffset();
                return projection;
            }
            break;

        case WindowControl::ProjectionIndeces::Equirectangular:
            {
                sgct::config::EquirectangularProjection projection;
                projection.quality = winControl->qualitySelectedValue();
                return projection;
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
                return projection;
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
