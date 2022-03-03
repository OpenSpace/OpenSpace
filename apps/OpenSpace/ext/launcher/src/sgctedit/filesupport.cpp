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

FileSupport::FileSupport(QVBoxLayout* parentLayout,
                         UserConfigurationElements& cfgElements,
                         SgctConfigElements& sgctElements,
                         std::function<void(bool)> finishedCallback)
    : _displayWidget(cfgElements.display)
    , _orientationWidget(cfgElements.orientation)
    , _monitors(cfgElements.monitorList)
    , _cluster(sgctElements.cluster)
    , _windowList(sgctElements.windowList)
    , _finishedCallback(finishedCallback)
    , _userConfigPath(cfgElements.configSavePath)
{
    QVBoxLayout* layoutFullVertical = new QVBoxLayout;
    _saveButton = new QPushButton("Save As");
    _saveButton->setToolTip("Save configuration changes (opens file chooser dialog)");
    _saveButton->setFocusPolicy(Qt::NoFocus);
    connect(_saveButton, &QPushButton::released, this, &FileSupport::save);
    _cancelButton = new QPushButton("Cancel");
    _cancelButton->setToolTip("Cancel changes");
    _cancelButton->setFocusPolicy(Qt::NoFocus);
    connect(_cancelButton, &QPushButton::released, this, &FileSupport::cancel);
    _applyButton = new QPushButton("Apply Without Saving");
    _applyButton->setToolTip("Apply configuration changes without saving to file");
    _applyButton->setFocusPolicy(Qt::NoFocus);
    connect(_applyButton, &QPushButton::released, this, &FileSupport::apply);
    {
        QHBoxLayout* layoutButtonBox = new QHBoxLayout;
        layoutButtonBox->addStretch(1);
        layoutButtonBox->addWidget(_cancelButton);
        layoutButtonBox->addWidget(_saveButton);
        layoutButtonBox->addWidget(_applyButton);
        layoutFullVertical->addLayout(layoutButtonBox);
    }
    parentLayout->addLayout(layoutFullVertical);
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
        if (tmpWindow.isFullScreen) {
            tmpWindow.monitor = wCtrl->monitorNum();
        }
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

std::string FileSupport::saveFilename() {
    return _saveTarget;
}

void FileSupport::save() {
    QString fileName = QFileDialog::getSaveFileName(
        this,
        "Save Window Configuration File",
        QString::fromStdString(_userConfigPath),
        "Window Configuration (*.json);;(*.json)",
        nullptr
#ifdef __linux__
        , QFileDialog::DontUseNativeDialog
#endif
    );
    if (fileName.length() != 0) {
        _saveTarget = fileName.toStdString();
        saveConfigToSgctFormat();
        _finishedCallback(true);
    }
}

void FileSupport::cancel() {
    _finishedCallback(false);
}

void FileSupport::apply() {
    std::string userCfgTempDir = _userConfigPath;
    if (userCfgTempDir.back() != '/') {
        userCfgTempDir += "/";
    }
    userCfgTempDir += "temp";
    if (!std::filesystem::is_directory(userCfgTempDir)) {
        std::filesystem::create_directories(absPath(userCfgTempDir));
    }
    _saveTarget = userCfgTempDir + "/" + "apply-without-saving.json";
    saveConfigToSgctFormat();
    _finishedCallback(true);
}

void FileSupport::saveConfigToSgctFormat() {
    saveCluster();
    saveWindows();
    saveUser();
}
