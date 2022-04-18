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

#include <ghoul/filesystem/filesystem.h>
#include <QFileDialog>
#include <filesystem>

namespace {
    bool isWindowFullscreen(QRect monitor, const sgct::ivec2& wDims) {
        return (monitor.width() == wDims.x && monitor.height() == wDims.y);
    }

    sgct::config::Viewport generateViewport() {
        sgct::config::Viewport vp;
        vp.isTracked = true;
        vp.position = { 0.f, 0.f };
        vp.size = { 1.f, 1.f };
        return vp;
    }

    ProjectionOptions saveProjectionNoSpout(const WindowControl& winControl) {
        switch (winControl.projectionSelectedIndex()) {
            case WindowControl::ProjectionIndeces::Fisheye:
                {
                    sgct::config::FisheyeProjection projection;
                    projection.quality = winControl.qualitySelectedValue();
                    projection.fov = 180.f;
                    projection.tilt = 0.f;
                    return projection;
                }
            case WindowControl::ProjectionIndeces::SphericalMirror:
                {
                    sgct::config::SphericalMirrorProjection projection;
                    projection.quality = winControl.qualitySelectedValue();
                    return projection;
                }
            case WindowControl::ProjectionIndeces::Cylindrical:
                {
                    sgct::config::CylindricalProjection projection;
                    projection.quality = winControl.qualitySelectedValue();
                    projection.heightOffset = winControl.heightOffset();
                    return projection;
                }
            case WindowControl::ProjectionIndeces::Equirectangular:
                {
                    sgct::config::EquirectangularProjection projection;
                    projection.quality = winControl.qualitySelectedValue();
                    return projection;
                }
            case WindowControl::ProjectionIndeces::Planar:
            default:
                {
                    // The negative values for left & down are due to SGCT's convention
                    sgct::config::PlanarProjection projection;
                    projection.fov.right = winControl.fovH() / 2.0;
                    projection.fov.left = -projection.fov.right;
                    projection.fov.up = winControl.fovV() / 2.0;
                    projection.fov.down = -projection.fov.up;
                    return projection;
                }
        }
    }

    ProjectionOptions saveProjectionSpout(const WindowControl& winControl) {
        sgct::config::SpoutOutputProjection projection;
        switch (winControl.projectionSelectedIndex()) {
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
        projection.quality = winControl.qualitySelectedValue();
        projection.mappingSpoutName = "OpenSpace";
        return projection;
    }

    ProjectionOptions saveProjectionInformation(const WindowControl& winControl) {
        if (winControl.isSpoutSelected()) {
            return saveProjectionSpout(winControl);
        }
        else {
            return saveProjectionNoSpout(winControl);
        }
    }
} // namespace

SgctEdit::SgctEdit(QWidget* parent, std::vector<sgct::config::Window>& windowList,
                   sgct::config::Cluster& cluster, const QList<QScreen*>& screenList,
                   std::string userConfigPath)
    : QDialog(parent)
    , _cluster(cluster)
    , _windowList(windowList)
    , _userConfigPath(std::move(userConfigPath))
{
    setWindowTitle("Window Configuration Editor");
    
    size_t nScreensManaged = std::min(static_cast<int>(screenList.length()), 2);
    for (unsigned int s = 0; s < static_cast<unsigned int>(nScreensManaged); ++s) {
        int actualWidth = std::max(
            screenList[s]->size().width(),
            screenList[s]->availableGeometry().width()
        );
        int actualHeight = std::max(
            screenList[s]->size().height(),
            screenList[s]->availableGeometry().height()
        );
        _monitorSizeList.emplace_back(
            screenList[s]->availableGeometry().x(),
            screenList[s]->availableGeometry().y(),
            actualWidth,
            actualHeight
        );
    }
    _nMaxWindows = (_monitorSizeList.size() == 1) ? 3 : 4;

    createWidgets();
}

void SgctEdit::createWidgets() {
    QVBoxLayout* layoutMainV = new QVBoxLayout;
    _orientationWidget = new Orientation;
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

        // Add Display Layout
        _displayLayout = new QVBoxLayout;
        _displayWidget = std::make_unique<DisplayWindowUnion>(
            *_monBox,
            _monitorSizeList,
            _nMaxWindows,
            _colorsForWindows
        );
        _displayFrame = new QFrame;
        _displayLayout->addWidget(_displayWidget.get());
        _displayFrame->setLayout(_displayLayout);
        _displayFrame->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
        layoutMainV->addWidget(_displayFrame);
    }
    {
        _orientationWidget->addControlsToParentLayout(layoutMainV);

        QFrame* bottomBorder = new QFrame;
        bottomBorder->setFrameShape(QFrame::HLine);
        layoutMainV->addWidget(bottomBorder);

        _saveButton = new QPushButton("Save As");
        _saveButton->setToolTip("Save configuration changes (opens file chooser dialog)");
        _saveButton->setFocusPolicy(Qt::NoFocus);
        connect(_saveButton, &QPushButton::released, this, &SgctEdit::save);
        _cancelButton = new QPushButton("Cancel");
        _cancelButton->setToolTip("Cancel changes");
        _cancelButton->setFocusPolicy(Qt::NoFocus);
        connect(_cancelButton, &QPushButton::released, this, &SgctEdit::reject);
        _applyButton = new QPushButton("Apply Without Saving");
        _applyButton->setToolTip("Apply configuration changes without saving to file");
        _applyButton->setFocusPolicy(Qt::NoFocus);
        connect(_applyButton, &QPushButton::released, this, &SgctEdit::apply);

        QHBoxLayout* layoutButtonBox = new QHBoxLayout;
        layoutButtonBox->addStretch(1);
        layoutButtonBox->addWidget(_cancelButton);
        layoutButtonBox->addWidget(_saveButton);
        layoutButtonBox->addWidget(_applyButton);
        layoutMainV->addLayout(layoutButtonBox);
    }
    setLayout(layoutMainV);
}

SgctEdit::~SgctEdit() {
    delete _orientationWidget;
    delete _displayLayout;
}

bool SgctEdit::wasSaved() const {
    return _saveSelected;
}

std::string SgctEdit::saveFilename() const {
    return _saveTarget;
}

void SgctEdit::save() {
    QString fileName = QFileDialog::getSaveFileName(
        this,
        "Save Window Configuration File",
        QString::fromStdString(_userConfigPath),
        "Window Configuration (*.json);;(*.json)",
        nullptr
#ifdef __linux__
        // Linux in Qt5 and Qt6 is crashed when trying to access the native dialog here
        , QFileDialog::DontUseNativeDialog
#endif
    );
    if (!fileName.isEmpty()) {
        _saveTarget = fileName.toStdString();
        saveConfigToSgctFormat();
        _saveSelected = true;
        accept();
    }
}

std::optional<unsigned int> SgctEdit::findGuiWindow() const {
    for (unsigned int w = 0; w < _displayWidget->nWindows(); ++w) {
        if (_displayWidget->windowControls()[w]->isGuiWindow()) {
            return w;
        }
    }
    return std::nullopt;
}

void SgctEdit::saveWindowsWebGui(unsigned int wIdx, sgct::config::Window& win) {
    win.viewports.back().isTracked = true;
    std::optional<unsigned int> webGuiWindowIndex = findGuiWindow();
    bool isOneOfWindowsSetAsWebGui = webGuiWindowIndex.has_value();
    if (isOneOfWindowsSetAsWebGui) {
        if (wIdx == *webGuiWindowIndex) {
            win.viewports.back().isTracked = false;
            win.tags.push_back("GUI");
        }
        win.draw2D = (wIdx == *webGuiWindowIndex);
        win.draw3D = !(win.draw2D.value_or(true));
    }
}

void SgctEdit::apply() {
    std::string userCfgTempDir = _userConfigPath;
    if (userCfgTempDir.back() != '/') {
        userCfgTempDir += '/';
    }
    userCfgTempDir += "temp";
    if (!std::filesystem::is_directory(absPath(userCfgTempDir))) {
        std::filesystem::create_directories(absPath(userCfgTempDir));
    }
    _saveTarget = userCfgTempDir + "/apply-without-saving.json";
    saveConfigToSgctFormat();
    _saveSelected = true;
    accept();
}

void SgctEdit::saveConfigToSgctFormat() {
    //
    // Save cluster
    if (!_orientationWidget) {
        return;
    }

    sgct::config::Scene scene;
    scene.orientation = _orientationWidget->orientationValue();
    _cluster.scene = std::move(scene);

    {
        sgct::config::Node node;
        node.address = "localhost";
        node.port = 20401;
        _cluster.nodes = { node };
    }
    _cluster.masterAddress = "localhost";
    _cluster.firmSync = _orientationWidget->vsyncValue();

    //
    // Save Windows
    unsigned int windowIndex = 0;
    for (unsigned int w = 0; w < _displayWidget->nWindows(); ++w) {
        WindowControl* wCtrl = _displayWidget->windowControls()[w];
        
        sgct::config::Window window;
        window.size = wCtrl->windowSize();
        window.pos = {
            _monitorSizeList[wCtrl->monitorNum()].x() + wCtrl->windowPos().x,
            _monitorSizeList[wCtrl->monitorNum()].y() + wCtrl->windowPos().y,
        };
        window.viewports.push_back(generateViewport());
        window.viewports.back().projection = saveProjectionInformation(*wCtrl);
        window.isDecorated = wCtrl->isDecorated();
        window.isFullScreen = isWindowFullscreen(
            _monitorSizeList[wCtrl->monitorNum()],
            wCtrl->windowSize()
        );
        if (window.isFullScreen) {
            window.monitor = wCtrl->monitorNum();
        }
        saveWindowsWebGui(windowIndex, window);
        if (!wCtrl->windowName().empty()) {
            window.name = wCtrl->windowName();
        }
        window.id = windowIndex++;
        _windowList.push_back(window);
    }

    //
    // Save User
    {
        sgct::config::User user;
        user.eyeSeparation = 0.065f;
        user.position = { 0.f, 0.f, 4.f };
        _cluster.users = { user };
    }
}
