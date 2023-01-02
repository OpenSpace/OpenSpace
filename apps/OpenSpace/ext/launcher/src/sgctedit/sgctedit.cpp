/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <sgctedit/displaywindowunion.h>
#include <sgctedit/monitorbox.h>
#include <sgctedit/settingswidget.h>
#include <sgctedit/windowcontrol.h>
#include <ghoul/filesystem/filesystem.h>
#include <QApplication>
#include <QFileDialog>
#include <QFrame>
#include <QMessageBox>
#include <QPushButton>
#include <QScreen>
#include <QVBoxLayout>
#include <filesystem>

namespace {
    constexpr QRect MonitorWidgetSize = { 0, 0, 500, 500 };
    constexpr int MaxNumberWindows = 4;

    // Returns true if the windows are not ordered correctly. 'Correct' in this means that
    // there is a smaller window defined before a bigger one
    // This check is only necessary until
    // https://github.com/OpenSpace/OpenSpace/issues/507
    // is fixed
    bool hasWindowIssues(const sgct::config::Cluster& cluster) {
        sgct::ivec2 size = {
            std::numeric_limits<int>::max(),
            std::numeric_limits<int>::max()
        };
        for (const sgct::config::Window& window : cluster.nodes.front().windows) {
            if (window.size.x <= size.x && window.size.y <= size.y) {
                size = window.size;
            }
            else {
                // The window size is bigger than a previous one, so we gotta bail
                return true;
            }
        }

        // We got to the end without running into any problems, so we are golden
        return false;
    }
} // namespace

SgctEdit::SgctEdit(QWidget* parent, std::string userConfigPath)
    : QDialog(parent)
    , _userConfigPath(std::move(userConfigPath))
{
    QList<QScreen*> screens = qApp->screens();
    setWindowTitle("Window Configuration Editor");
    
    int nScreensManaged = std::min(static_cast<int>(screens.length()), 4);
    std::vector<QRect> monitorSizes;
    for (int s = 0; s < nScreensManaged; ++s) {
        QSize size = screens[s]->size();
        QRect geometry = screens[s]->availableGeometry();
        int actualWidth = std::max(size.width(), geometry.width());
        int actualHeight = std::max(size.height(), geometry.height());
        monitorSizes.emplace_back(
            geometry.x(),
            geometry.y(),
            static_cast<int>(actualWidth * screens[s]->devicePixelRatio()),
            static_cast<int>(actualHeight * screens[s]->devicePixelRatio())
        );
    }

    createWidgets(monitorSizes);
}

void SgctEdit::createWidgets(const std::vector<QRect>& monitorSizes) {
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    sgct::quat orientation = { 0.f, 0.f, 0.f, 0.f };
    if (_cluster.scene.has_value() && _cluster.scene->orientation.has_value()) {
        orientation = *_cluster.scene->orientation;
    }
    {
        MonitorBox* monitorBox = new MonitorBox(
            MonitorWidgetSize,
            monitorSizes,
            MaxNumberWindows,
            _colorsForWindows,
            this
        );
        layout->addWidget(monitorBox, 0, Qt::AlignCenter);

        QFrame* displayFrame = new QFrame;
        displayFrame->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);

        QBoxLayout* displayLayout = new QVBoxLayout(displayFrame);
        _displayWidget = new DisplayWindowUnion(
            monitorSizes,
            MaxNumberWindows,
            _colorsForWindows,
            this
        );
        connect(
            _displayWidget, &DisplayWindowUnion::windowChanged,
            monitorBox, &MonitorBox::windowDimensionsChanged
        );
        connect(
            _displayWidget, &DisplayWindowUnion::nWindowsChanged,
            monitorBox, &MonitorBox::nWindowsDisplayedChanged
        );
        _displayWidget->addWindow();
        
        displayLayout->addWidget(_displayWidget);
        
        layout->addWidget(displayFrame);
    }
    
    _settingsWidget = new SettingsWidget(orientation, this);
    layout->addWidget(_settingsWidget);
    
    {
        QHBoxLayout* layoutButtonBox = new QHBoxLayout;
        layoutButtonBox->addStretch(1);

        QFrame* bottomBorder = new QFrame;
        bottomBorder->setFrameShape(QFrame::HLine);
        layout->addWidget(bottomBorder);

        _cancelButton = new QPushButton("Cancel");
        _cancelButton->setToolTip("Cancel changes");
        _cancelButton->setFocusPolicy(Qt::NoFocus);
        connect(_cancelButton, &QPushButton::released, this, &SgctEdit::reject);
        layoutButtonBox->addWidget(_cancelButton);

        _saveButton = new QPushButton("Save As");
        _saveButton->setToolTip("Save configuration changes");
        _saveButton->setFocusPolicy(Qt::NoFocus);
        connect(_saveButton, &QPushButton::released, this, &SgctEdit::save);
        layoutButtonBox->addWidget(_saveButton);

        _applyButton = new QPushButton("Apply Without Saving");
        _applyButton->setToolTip("Apply configuration changes without saving to file");
        _applyButton->setFocusPolicy(Qt::NoFocus);
        connect(_applyButton, &QPushButton::released, this, &SgctEdit::apply);
        layoutButtonBox->addWidget(_applyButton);

        layout->addLayout(layoutButtonBox);
    }
}

std::filesystem::path SgctEdit::saveFilename() const {
    return _saveTarget;
}

void SgctEdit::save() {
    sgct::config::Cluster cluster = generateConfiguration();
    if (hasWindowIssues(cluster)) {
        int ret = QMessageBox::warning(
            this,
            "Window Sizes Incompatible",
            "Window sizes for multiple windows have to be strictly ordered, meaning that "
            "the size of window 1 has to be bigger in each dimension than window 2, "
            "window 2 has to be bigger than window 3 (if it exists), and window 3 has to "
            "be bigger than window 4.\nOtherwise, rendering errors might occur.\n\nAre "
            "you sure you want to continue?",
            QMessageBox::StandardButtons(QMessageBox::Yes | QMessageBox::No)
        );
        if (ret == QMessageBox::No) {
            return;
        }
    }

    QString fileName = QFileDialog::getSaveFileName(
        this,
        "Save Window Configuration File",
        QString::fromStdString(_userConfigPath),
        "Window Configuration (*.json)",
        nullptr
#ifdef __linux__
        // Linux in Qt5 and Qt6 crashes when trying to access the native dialog here
        , QFileDialog::DontUseNativeDialog
#endif
    );
    if (!fileName.isEmpty()) {
        _saveTarget = fileName.toStdString();
        _cluster = std::move(cluster);
        accept();
    }
}

void SgctEdit::apply() {
    sgct::config::Cluster cluster = generateConfiguration();
    if (hasWindowIssues(cluster)) {
        int ret = QMessageBox::warning(
            this,
            "Window Sizes Incompatible",
            "Window sizes for multiple windows have to be strictly ordered, meaning that "
            "the size of window 1 has to be bigger in each dimension than window 2, "
            "window 2 has to be bigger than window 3 (if it exists), and window 3 has to "
            "be bigger than window 4.\nOtherwise, rendering errors might occur.\n\nAre "
            "you sure you want to continue?",
            QMessageBox::Yes | QMessageBox::No
        );
        if (ret == QMessageBox::No) {
            return;
        }
    }

    std::string userCfgTempDir = _userConfigPath;
    if (userCfgTempDir.back() != '/') {
        userCfgTempDir += '/';
    }
    userCfgTempDir += "temp";
    if (!std::filesystem::is_directory(absPath(userCfgTempDir))) {
        std::filesystem::create_directories(absPath(userCfgTempDir));
    }
    _saveTarget = userCfgTempDir + "/apply-without-saving.json";
    _cluster = std::move(cluster);
    accept();
}

sgct::config::Cluster SgctEdit::generateConfiguration() const {
    sgct::config::Cluster cluster;

    sgct::config::Scene scene;
    scene.orientation = _settingsWidget->orientation();
    cluster.scene = std::move(scene);

    cluster.masterAddress = "localhost";

    if (_settingsWidget->vsync()) {
        sgct::config::Settings::Display display;
        display.swapInterval = 1;
        
        sgct::config::Settings settings;
        settings.display = display;

        cluster.settings = settings;
    }

    sgct::config::Node node;
    node.address = "localhost";
    node.port = 20401;

    // Save Windows
    unsigned int windowIndex = 0;
    for (WindowControl* wCtrl : _displayWidget->windowControls()) {
        sgct::config::Window window = wCtrl->generateWindowInformation();

        window.id = windowIndex++;
        node.windows.push_back(std::move(window));
    }

    if (_settingsWidget->showUiOnFirstWindow()) {
        sgct::config::Window& window = node.windows.front();
        window.viewports.back().isTracked = false;
        window.tags.push_back("GUI");
        window.draw2D = true;
        window.draw3D = false;
    }

    cluster.nodes.push_back(node);

    sgct::config::User user;
    user.eyeSeparation = 0.065f;
    user.position = { 0.f, 0.f, 4.f };
    cluster.users = { user };

    return cluster;
}

sgct::config::Cluster SgctEdit::cluster() const {
    return _cluster;
}
