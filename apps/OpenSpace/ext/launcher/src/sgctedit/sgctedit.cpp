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

#include "sgctedit/sgctedit.h"

#include <sgctedit/displaywindowunion.h>
#include <sgctedit/monitorbox.h>
#include <sgctedit/windowcontrol.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <QApplication>
#include <QCheckBox>
#include <QColor>
#include <QComboBox>
#include <QFileDialog>
#include <QLabel>
#include <QMessageBox>
#include <QPushButton>
#include <QScreen>
#include <QVBoxLayout>
#include <fstream>

namespace {
    constexpr int MaxNumberWindows = 4;

    // Returns true if the windows are not ordered correctly. 'Correct' in this means that
    // there is a smaller window defined before a bigger one
    // This check is only necessary until
    // https://github.com/OpenSpace/OpenSpace/issues/507
    // is fixed
    bool hasWindowIssues(const sgct::config::Cluster& cluster) {
        sgct::ivec2 size = sgct::ivec2(
            std::numeric_limits<int>::max(),
            std::numeric_limits<int>::max()
        );
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

    std::vector<QRect> createMonitorInfoSet() {
        std::vector<QRect> monitorSizes;
        for (QScreen* screen : qApp->screens()) {
            const QSize size = screen->size();
            const QRect geometry = screen->availableGeometry();
            const int actualWidth = std::max(size.width(), geometry.width());
            const int actualHeight = std::max(size.height(), geometry.height());
            monitorSizes.emplace_back(
                geometry.x(),
                geometry.y(),
                static_cast<int>(actualWidth * screen->devicePixelRatio()),
                static_cast<int>(actualHeight * screen->devicePixelRatio())
            );
        }
        return monitorSizes;
    }
} // namespace

SgctEdit::SgctEdit(sgct::config::Cluster cluster, std::string configName,
                   std::filesystem::path configBasePath, QWidget* parent)
    : QDialog(parent)
    , _cluster(std::move(cluster))
    , _userConfigPath(std::move(configBasePath))
    , _configurationFilename(std::move(configName))
{
    setWindowTitle("Window Configuration Editor");

    std::vector<QRect> monitorSizes = createMonitorInfoSet();

    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    //
    // Monitor widget at the top of the window
    {
        constexpr QRect MonitorWidgetSize = QRect(0, 0, 350, 350);

        MonitorBox* monitorBox = new MonitorBox(MonitorWidgetSize, monitorSizes);
        layout->addWidget(monitorBox, 0, Qt::AlignCenter);

        QFrame* displayFrame = new QFrame;
        displayFrame->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);

        _displayWidget = new DisplayWindowUnion(monitorSizes, MaxNumberWindows, this);
        connect(
            _displayWidget, &DisplayWindowUnion::windowChanged,
            monitorBox, &MonitorBox::windowDimensionsChanged
        );
        connect(
            _displayWidget, &DisplayWindowUnion::nWindowsChanged,
            monitorBox, &MonitorBox::nWindowsDisplayedChanged
        );
        // We initialize the widget after making the connections so that the monitorbox
        // widget will get informed about the windows and their sizes automatically
        _displayWidget->initialize(monitorSizes, _cluster);
        layout->addWidget(_displayWidget);
    }


    QWidget* settingsContainer = new QWidget;
    layout->addWidget(settingsContainer);
    QBoxLayout* settingsLayout = new QVBoxLayout(settingsContainer);
    settingsLayout->setContentsMargins(0, 0, 0, 0);
    settingsLayout->setSpacing(0);


    //
    // VSync settings
    _checkBoxVsync = new QCheckBox("Enable VSync");
    _checkBoxVsync->setToolTip(
        "If enabled the framerate will be locked to the refresh rate of the monitor"
    );
    _checkBoxVsync->setChecked(
        _cluster.settings.has_value() &&
        _cluster.settings->display.has_value() &&
        _cluster.settings->display->swapInterval
    );
    settingsLayout->addWidget(_checkBoxVsync);


    //
    // Orientation specification
    QWidget* orientationContainer = new QWidget;
    settingsLayout->addWidget(orientationContainer);
    QGridLayout* layoutWindow = new QGridLayout(orientationContainer);

    sgct::quat orientation = sgct::quat(0.f, 0.f, 0.f, 0.f);
    if (_cluster.scene.has_value() && _cluster.scene->orientation.has_value()) {
        orientation = *_cluster.scene->orientation;
    }
    glm::quat q = glm::quat(orientation.w, orientation.x, orientation.y, orientation.z);

    {
        const QString pitchTip = "Pitch or elevation: negative numbers tilt the camera "
            "downwards; positive numbers tilt upwards.\nThe allowed range is [-90, 90].";

        QLabel* labelPitch = new QLabel("Pitch");
        labelPitch->setToolTip(pitchTip);
        layoutWindow->addWidget(labelPitch, 0, 0);

        _linePitch = new QLineEdit;
        _linePitch->setText(QString::number(glm::degrees(glm::pitch(q))));
        _linePitch->setToolTip(pitchTip);
        QDoubleValidator* validatorPitch = new QDoubleValidator(-90.0, 90.0, 15);
        validatorPitch->setNotation(QDoubleValidator::StandardNotation);
        _linePitch->setValidator(validatorPitch);
        layoutWindow->addWidget(_linePitch, 0, 1);
    }
    {
        const QString rollTip = "Roll or bank: negative numbers rotate the camera "
            "counter-clockwise; positive numbers clockwise.\nThe allowed range is "
            "[-180, 180].";

        QLabel* labelRoll = new QLabel("Roll");
        labelRoll->setToolTip(rollTip);
        layoutWindow->addWidget(labelRoll, 0, 2);

        _lineRoll = new QLineEdit;
        _lineRoll->setText(QString::number(glm::degrees(glm::roll(q))));
        _lineRoll->setToolTip(rollTip);
        QDoubleValidator* validatorRoll = new QDoubleValidator(-360.0, 360.0, 15);
        validatorRoll->setNotation(QDoubleValidator::StandardNotation);
        _lineRoll->setValidator(validatorRoll);
        layoutWindow->addWidget(_lineRoll, 0, 3);
    }
    {
        const QString yawTip = "Yaw, heading, or azimuth: negative numbers pan the "
            "camera to the left; positive numbers pan to the\nright. The allowed range "
            "is [-360, 360].";

        QLabel* labelYaw = new QLabel;
        labelYaw->setText("Yaw");
        labelYaw->setToolTip(yawTip);
        layoutWindow->addWidget(labelYaw, 0, 4);

        _lineYaw = new QLineEdit;
        _lineYaw->setText(QString::number(glm::degrees(glm::yaw(q))));
        _lineYaw->setToolTip(yawTip);
        QDoubleValidator* validatorYaw = new QDoubleValidator(-180.0, 180.0, 15, this);
        validatorYaw->setNotation(QDoubleValidator::StandardNotation);
        _lineYaw->setValidator(validatorYaw);
        layoutWindow->addWidget(_lineYaw, 0, 5);
    }

    {
        QLabel* info = new QLabel(
            "The allowed ranges for pitch is [-90, 90], for roll [-180, 180], and for "
            "yaw [-360, 360]."
        );
        layoutWindow->addWidget(info, 1, 0, 1, 6);
    }
    

    //
    // Button box
    {
        QHBoxLayout* layoutButtonBox = new QHBoxLayout;
        layoutButtonBox->addStretch(1);

        QFrame* bottomBorder = new QFrame;
        bottomBorder->setFrameShape(QFrame::HLine);
        layout->addWidget(bottomBorder);

        QPushButton* cancelButton = new QPushButton("Cancel");
        cancelButton->setToolTip("Cancel changes");
        cancelButton->setFocusPolicy(Qt::NoFocus);
        connect(cancelButton, &QPushButton::released, this, &SgctEdit::reject);
        layoutButtonBox->addWidget(cancelButton);

        QPushButton* saveButton = new QPushButton("Save");
        saveButton->setToolTip("Save configuration changes");
        saveButton->setFocusPolicy(Qt::NoFocus);
        connect(saveButton, &QPushButton::released, this, &SgctEdit::saveCluster);
        layoutButtonBox->addWidget(saveButton);

        QPushButton* applyButton = new QPushButton("Apply Without Saving");
        applyButton->setToolTip("Apply configuration changes without saving to file");
        applyButton->setFocusPolicy(Qt::NoFocus);
        connect(applyButton, &QPushButton::released, this, &SgctEdit::apply);
        layoutButtonBox->addWidget(applyButton);

        layout->addLayout(layoutButtonBox);
    }
}

std::filesystem::path SgctEdit::saveFilename() const {
    return _configurationFilename;
}

void SgctEdit::saveCluster() {
    //
    // Generate configuration
    // Reconstitute the quaternion if the provided values are not 0
    const float pitch = glm::radians(_linePitch->text().toFloat());
    const float yaw = glm::radians(_lineYaw->text().toFloat());
    const float roll = glm::radians(_lineRoll->text().toFloat());
    if (pitch != 0.f && yaw != 0.f && roll != 0.f) {
        glm::quat q = glm::quat(glm::vec3(pitch, yaw, roll));
        _cluster.scene = {
            .orientation = sgct::quat(q.x, q.y, q.z, q.w)
        };
    }

    ghoul_assert(!_cluster.nodes.empty(), "There must be at least one node");
    sgct::config::Node& node = _cluster.nodes.back();

    //
    // Generate vsync setup
    if (_checkBoxVsync->isChecked()) {
        if (!_cluster.settings || !_cluster.settings->display ||
            !_cluster.settings->display->swapInterval)
        {
            _cluster.settings = sgct::config::Settings{
                .display = sgct::config::Settings::Display {
                    .swapInterval = 1
                }
            };
        }
    }
    else {
        _cluster.settings = std::nullopt;
    }

    _displayWidget->applyWindowSettings(node.windows);

    //
    // Generate individual window settings
    for (size_t i = 0; i < node.windows.size(); i++) {
        // First apply default settings to each window
        node.windows[i].id = static_cast<int8_t>(i);
        node.windows[i].viewports.back().isTracked = true;
    }

    if (hasWindowIssues(_cluster)) {
        const int ret = QMessageBox::warning(
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

    if (_configurationFilename.empty()) {
        const QString fileName = QFileDialog::getSaveFileName(
            this,
            "Save Window Configuration File",
            QString::fromStdString(_userConfigPath.string()),
            "Window Configuration (*.json)",
            nullptr
#ifdef __linux__
            // Linux in Qt5 and Qt6 crashes when trying to access the native dialog here
            , QFileDialog::DontUseNativeDialog
#endif // __linux__
        );
        if (fileName.isEmpty()) {
            return;
        }

        _configurationFilename = fileName.toStdString();
    }


    //
    // Save the cluster configuration
    ghoul_assert(!_configurationFilename.empty(), "Filename must not be empty");
    std::ofstream outFile;
    outFile.open(_configurationFilename, std::ofstream::out);
    if (outFile.good()) {
        sgct::config::GeneratorVersion genEntry = VersionMin;
        outFile << sgct::serializeConfig(_cluster, genEntry);
        accept();
    }
    else {
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(std::format(
                "Error writing data to file '{}'", _configurationFilename
            ))
        );
    }
}

void SgctEdit::apply() {
    std::filesystem::path userTmp = _userConfigPath / "temp";
    if (!std::filesystem::is_directory(absPath(userTmp))) {
        std::filesystem::create_directories(absPath(userTmp));
    }
    _configurationFilename = (userTmp / "apply-without-saving.json").string();
    saveCluster();
}
