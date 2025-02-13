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
    constexpr QRect MonitorWidgetSize = QRect(0, 0, 500, 500);
    constexpr int MaxNumberWindows = 4;

    const std::array<QColor, 4> ColorsForWindows = {
        QColor(43, 158, 195),
        QColor(252, 171, 16),
        QColor(68, 175, 105),
        QColor(248, 51, 60)
    };

    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

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
        const QList<QScreen*> screens = qApp->screens();
        const int nScreensManaged = std::min(static_cast<int>(screens.length()), 4);
        std::vector<QRect> monitorSizes;
        for (int s = 0; s < nScreensManaged; ++s) {
            const QSize size = screens[s]->size();
            const QRect geometry = screens[s]->availableGeometry();
            const int actualWidth = std::max(size.width(), geometry.width());
            const int actualHeight = std::max(size.height(), geometry.height());
            monitorSizes.emplace_back(
                geometry.x(),
                geometry.y(),
                static_cast<int>(actualWidth * screens[s]->devicePixelRatio()),
                static_cast<int>(actualHeight * screens[s]->devicePixelRatio())
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

    const size_t nWindows = _cluster.nodes.front().windows.size();
    std::vector<QRect> monitorSizes = createMonitorInfoSet();
    createWidgets(monitorSizes, static_cast<unsigned int>(nWindows), false);
    const size_t existingWindowsControlSize = _displayWidget->windowControls().size();
    for (size_t i = 0; i < nWindows; i++) {
        sgct::config::Window& w = _cluster.nodes.front().windows[i];
        WindowControl* wCtrl = _displayWidget->windowControls()[i];
        if (i < existingWindowsControlSize && wCtrl) {
            unsigned int monitorNum = 0;
            if (w.monitor.has_value()) {
                monitorNum = static_cast<unsigned int>(*w.monitor);
                if (monitorNum > (monitorSizes.size() - 1)) {
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
                if (monitorSizes.size() > monitorNum) {
                    posX -= monitorSizes[monitorNum].x();
                    posY -= monitorSizes[monitorNum].y();
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
        std::visit(overloaded {
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

    //
    // Setup state of UI on first window
    bool firstWindowGuiIsEnabled = (nWindows > 1);
    int graphicsSelectionForFirstWindow = 0;
    int nGuiRenderTagsFound = 0;
    nWindowsDisplayedChanged(static_cast<int>(nWindows));

    for (size_t i = 0; i < nWindows; i++) {
        sgct::config::Window& w = _cluster.nodes.front().windows[i];
        // First window needs to have "GUI" tag if this mode is set
        if (i == 0) {
            firstWindowGuiIsEnabled =
                std::find(w.tags.begin(), w.tags.end(), "GUI") != w.tags.end();
            if (std::find(w.tags.begin(), w.tags.end(), "GUI_No_Render") != w.tags.end())
            {
                graphicsSelectionForFirstWindow = 0;
                nGuiRenderTagsFound++;
            }
            for (int winNum = 0; winNum <= 4; ++winNum) {
                const std::string searchTag = "GUI_Render_Win" + std::to_string(winNum);
                if (std::find(w.tags.begin(), w.tags.end(), searchTag) != w.tags.end()) {
                    graphicsSelectionForFirstWindow = winNum;
                    nGuiRenderTagsFound++;
                }
            }
        }
        // If first window only renders 2D, and all subsequent windows do not, then
        // will enable the checkbox option for showing GUI only on first window
        if (w.draw2D.has_value()) {
            firstWindowGuiIsEnabled &= (i == 0) ? *w.draw2D : !*w.draw2D;
        }
        else {
            firstWindowGuiIsEnabled = false;
        }
    }

    if ((nGuiRenderTagsFound > 1) ||
        (graphicsSelectionForFirstWindow > static_cast<int>(nWindows)))
    {
        graphicsSelectionForFirstWindow = 0;
    }

    _showUiOnFirstWindow->setChecked(firstWindowGuiIsEnabled);
    if (firstWindowGuiIsEnabled) {
        // Call these again in order to ensure that GUI is configured correctly based on
        // the values read from the config file
        _showUiOnFirstWindow->setEnabled(true);
        _firstWindowGraphicsSelection->setEnabled(true);
        nWindowsDisplayedChanged(static_cast<int>(nWindows));
    }
    _firstWindowGraphicsSelection->setCurrentIndex(graphicsSelectionForFirstWindow);

    _checkBoxVsync->setChecked(
        _cluster.settings.has_value() &&
        _cluster.settings->display.has_value() &&
        _cluster.settings->display->swapInterval
    );
}

void SgctEdit::createWidgets(const std::vector<QRect>& monitorSizes,
                             unsigned int nWindows, bool setToDefaults)
{
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    sgct::quat orientation = sgct::quat(0.f, 0.f, 0.f, 0.f);
    if (_cluster.scene.has_value() && _cluster.scene->orientation.has_value()) {
        orientation = *_cluster.scene->orientation;
    }

    //
    // Monitor widget at the top of the window
    {
        MonitorBox* monitorBox = new MonitorBox(
            MonitorWidgetSize,
            monitorSizes,
            MaxNumberWindows,
            ColorsForWindows,
            this
        );
        layout->addWidget(monitorBox, 0, Qt::AlignCenter);

        QFrame* displayFrame = new QFrame;
        displayFrame->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);

        QBoxLayout* displayLayout = new QVBoxLayout(displayFrame);
        _displayWidget = new DisplayWindowUnion(
            monitorSizes,
            MaxNumberWindows,
            ColorsForWindows,
            setToDefaults,
            nWindows,
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
        displayLayout->addWidget(_displayWidget);

        layout->addWidget(displayFrame);
    }


    QWidget* settingsContainer = new QWidget;
    layout->addWidget(settingsContainer);
    QBoxLayout* settingsLayout = new QVBoxLayout(settingsContainer);
    settingsLayout->setContentsMargins(0, 0, 0, 0);

    QBoxLayout* firstWindowSelectionLayout = new QHBoxLayout;

    //
    // Show UI in specific window
    _showUiOnFirstWindow = new QCheckBox(
        "Show user interface only on first window using graphics:"
    );
    _showUiOnFirstWindow->setChecked(false);
    _showUiOnFirstWindow->setEnabled(false);
    _showUiOnFirstWindow->setToolTip(
        "If enabled the first window is marked as a GUI window resulting in the user "
        "interface only being shown\non that window and the rendering is suppressed on "
        "this first window. The remaining windows will render\nnormally but they will "
        "not show the user interface"
    );
    connect(
        _showUiOnFirstWindow, &QCheckBox::clicked,
        this, &SgctEdit::firstWindowGuiOptionClicked
    );
    firstWindowSelectionLayout->addWidget(_showUiOnFirstWindow);

    _firstWindowGraphicsSelection = new QComboBox;
    _firstWindowGraphicsSelection->setToolTip(
        "Select the contents of the first window to match one of the other windows"
    );
    _firstWindowGraphicsSelection->setFixedWidth(150);
    connect(
        _showUiOnFirstWindow, &QCheckBox::clicked,
        _firstWindowGraphicsSelection, &QComboBox::setEnabled
    );
    firstWindowSelectionLayout->addWidget(_firstWindowGraphicsSelection);
    firstWindowSelectionLayout->addStretch();
    settingsLayout->addLayout(firstWindowSelectionLayout);


    //
    // VSync settings
    _checkBoxVsync = new QCheckBox("Enable VSync");
    _checkBoxVsync->setToolTip(
        "If enabled the framerate will be locked to the refresh rate of the monitor"
    );
    settingsLayout->addWidget(_checkBoxVsync);


    //
    // Orientation specification
    QLabel* labelOrientation = new QLabel("Orientation");
    settingsLayout->addWidget(labelOrientation);

    QWidget* orientationContainer = new QWidget;
    settingsLayout->addWidget(orientationContainer);
    QGridLayout* layoutWindow = new QGridLayout(orientationContainer);

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
        layoutWindow->addWidget(labelRoll, 1, 0);

        _lineRoll = new QLineEdit;
        _lineRoll->setText(QString::number(glm::degrees(glm::roll(q))));
        _lineRoll->setToolTip(rollTip);
        QDoubleValidator* validatorRoll = new QDoubleValidator(-360.0, 360.0, 15);
        validatorRoll->setNotation(QDoubleValidator::StandardNotation);
        _lineRoll->setValidator(validatorRoll);
        layoutWindow->addWidget(_lineRoll, 1, 1);
    }
    {
        const QString yawTip = "Yaw, heading, or azimuth: negative numbers pan the "
            "camera to the left; positive numbers pan to the\nright. The allowed range "
            "is [-360, 360].";

        QLabel* labelYaw = new QLabel;
        labelYaw->setText("Yaw");
        labelYaw->setToolTip(yawTip);
        layoutWindow->addWidget(labelYaw, 2, 0);

        _lineYaw = new QLineEdit;
        _lineYaw->setText(QString::number(glm::degrees(glm::yaw(q))));
        _lineYaw->setToolTip(yawTip);
        QDoubleValidator* validatorYaw = new QDoubleValidator(-180.0, 180.0, 15, this);
        validatorYaw->setNotation(QDoubleValidator::StandardNotation);
        _lineYaw->setValidator(validatorYaw);
        layoutWindow->addWidget(_lineYaw, 2, 1);
    }
    
    connect(
        _displayWidget, &DisplayWindowUnion::nWindowsChanged,
        this, &SgctEdit::nWindowsDisplayedChanged
    );
    connect(
        _firstWindowGraphicsSelection, &QComboBox::currentTextChanged,
        this, &SgctEdit::firstWindowGraphicsSelectionChanged
    ); 


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
    generateConfiguration();

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

    ghoul_assert(!_configurationFilename.empty(), "Filename must not be empty");

    // Save the cluster configuration
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

void SgctEdit::generateConfiguration() {
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

    if (_cluster.nodes.empty()) {
        _cluster.nodes.emplace_back();
    }
    sgct::config::Node& node = _cluster.nodes.back();

    //
    // Generate vsync setup
    if (_checkBoxVsync->isChecked()) {
        if (!_cluster.settings || !_cluster.settings->display ||
            !_cluster.settings->display->swapInterval)
        {
            _cluster.settings = sgct::config::Settings {
                .display = sgct::config::Settings::Display {
                    .swapInterval = 1
                }
            };
        }
    }
    else {
        _cluster.settings = std::nullopt;
    }

    //
    // Resize windows according to selected
    std::vector<WindowControl*> windowControls = _displayWidget->activeWindowControls();
    for (size_t wIdx = 0; wIdx < windowControls.size(); ++wIdx) {
        if (node.windows.size() <= wIdx) {
            node.windows.emplace_back();
        }
        if (windowControls[wIdx]) {
            windowControls[wIdx]->generateWindowInformation(node.windows[wIdx]);
        }
    }
    while (node.windows.size() > windowControls.size()) {
        node.windows.pop_back();
    }

    //
    // Generate individual window settings
    for (size_t i = 0; i < node.windows.size(); i++) {
        // First apply default settings to each window...
        node.windows[i].id = static_cast<int>(i);
        node.windows[i].draw2D = true;
        node.windows[i].draw3D = true;
        node.windows[i].viewports.back().isTracked = true;

        // Remove tags
        constexpr std::array<std::string_view, 6> Tags = {
            "GUI",
            "GUI_No_Render",
            "GUI_Render_Win1",
            "GUI_Render_Win2",
            "GUI_Render_Win3",
            "GUI_Render_Win4"
        };
        for (const std::string_view tag : Tags) {
            node.windows[i].tags.erase(
                std::remove(
                    node.windows[i].tags.begin(),
                    node.windows[i].tags.end(),
                    tag
                ),
                node.windows[i].tags.end()
            );
        }

        // If "show UI on first window" option is enabled, then modify the settings
        // depending on if this is the first window or not
        if ((_showUiOnFirstWindow->isChecked() && _showUiOnFirstWindow->isEnabled())) {
            if (i == 0) {
                node.windows[i].tags.emplace_back("GUI");
                const int selected = _firstWindowGraphicsSelection->currentIndex();
                ghoul_assert(selected != -1, "Graphics selection should not be empty");
                ghoul_assert(
                    selected <= static_cast<int>(node.windows.size()),
                    "Invalid selected index"
                );

                if (selected == 0) {
                    node.windows[i].viewports.back().isTracked = false;
                    node.windows[i].tags.emplace_back("GUI_No_Render");
                }
                else {
                    node.windows[i].tags.emplace_back(
                        "GUI_Render_Win" + std::to_string(selected)
                    );
                    // Set first window viewport to mirror the selected window's viewport
                    node.windows[i].viewports = node.windows[(selected - 1)].viewports;
                }
                node.windows[i].draw2D = true;
                node.windows[i].draw3D = (selected > 0);
            }
            else {
                node.windows[i].draw2D = false;
                node.windows[i].draw3D = true;
            }
        }
    }
}

void SgctEdit::firstWindowGraphicsSelectionChanged() {
    if ((_showUiOnFirstWindow->isChecked() && _showUiOnFirstWindow->isEnabled())) {
        const int newSetting = _firstWindowGraphicsSelection->currentIndex();
        _displayWidget->activeWindowControls()[0]->setVisibilityOfProjectionGui(
            newSetting == 1
        );
    }
}

void SgctEdit::firstWindowGuiOptionClicked(bool checked) {
    if (checked) {
        firstWindowGraphicsSelectionChanged();
    }
    else {
        _displayWidget->activeWindowControls()[0]->setVisibilityOfProjectionGui(true);
    }
}

void SgctEdit::nWindowsDisplayedChanged(int newCount) {
    if (newCount == 1) {
        _displayWidget->activeWindowControls()[0]->setVisibilityOfProjectionGui(true);
    }
    else {
        firstWindowGraphicsSelectionChanged();
    }

    constexpr int CountOneWindow = 1;
    constexpr int CountTwoWindows = 2;

    QList<QString> graphicsOptions = { "None (GUI only)" };
    for (int i = CountOneWindow; i <= newCount; i++) {
        graphicsOptions.append("Window " + QString::number(i));
    }
    _firstWindowGraphicsSelection->clear();
    _firstWindowGraphicsSelection->addItems(graphicsOptions);
    _showUiOnFirstWindow->setEnabled(newCount > CountOneWindow);
    _firstWindowGraphicsSelection->setEnabled(newCount > CountOneWindow);
    int graphicsSelect = std::max(0, _firstWindowGraphicsSelection->currentIndex());
    if (graphicsSelect > newCount) {
        graphicsSelect = newCount;
    }
    _firstWindowGraphicsSelection->setCurrentIndex(graphicsSelect);

    if (newCount == CountOneWindow) {
        _stateOfUiOnFirstWindowWhenDisabled = _showUiOnFirstWindow->isChecked();
        _showUiOnFirstWindow->setChecked(false);
        _firstWindowGraphicsSelection->setEnabled(false);
    }
    else if (newCount == CountTwoWindows &&
        _stateOfUiOnFirstWindowPreviousCount == CountOneWindow)
    {
        if (_stateOfUiOnFirstWindowWhenDisabled) {
            _showUiOnFirstWindow->setChecked(true);
        }
        _firstWindowGraphicsSelection->setEnabled(_showUiOnFirstWindow->isChecked());
    }
    else {
        _firstWindowGraphicsSelection->setEnabled(_showUiOnFirstWindow->isChecked());
    }
    _stateOfUiOnFirstWindowPreviousCount = newCount;
}
