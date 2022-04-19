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

#include "sgctedit/windowcontrol.h"

#include <ghoul/fmt.h>
#include "sgctedit/displaywindowunion.h"
#include "sgctedit/monitorbox.h"

namespace {
    const QList<QString> MonitorNames = {
        "Primary", "Secondary", "Tertiary", "Quaternary"
    };

    const QList<QString> ProjectionTypes = {
        "Planar", "Fisheye", "Spherical Mirror", "Cylindrical", "Equirectangular"
    };

    const QList<QString> QualityTypes = {
        "Low (256)", "Medium (512)", "High (1K)", "1.5K (1536)", "2K (2048)", "4K (4096)",
        "8K (8192)", "16K (16384)", "32K (32768)", "64K (65536)"
    };

    constexpr int QualityValues[10] = {
        256, 512, 1024, 1536, 2048, 4096, 8192, 16384, 32768, 65536
    };

    constexpr QRectF DefaultWindowSizes[4] = {
        { 50.f, 50.f, 1280.f, 720.f },
        { 150.f, 150.f, 1280.f, 720.f },
        { 50.f, 50.f, 1280.f, 720.f },
        { 150.f, 150.f, 1280.f, 720.f }
    };

    constexpr int LineEditWidthFixedWinSize = 50;
    constexpr int LineEditWidthFixedFov = 80;
    constexpr float DefaultFovH = 80.f;
    constexpr float DefaultFovV = 50.534f;
    constexpr float DefaultHeightOffset = 0.f;

    constexpr int MaxWindowSizePixels = 10000;

    QString resolutionText(QRect res) {
        return QString::number(res.width()) + "x" + QString::number(res.height());
    }

    QList<QString> monitorNames(const std::vector<QRect>& resolutions) {
        QList<QString> monitorNames;
        for (size_t i = 0; i < resolutions.size(); i++) {
            QString r = resolutionText(resolutions[i]);
            monitorNames.push_back(MonitorNames[i] + " (" + r + ")");
        }
        return monitorNames;
    }
} // namespace

WindowControl::WindowControl(unsigned int monitorIndex, unsigned int windowIndex,
                             const std::vector<QRect>& monitorDims,
                             const QColor& winColor, QWidget* parent)
    : QWidget(parent)
    , _monIndex(monitorIndex)
    , _monIndexDefault(monitorIndex)
    , _index(windowIndex)
    , _monitorResolutions(monitorDims)
    , _colorForWindow(winColor)
    , _lockIcon(":/images/outline_locked.png")
    , _unlockIcon(":/images/outline_unlocked.png")
{
    createWidgets();
    resetToDefaults();
}

void WindowControl::createWidgets() {
    QVBoxLayout* layout = new QVBoxLayout;
    
    {
        _labelWinNum = new QLabel;
        _labelWinNum->setText("Window " + QString::number(_index + 1));
        QString colorStr = QString::fromStdString(
            fmt::format("QLabel {{ color : #{:02x}{:02x}{:02x}; }}",
                _colorForWindow.red(), _colorForWindow.green(), _colorForWindow.blue())
        );
        _labelWinNum->setStyleSheet(colorStr);

        QHBoxLayout* layoutWinNum = new QHBoxLayout;
        layoutWinNum->addStretch(1);
        layoutWinNum->addWidget(_labelWinNum);
        layoutWinNum->addStretch(1);
        layout->addLayout(layoutWinNum);
    }
    {
        QHBoxLayout* layoutName = new QHBoxLayout;
        QString tip = "Enter a name for the window (displayed in title bar)";

        QLabel* labelName = new QLabel(this);
        labelName->setText("Name: ");
        labelName->setToolTip(tip);
        layoutName->addWidget(labelName);

        _windowName = new QLineEdit;
        _windowName->setFixedWidth(160);
        _windowName->setToolTip(tip);
        layoutName->addWidget(_windowName);

        layoutName->addStretch(1);
        layout->addLayout(layoutName);
    }
    if (_monitorResolutions.size() > 1) {
        QHBoxLayout* layoutMonitorNum = new QHBoxLayout;
        QString tip = "Select monitor where this window is located";

        QLabel* labelLocation = new QLabel;
        labelLocation->setText("Monitor: ");
        labelLocation->setToolTip(tip);
        layoutMonitorNum->addWidget(labelLocation);

        _comboMonitorSelect = new QComboBox;
        _comboMonitorSelect->addItems(monitorNames(_monitorResolutions));
        _comboMonitorSelect->setCurrentIndex(_monIndexDefault);
        _comboMonitorSelect->setToolTip(tip);
        layoutMonitorNum->addWidget(_comboMonitorSelect);
        connect(
            _comboMonitorSelect, qOverload<int>(&QComboBox::currentIndexChanged),
            this, &WindowControl::onMonitorChanged
        );
        layoutMonitorNum->addStretch(1);
        layout->addLayout(layoutMonitorNum);
    }
    {
        QHBoxLayout* layoutSize = new QHBoxLayout;

        QLabel* labelSize = new QLabel;
        labelSize->setToolTip("Enter window width & height in pixels");
        labelSize->setText("Size:");
        labelSize->setFixedWidth(55);
        layoutSize->addWidget(labelSize);

        _sizeX = new QLineEdit;
        _sizeX->setValidator(new QIntValidator(10, MaxWindowSizePixels, this));
        _sizeX->setFixedWidth(LineEditWidthFixedWinSize);
        _sizeX->setToolTip("Enter window width (pixels)");
        layoutSize->addWidget(_sizeX);
        connect(_sizeX, &QLineEdit::textChanged, this, &WindowControl::onSizeXChanged);

        QLabel* labelDelim = new QLabel;
        labelDelim->setText("x");
        labelDelim->setFixedWidth(9);
        layoutSize->addWidget(labelDelim);

        _sizeY = new QLineEdit;
        _sizeY->setValidator(new QIntValidator(10, MaxWindowSizePixels, this));
        _sizeY->setFixedWidth(LineEditWidthFixedWinSize);
        _sizeY->setToolTip("Enter window height (pixels)");
        layoutSize->addWidget(_sizeY);
        connect(_sizeY, &QLineEdit::textChanged, this, &WindowControl::onSizeYChanged);

        QLabel* labelUnit = new QLabel;
        labelUnit->setText(" px");
        layoutSize->addWidget(labelUnit);

        _buttonLockAspectRatio = new QPushButton;
        _buttonLockAspectRatio->setIcon(_unlockIcon);
        _buttonLockAspectRatio->setFocusPolicy(Qt::NoFocus);
        _buttonLockAspectRatio->setToolTip("Locks/Unlocks size aspect ratio");
        layoutSize->addWidget(_buttonLockAspectRatio);
        connect(
            _buttonLockAspectRatio, &QPushButton::released,
            this, &WindowControl::onAspectRatioLockClicked
        );

        layoutSize->addStretch(1);
        layout->addLayout(layoutSize);
    }
    {
        QHBoxLayout* layoutOffset = new QHBoxLayout;
        std::string baseTip =
            "Enter {} location of window's upper left corner from monitor's {} (pixels)";

        QLabel* labelOffset = new QLabel(this);
        labelOffset->setToolTip(QString::fromStdString(fmt::format(
            baseTip, "x,y", "upper-left corner origin")));
        labelOffset->setText("Offset:");
        labelOffset->setFixedWidth(55);
        layoutOffset->addWidget(labelOffset);

        _offsetX = new QLineEdit;
        _offsetX->setValidator(new QIntValidator(-MaxWindowSizePixels, MaxWindowSizePixels, this));
        _offsetX->setToolTip(QString::fromStdString(fmt::format(
            baseTip, "x", "left side")));
        _offsetX->setFixedWidth(LineEditWidthFixedWinSize);
        layoutOffset->addWidget(_offsetX);
        connect(_offsetX, &QLineEdit::textChanged, this, &WindowControl::onOffsetXChanged);

        QLabel* labelComma = new QLabel(this);
        labelComma->setText(",");
        labelComma->setFixedWidth(9);
        layoutOffset->addWidget(labelComma);

        _offsetY = new QLineEdit;
        _offsetY->setValidator(new QIntValidator(-MaxWindowSizePixels, MaxWindowSizePixels, this));
        _offsetY->setToolTip(QString::fromStdString(fmt::format(
            baseTip, "y", "top edge")));
        _offsetY->setFixedWidth(LineEditWidthFixedWinSize);
        layoutOffset->addWidget(_offsetY);
        connect(_offsetY, &QLineEdit::textChanged, this, &WindowControl::onOffsetYChanged);

        QLabel* labelUnit = new QLabel(this);
        labelUnit->setText(" px");
        layoutOffset->addWidget(labelUnit);

        layoutOffset->addStretch(1);
        layout->addLayout(layoutOffset);
    }
    {
        QHBoxLayout* layoutCheckboxesFull1 = new QHBoxLayout;
        QVBoxLayout* layoutCheckboxesFull2 = new QVBoxLayout;
        QHBoxLayout* layoutFullscreenButton = new QHBoxLayout;
        _fullscreenButton = new QPushButton;
        _fullscreenButton->setText("Set to Fullscreen");
        _fullscreenButton->setToolTip(
            "If enabled, the window will be created in an exclusive fullscreen mode. The "
            "size of this\nwindow will be set to the screen resolution, and the window "
            "decoration automatically disabled."
        );
        _fullscreenButton->setFocusPolicy(Qt::NoFocus);
        layoutFullscreenButton->addWidget(_fullscreenButton);
        connect(
            _fullscreenButton, &QPushButton::released,
            this, &WindowControl::onFullscreenClicked
        );
        layoutFullscreenButton->addStretch(1);
        layoutCheckboxesFull2->addLayout(layoutFullscreenButton);
        QHBoxLayout* layoutCBoxWindowDecor = new QHBoxLayout;
        _checkBoxWindowDecor = new QCheckBox("Window Decoration", this);
        _checkBoxWindowDecor->setCheckState(Qt::CheckState::Checked);
        _checkBoxWindowDecor->setToolTip(
            "If enabled, the window will not have a border frame or title bar, and no\n "
            "controls for minimizing/maximizing, resizing, or closing the window."
        );
        layoutCBoxWindowDecor->addWidget(_checkBoxWindowDecor);
        layoutCBoxWindowDecor->addStretch(1);
        layoutCheckboxesFull2->addLayout(layoutCBoxWindowDecor);
        QHBoxLayout* _layoutCBoxWebGui = new QHBoxLayout;
        _checkBoxWebGui = new QCheckBox("WebGUI only this window");
        _checkBoxWebGui->setToolTip(
            "If enabled, the window will be dedicated solely to displaying the GUI "
            "controls, and will not\nrender any 3D content. All other window(s) will "
            "render in 3D but will not have GUI controls."
        );
        _layoutCBoxWebGui->addWidget(_checkBoxWebGui);
        connect(
            _checkBoxWebGui, &QCheckBox::stateChanged,
            this, &WindowControl::onWebGuiSelection
        );
        _layoutCBoxWebGui->addStretch(1);
        layoutCheckboxesFull2->addLayout(_layoutCBoxWebGui);
        QVBoxLayout* layoutProjectionGroup = new QVBoxLayout;
        QHBoxLayout* layoutComboProjection = new QHBoxLayout;
        _comboProjection = new QComboBox;
        _comboProjection->addItems(ProjectionTypes);
        _comboProjection->setToolTip("Select from the supported window projection types");
        layoutComboProjection->addWidget(_comboProjection);
        connect(
            _comboProjection, qOverload<int>(&QComboBox::currentIndexChanged),
            this, &WindowControl::onProjectionChanged
        );
        _buttonLockFov = new QPushButton;
        _buttonLockFov->setIcon(_lockIcon);
        _buttonLockFov->setToolTip(
            "Locks and scales the Horizontal & Vertical field-of-view to the ideal "
            "settings based on aspect ratio."
        );
        _buttonLockFov->setFocusPolicy(Qt::NoFocus);
        layoutComboProjection->addWidget(_buttonLockFov);
        connect(
            _buttonLockFov, &QPushButton::released,
            this, &WindowControl::onFovLockClicked
        );
        layoutComboProjection->addStretch(1);
        layoutProjectionGroup->addLayout(layoutComboProjection);
        QFrame* borderProjectionGroup = new QFrame;
        borderProjectionGroup->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
        borderProjectionGroup->setLayout(layoutProjectionGroup);
        borderProjectionGroup->setVisible(true);
        QHBoxLayout* layoutCBoxSpoutOutput = new QHBoxLayout;
        _checkBoxSpoutOutput = new QCheckBox("Spout Output");
        _checkBoxSpoutOutput->setToolTip(
            "This projection method provides the ability to share the reprojected image "
            "using the Spout library.\nThis library only supports the Windows operating "
            "system. Spout makes it possible to make the rendered\nimages available to "
            "other real-time applications on the same machine for further processing.\n"
            "The SpoutOutputProjection option can work with either Fisheye or "
            "Equirectangular projection."
        );
        layoutCBoxSpoutOutput->addWidget(_checkBoxSpoutOutput);
        connect(
            _checkBoxSpoutOutput, &QCheckBox::stateChanged,
            this, &WindowControl::onSpoutSelection
        );
        layoutCBoxSpoutOutput->addStretch(1);
        layoutProjectionGroup->addLayout(layoutCBoxSpoutOutput);
        QHBoxLayout* layoutComboQuality = new QHBoxLayout;
        _labelQuality = new QLabel;
        _labelQuality->setText("Quality:");
        QString qualityTip = "Determines the pixel resolution of the projection "
            "rendering. The higher resolution,\nthe better the rendering quality, but at "
            "the expense of increased rendering times.";
        _labelQuality->setToolTip(qualityTip);
        layoutComboQuality->addWidget(_labelQuality);
        _comboQuality = new QComboBox;
        _comboQuality->addItems(QualityTypes);
        _comboQuality->setToolTip(qualityTip);
        layoutComboQuality->addWidget(_comboQuality);
        layoutComboQuality->addStretch(1);
        layoutProjectionGroup->addLayout(layoutComboQuality);
        QHBoxLayout* layoutFovH = new QHBoxLayout;
        _labelFovH = new QLabel;
        _labelFovH->setText("Horizontal FOV:");
        QString hfovTip = "The total horizontal field of view of the viewport (degrees). "
            "Internally,\nthe values for 'left' & 'right' will each be half this value.";
        _labelFovH->setToolTip(hfovTip);
        _lineFovH = new QLineEdit(QString::number(DefaultFovH));
        _lineFovH->setValidator(new QDoubleValidator(-180.0, 180.0, 10, this));
        _lineFovH->setToolTip(hfovTip);
        layoutFovH->addWidget(_labelFovH);
        layoutFovH->addStretch(1);
        layoutFovH->addWidget(_lineFovH);
        QHBoxLayout* layoutFovV = new QHBoxLayout;
        _labelFovV = new QLabel;
        _labelFovV->setText("Vertical FOV:");
        QString vfovTip = "The total vertical field of view of the viewport (degrees). "
            "Internally,\nthe values for 'up' & 'down' will each be half this value.";
        _labelFovV->setToolTip(vfovTip);
        _lineFovV = new QLineEdit(QString::number(DefaultFovV));
        _lineFovV->setValidator(new QDoubleValidator(-90.0, 90.0, 10, this));
        _lineFovV->setToolTip(vfovTip);
        layoutFovV->addWidget(_labelFovV);
        layoutFovV->addStretch(1);
        layoutFovV->addWidget(_lineFovV);
        _lineFovH->setFixedWidth(LineEditWidthFixedFov);
        _lineFovV->setFixedWidth(LineEditWidthFixedFov);
        _lineFovH->setEnabled(false);
        _lineFovV->setEnabled(false);
        layoutProjectionGroup->addLayout(layoutFovH);
        layoutProjectionGroup->addLayout(layoutFovV);
        QHBoxLayout* layoutHeightOffset = new QHBoxLayout;
        _labelHeightOffset = new QLabel;
        _labelHeightOffset->setText("Height Offset:");
        QString heightTip = "Offsets the height from which the cylindrical projection "
            "is generated.\nThis is, in general, only necessary if the user position is "
            "offset and\ncountering that offset is desired in order to continue producing"
            "\na 'standard' cylindrical projection.";
        _labelHeightOffset->setToolTip(heightTip);
        _lineHeightOffset = new QLineEdit(QString::number(DefaultHeightOffset));
        _lineHeightOffset->setValidator(new QDoubleValidator(-1000000.0, 1000000.0, 12, this));
        _lineHeightOffset->setToolTip(heightTip);
        layoutHeightOffset->addWidget(_labelHeightOffset);
        layoutHeightOffset->addWidget(_lineHeightOffset);
        layoutHeightOffset->addStretch(1);
        layoutProjectionGroup->addLayout(layoutHeightOffset);
        layoutCheckboxesFull2->addWidget(borderProjectionGroup);
        layoutCheckboxesFull1->addLayout(layoutCheckboxesFull2);
        layoutCheckboxesFull1->addStretch(1);
        layout->addLayout(layoutCheckboxesFull1);
    }

    layout->addStretch(1);

    _comboProjection->setCurrentIndex(0);
    onProjectionChanged(static_cast<unsigned int>(ProjectionIndices::Planar));
    _comboQuality->setCurrentIndex(2);
    setLayout(layout);
}

void WindowControl::resetToDefaults() {
    determineIdealWindowSize();
    _windowName->clear();
    _monIndex = _monIndexDefault;
    if (_monitorResolutions.size() > 1) {
        _comboMonitorSelect->setCurrentIndex(_monIndexDefault);
    }
    _checkBoxWindowDecor->setCheckState(Qt::CheckState::Checked);
    _checkBoxWebGui->setCheckState(Qt::CheckState::Unchecked);
    onWebGuiSelection(_checkBoxWebGui->checkState());
    _checkBoxSpoutOutput->setCheckState(Qt::CheckState::Unchecked);
    onSpoutSelection(_checkBoxSpoutOutput->checkState());
    _comboProjection->setCurrentIndex(static_cast<int>(ProjectionIndices::Planar));
    onProjectionChanged(_comboProjection->currentIndex());
    _lineFovH->setText(QString::number(DefaultFovH));
    _lineFovV->setText(QString::number(DefaultFovV));
    _lineHeightOffset->setText(QString::number(DefaultHeightOffset));
    _comboQuality->setCurrentIndex(2);
    emit windowChanged(_monIndex, _index, _windowDims);
}

void WindowControl::determineIdealWindowSize() {
    constexpr float IdealScaleVerticalLines = 2.f / 3.f;
    constexpr int PrimaryMonitorIdx = 0;

    _windowDims = DefaultWindowSizes[_index];
    _offsetX->setText(QString::number(_windowDims.x()));
    _offsetY->setText(QString::number(_windowDims.y()));
    float newHeight =
        static_cast<float>(_monitorResolutions[PrimaryMonitorIdx].height())
        * IdealScaleVerticalLines;
    float newWidth = newHeight * IdealAspectRatio;
    _windowDims.setHeight(newHeight);
    _windowDims.setWidth(newWidth);
    _sizeX->setText(QString::number(static_cast<int>(newWidth)));
    _sizeY->setText(QString::number(static_cast<int>(newHeight)));
}

void WindowControl::showWindowLabel(bool show) {
    _labelWinNum->setVisible(show);
}

void WindowControl::onSizeXChanged(const QString& newText) {
    _windowDims.setWidth(newText.toInt());
    if (_aspectRatioLocked) {
        int updatedHeight = _windowDims.width() / _aspectRatioSize;
        _sizeY->blockSignals(true);
        _sizeY->setText(QString::number(updatedHeight));
        _sizeY->blockSignals(false);
        _windowDims.setHeight(updatedHeight);
    }
    emit windowChanged(_monIndex, _index, _windowDims);
    if (_fovLocked) {
        updatePlanarLockedFov();
    }
}

void WindowControl::onSizeYChanged(const QString& newText) {
    _windowDims.setHeight(newText.toInt());
    if (_aspectRatioLocked) {
        int updatedWidth = _windowDims.height() * _aspectRatioSize;
        _sizeX->blockSignals(true);
        _sizeX->setText(QString::number(updatedWidth));
        _sizeX->blockSignals(false);
        _windowDims.setWidth(updatedWidth);
    }
    emit windowChanged(_monIndex, _index, _windowDims);
    if (_fovLocked) {
        updatePlanarLockedFov();
    }
}

void WindowControl::onOffsetXChanged(const QString& newText) {
    float prevWidth = _windowDims.width();
    try {
        _windowDims.setX(newText.toInt());
        _windowDims.setWidth(prevWidth);
        emit windowChanged(_monIndex, _index, _windowDims);
    }
    catch (const std::exception&) {
        // The QIntValidator ensures that the range is a +/- integer. However, it's 
        // possible to enter only a - character causing an exception, which is ignored
        // here (when user enters an integer after the - then the value will be updated)
    }
}

void WindowControl::onOffsetYChanged(const QString& newText) {
    float prevHeight = _windowDims.height();
    try {
        _windowDims.setY(newText.toInt());
        _windowDims.setHeight(prevHeight);
        emit windowChanged(_monIndex, _index, _windowDims);
    }
    catch (const std::exception&) {
        // See comment in onOffsetXChanged
    }
}

void WindowControl::onFullscreenClicked() {
    _offsetX->setText("0");
    _offsetY->setText("0");
    _sizeX->setText(QString::number(_monitorResolutions[_monIndex].width()));
    _sizeY->setText(QString::number(_monitorResolutions[_monIndex].height()));
    _checkBoxWindowDecor->setCheckState(Qt::Unchecked);
}

void WindowControl::onWebGuiSelection(int selectionState) {
    if (selectionState == Qt::Checked) {
        emit webGuiChanged(_index);
    }
}

void WindowControl::onSpoutSelection(int selectionState) {
    if (selectionState != Qt::Checked) {
        return;
    }

    WindowControl::ProjectionIndices currentProjectionSelection;
    currentProjectionSelection = static_cast<WindowControl::ProjectionIndices>(
        _comboProjection->currentIndex()
    );
    if ((currentProjectionSelection != ProjectionIndices::Equirectangular) &&
        (currentProjectionSelection != ProjectionIndices::Fisheye))
    {
        _comboProjection->setCurrentIndex(
            static_cast<int>(ProjectionIndices::Equirectangular)
        );
    }
}

void WindowControl::onMonitorChanged(int newSelection) {
    _monIndex = newSelection;
    emit windowChanged(_monIndex, _index, _windowDims);
}

void WindowControl::onProjectionChanged(int newSelection) {
    WindowControl::ProjectionIndices selected =
        static_cast<WindowControl::ProjectionIndices>(newSelection);
    _comboQuality->setVisible(selected != ProjectionIndices::Planar);
    _labelQuality->setVisible(selected != ProjectionIndices::Planar);
    _labelFovH->setVisible(selected == ProjectionIndices::Planar);
    _lineFovH->setVisible(selected == ProjectionIndices::Planar);
    _labelFovV->setVisible(selected == ProjectionIndices::Planar);
    _lineFovV->setVisible(selected == ProjectionIndices::Planar);
    _buttonLockFov->setVisible(selected == ProjectionIndices::Planar);
    _labelHeightOffset->setVisible(selected == ProjectionIndices::Cylindrical);
    _lineHeightOffset->setVisible(selected == ProjectionIndices::Cylindrical);
    _checkBoxSpoutOutput->setVisible(
        selected == ProjectionIndices::Fisheye ||
        selected == ProjectionIndices::Equirectangular
    );
}

void WindowControl::onAspectRatioLockClicked() {
    _aspectRatioLocked = !_aspectRatioLocked;
    _buttonLockAspectRatio->setIcon(_aspectRatioLocked ? _lockIcon : _unlockIcon);
    if (_aspectRatioLocked) {
        _aspectRatioSize = _windowDims.width() / _windowDims.height();
    }
}

void WindowControl::onFovLockClicked() {
    _fovLocked = !_fovLocked;
    _buttonLockFov->setIcon(_fovLocked ? _lockIcon : _unlockIcon);
    if (_fovLocked) {
        _lineFovH->setEnabled(false);
        _lineFovV->setEnabled(false);
        updatePlanarLockedFov();
    }
    else {
        _lineFovH->setEnabled(true);
        _lineFovV->setEnabled(true);
    }
}

void WindowControl::updatePlanarLockedFov() {
    const float currentAspectRatio = _windowDims.width() / _windowDims.height();
    const float relativeRatio = currentAspectRatio / IdealAspectRatio;
    if (relativeRatio >= 1.0) {
        _lineFovH->setText(QString::number(std::min(DefaultFovH *relativeRatio, 180.f)));
        _lineFovV->setText(QString::number(DefaultFovV));
    }
    else {
        _lineFovH->setText(QString::number(DefaultFovH));
        _lineFovV->setText(QString::number(std::min(DefaultFovV /relativeRatio, 180.f)));
    }
}

void WindowControl::uncheckWebGuiOption() {
    _checkBoxWebGui->setCheckState(Qt::Unchecked);
}

QRectF WindowControl::dimensions() const {
    return _windowDims;
}

std::string WindowControl::windowName() const {
    return _windowName->text().toStdString();
}

sgct::ivec2 WindowControl::windowSize() const {
    return {
        _sizeX->text().toInt(),
        _sizeY->text().toInt()
    };
}

sgct::ivec2 WindowControl::windowPos() const {
    return {
        _offsetX->text().toInt(),
        _offsetY->text().toInt()
    };
}

bool WindowControl::isDecorated() const {
    return (_checkBoxWindowDecor->checkState() == Qt::Checked);
}

bool WindowControl::isGuiWindow() const {
    return (_checkBoxWebGui->checkState() == Qt::Checked);
}

bool WindowControl::isSpoutSelected() const {
    return (_checkBoxSpoutOutput->checkState() == Qt::Checked);
}

WindowControl::ProjectionIndices WindowControl::projectionSelectedIndex() const {
    return
        static_cast<WindowControl::ProjectionIndices>(_comboProjection->currentIndex());
}

int WindowControl::qualitySelectedValue() const {
    return QualityValues[_comboQuality->currentIndex()];
}

float WindowControl::fovH() const {
    return _lineFovH->text().toFloat();
}

float WindowControl::fovV() const {
    return _lineFovV->text().toFloat();
}

float WindowControl::heightOffset() const {
    return _lineHeightOffset->text().toFloat();
}

unsigned int WindowControl::monitorNum() const {
    return _monIndex;
}
