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
#include <QGridLayout>

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

    constexpr int LineEditWidthFixedWindowSize = 50;
    //constexpr int LineEditWidthFixedFov = 80;
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
    , _monitorIndex(monitorIndex)
    , _monitorIndexDefault(monitorIndex)
    , _windowIndex(windowIndex)
    , _monitorResolutions(monitorDims)
    , _colorForWindow(winColor)
    , _lockIcon(":/images/outline_locked.png")
    , _unlockIcon(":/images/outline_unlocked.png")
{
    createWidgets();
    resetToDefaults();
}

void WindowControl::createWidgets() {
    //    Column 0   Column 1   Column 2   Column 3   Column 4   Column 5 * Column 6 *
    //  *----------*----------*----------*----------*----------*----------*----------*
    //  |                                   Window {n}                               | R0
    //  | Name     * [ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo] | R1
    //  | Monitor  * DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD> | R2
    //  | Size     * [xxxxxx] *    x     * [yyyyyyy] *   px    *  <lock>  *          | R3
    //  | Offset   * [xxxxxx] *    ,     * [yyyyyyy] *   px    *          *          | R4
    //  | <Set to Fullscreen                                                       > | R5
    //  | [] Window Decoration                                                       | R6
    //  | [] UI only in this window                                                  | R7
    //  | ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Detail component~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ | R8
    //  *----------*----------*----------*----------*----------*----------*----------*

    QGridLayout* layout = new QGridLayout;
    layout->setColumnStretch(6, 1);
    
    {
        _labelWinNum = new QLabel("Window " + QString::number(_windowIndex + 1));
        QString colorStr = QString::fromStdString(
            fmt::format("QLabel {{ color : #{:02x}{:02x}{:02x}; }}",
                _colorForWindow.red(), _colorForWindow.green(), _colorForWindow.blue())
        );
        _labelWinNum->setStyleSheet(colorStr);
        layout->addWidget(_labelWinNum, 0, 0, 1, 7, Qt::AlignCenter);
    }
    {
        QString tip = "Enter a name for the window (displayed in title bar)";

        QLabel* labelName = new QLabel("Name");
        labelName->setToolTip(tip);
        layout->addWidget(labelName, 1, 0);

        _windowName = new QLineEdit;
        _windowName->setToolTip(tip);
        layout->addWidget(_windowName, 1, 1, 1, 6);
    }
    if (_monitorResolutions.size() > 1) {
        QString tip = "Select monitor where this window is located";

        QLabel* labelLocation = new QLabel("Monitor");
        labelLocation->setToolTip(tip);
        layout->addWidget(labelLocation, 2, 0);

        _comboMonitorSelect = new QComboBox;
        _comboMonitorSelect->addItems(monitorNames(_monitorResolutions));
        _comboMonitorSelect->setCurrentIndex(_monitorIndexDefault);
        _comboMonitorSelect->setToolTip(tip);
        layout->addWidget(_comboMonitorSelect, 2, 1, 1, 6);
        connect(
            _comboMonitorSelect, qOverload<int>(&QComboBox::currentIndexChanged),
            this, &WindowControl::onMonitorChanged
        );
    }
    {
        QLabel* labelSize = new QLabel("Size");
        labelSize->setToolTip("Enter window width & height in pixels");
        labelSize->setFixedWidth(55);
        layout->addWidget(labelSize, 3, 0);

        _sizeX = new QLineEdit;
        _sizeX->setValidator(new QIntValidator(10, MaxWindowSizePixels, this));
        _sizeX->setFixedWidth(LineEditWidthFixedWindowSize);
        _sizeX->setToolTip("Enter window width (pixels)");
        layout->addWidget(_sizeX, 3, 1);
        connect(_sizeX, &QLineEdit::textChanged, this, &WindowControl::onSizeXChanged);

        QLabel* labelDelim = new QLabel("x");
        labelDelim->setSizePolicy(
            QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum)
        );
        layout->addWidget(labelDelim, 3, 2);

        _sizeY = new QLineEdit;
        _sizeY->setValidator(new QIntValidator(10, MaxWindowSizePixels, this));
        _sizeY->setFixedWidth(LineEditWidthFixedWindowSize);
        _sizeY->setToolTip("Enter window height (pixels)");
        layout->addWidget(_sizeY, 3, 3, Qt::AlignLeft);
        connect(_sizeY, &QLineEdit::textChanged, this, &WindowControl::onSizeYChanged);

        QLabel* labelUnit = new QLabel("px");
        labelUnit->setSizePolicy(
            QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum)
        );
        layout->addWidget(labelUnit, 3, 4, Qt::AlignLeft);

        _buttonLockAspectRatio = new QPushButton;
        _buttonLockAspectRatio->setIcon(_unlockIcon);
        _buttonLockAspectRatio->setFocusPolicy(Qt::NoFocus);
        _buttonLockAspectRatio->setToolTip("Locks/Unlocks size aspect ratio");
        layout->addWidget(_buttonLockAspectRatio, 3, 5, Qt::AlignLeft);
        connect(
            _buttonLockAspectRatio, &QPushButton::released,
            this, &WindowControl::onAspectRatioLockClicked
        );
    }
    {
        std::string baseTip =
            "Enter {} location of window's upper left corner from monitor's {} (pixels)";

        QLabel* labelOffset = new QLabel("Offset");
        labelOffset->setToolTip(
            "Enter x,y location of window's upper left corner from monitor's upper-left "
            "corner origin (pixels)"
        );
        labelOffset->setFixedWidth(55);
        layout->addWidget(labelOffset, 4, 0);

        _offsetX = new QLineEdit;
        _offsetX->setValidator(
            new QIntValidator(-MaxWindowSizePixels, MaxWindowSizePixels, this)
        );
        _offsetX->setToolTip(
            "Enter x location of window's upper left corner from monitor's left side "
            "(pixels)"
        );
        _offsetX->setFixedWidth(LineEditWidthFixedWindowSize);
        layout->addWidget(_offsetX, 4, 1);
        connect(_offsetX, &QLineEdit::textChanged, this, &WindowControl::onOffsetXChanged);

        QLabel* labelComma = new QLabel(",");
        labelComma->setSizePolicy(
            QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum
        ));
        layout->addWidget(labelComma, 4, 2);

        _offsetY = new QLineEdit;
        _offsetY->setValidator(
            new QIntValidator(-MaxWindowSizePixels, MaxWindowSizePixels, this)
        );
        _offsetY->setToolTip(
            "Enter y location of window's upper left corner from monitor's top edge "
            "(pixels)"
        );
        _offsetY->setFixedWidth(LineEditWidthFixedWindowSize);
        layout->addWidget(_offsetY, 4, 3, Qt::AlignLeft);
        connect(
            _offsetY, &QLineEdit::textChanged,
            this, &WindowControl::onOffsetYChanged
        );

        QLabel* labelUnit = new QLabel("px");
        labelUnit->setSizePolicy(QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum));
        layout->addWidget(labelUnit, 4, 4, Qt::AlignLeft);
    }
    {
        QPushButton* setFullscreen = new QPushButton("Set to Fullscreen");
        setFullscreen->setToolTip(
            "If enabled, the window will be created in an exclusive fullscreen mode. The "
            "size of this\nwindow will be set to the screen resolution, and the window "
            "decoration automatically disabled."
        );
        setFullscreen->setFocusPolicy(Qt::NoFocus);
        layout->addWidget(setFullscreen, 5, 0, 1, 7);
        connect(
            setFullscreen, &QPushButton::released,
            this, &WindowControl::onFullscreenClicked
        );

        _checkBoxWindowDecor = new QCheckBox("Window Decoration");
        _checkBoxWindowDecor->setChecked(true);
        _checkBoxWindowDecor->setToolTip(
            "If enabled, the window will not have a border frame or title bar, and no\n "
            "controls for minimizing/maximizing, resizing, or closing the window."
        );
        layout->addWidget(_checkBoxWindowDecor, 6, 0, 1, 7);
        
        _checkBoxWebGui = new QCheckBox("UI only in this window");
        _checkBoxWebGui->setToolTip(
            "If enabled, the window will be dedicated solely to displaying the GUI "
            "controls, and will not\nrender any 3D content. All other window(s) will "
            "render in 3D but will not have GUI controls."
        );
        layout->addWidget(_checkBoxWebGui, 7, 0, 1, 7);
        connect(
            _checkBoxWebGui, &QCheckBox::stateChanged,
            this, &WindowControl::onWebGuiSelection
        );
    }
    {
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
        borderProjectionGroup->setVisible(true);
        borderProjectionGroup->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
        borderProjectionGroup->setSizePolicy(
            QSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed)
        );
        borderProjectionGroup->setLayout(layoutProjectionGroup);
        
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
        
        _labelQuality = new QLabel("Quality");
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
        _labelFovH = new QLabel("Horizontal FOV");
        QString hfovTip = "The total horizontal field of view of the viewport (degrees). "
            "Internally,\nthe values for 'left' & 'right' will each be half this value.";
        _labelFovH->setToolTip(hfovTip);
        layoutFovH->addWidget(_labelFovH);

        layoutFovH->addStretch(1);

        _lineFovH = new QLineEdit(QString::number(DefaultFovH));
        _lineFovH->setEnabled(false);
        _lineFovH->setValidator(new QDoubleValidator(-180.0, 180.0, 10, this));
        _lineFovH->setToolTip(hfovTip);
        layoutFovH->addWidget(_lineFovH);
        
        QHBoxLayout* layoutFovV = new QHBoxLayout;
        _labelFovV = new QLabel("Vertical FOV");
        QString vfovTip = "The total vertical field of view of the viewport (degrees). "
            "Internally,\nthe values for 'up' & 'down' will each be half this value.";
        _labelFovV->setToolTip(vfovTip);
        layoutFovV->addWidget(_labelFovV);
        
        layoutFovV->addStretch(1);
        
        _lineFovV = new QLineEdit(QString::number(DefaultFovV));
        _lineFovV->setEnabled(false);
        _lineFovV->setValidator(new QDoubleValidator(-90.0, 90.0, 10, this));
        _lineFovV->setToolTip(vfovTip);
        layoutFovV->addWidget(_lineFovV);
        
        layoutProjectionGroup->addLayout(layoutFovH);
        layoutProjectionGroup->addLayout(layoutFovV);
        
        QHBoxLayout* layoutHeightOffset = new QHBoxLayout;
        _labelHeightOffset = new QLabel("Height Offset");
        QString heightTip = "Offsets the height from which the cylindrical projection "
            "is generated.\nThis is, in general, only necessary if the user position is "
            "offset and\ncountering that offset is desired in order to continue producing"
            "\na 'standard' cylindrical projection.";
        _labelHeightOffset->setToolTip(heightTip);
        layoutHeightOffset->addWidget(_labelHeightOffset);

        _lineHeightOffset = new QLineEdit(QString::number(DefaultHeightOffset));
        _lineHeightOffset->setValidator(new QDoubleValidator(-1000000.0, 1000000.0, 12, this));
        _lineHeightOffset->setToolTip(heightTip);
        layoutHeightOffset->addWidget(_lineHeightOffset);
        
        layoutHeightOffset->addStretch(1);
        
        layoutProjectionGroup->addLayout(layoutHeightOffset);
        
        layout->addWidget(borderProjectionGroup, 8, 0, 1, 7);
    }


    _comboProjection->setCurrentIndex(0);
    onProjectionChanged(static_cast<unsigned int>(ProjectionIndices::Planar));
    _comboQuality->setCurrentIndex(2);
    setLayout(layout);
}

void WindowControl::resetToDefaults() {
    determineIdealWindowSize();
    _windowName->clear();
    _monitorIndex = _monitorIndexDefault;
    if (_monitorResolutions.size() > 1) {
        _comboMonitorSelect->setCurrentIndex(_monitorIndexDefault);
    }
    _checkBoxWindowDecor->setChecked(true);
    _checkBoxWebGui->setChecked(false);
    _checkBoxSpoutOutput->setChecked(false);
    _comboProjection->setCurrentIndex(static_cast<int>(ProjectionIndices::Planar));
    onProjectionChanged(_comboProjection->currentIndex());
    _lineFovH->setText(QString::number(DefaultFovH));
    _lineFovV->setText(QString::number(DefaultFovV));
    _lineHeightOffset->setText(QString::number(DefaultHeightOffset));
    _comboQuality->setCurrentIndex(2);
    emit windowChanged(_monitorIndex, _windowIndex, _windowDims);
}

void WindowControl::determineIdealWindowSize() {
    constexpr float IdealScaleVerticalLines = 2.f / 3.f;
    constexpr int PrimaryMonitorIdx = 0;

    _windowDims = DefaultWindowSizes[_windowIndex];
    _offsetX->setText(QString::number(_windowDims.x()));
    _offsetY->setText(QString::number(_windowDims.y()));
    float newHeight =
        static_cast<float>(_monitorResolutions[PrimaryMonitorIdx].height()) *
        IdealScaleVerticalLines;
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
    emit windowChanged(_monitorIndex, _windowIndex, _windowDims);
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
    emit windowChanged(_monitorIndex, _windowIndex, _windowDims);
    if (_fovLocked) {
        updatePlanarLockedFov();
    }
}

void WindowControl::onOffsetXChanged(const QString& newText) {
    float prevWidth = _windowDims.width();
    try {
        _windowDims.setX(newText.toInt());
        _windowDims.setWidth(prevWidth);
        emit windowChanged(_monitorIndex, _windowIndex, _windowDims);
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
        emit windowChanged(_monitorIndex, _windowIndex, _windowDims);
    }
    catch (const std::exception&) {
        // See comment in onOffsetXChanged
    }
}

void WindowControl::onFullscreenClicked() {
    _offsetX->setText("0");
    _offsetY->setText("0");
    _sizeX->setText(QString::number(_monitorResolutions[_monitorIndex].width()));
    _sizeY->setText(QString::number(_monitorResolutions[_monitorIndex].height()));
    _checkBoxWindowDecor->setChecked(false);
}

void WindowControl::onWebGuiSelection() {
    if (_checkBoxWebGui->isChecked()) {
        emit webGuiChanged(_windowIndex);
    }
}

void WindowControl::onSpoutSelection() {
    if (!_checkBoxSpoutOutput->isChecked()) {
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
    _monitorIndex = newSelection;
    emit windowChanged(_monitorIndex, _windowIndex, _windowDims);
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
    _checkBoxWebGui->setChecked(false);
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
    return _checkBoxWindowDecor->isChecked();
}

bool WindowControl::isGuiWindow() const {
    return _checkBoxWebGui->isChecked();
}

bool WindowControl::isSpoutSelected() const {
    return _checkBoxSpoutOutput->isChecked();
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
    return _monitorIndex;
}
