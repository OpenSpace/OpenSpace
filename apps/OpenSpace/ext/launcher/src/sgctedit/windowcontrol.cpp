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
#include "sgctedit/display.h"
#include "sgctedit/monitorbox.h"

const std::string ProjectionTypeNames[5] = {"Planar", "Fisheye", "Spherical Mirror",
    "Cylindrical", "Equirectangular"};
QList<QString> ProjectionTypes = {
    QString::fromStdString(ProjectionTypeNames[static_cast<int>(
        WindowControl::ProjectionIndeces::Planar)]),
    QString::fromStdString(ProjectionTypeNames[static_cast<int>(
        WindowControl::ProjectionIndeces::Fisheye)]),
    QString::fromStdString(ProjectionTypeNames[static_cast<int>(
        WindowControl::ProjectionIndeces::SphericalMirror)]),
    QString::fromStdString(ProjectionTypeNames[static_cast<int>(
        WindowControl::ProjectionIndeces::Cylindrical)]),
    QString::fromStdString(ProjectionTypeNames[static_cast<int>(
        WindowControl::ProjectionIndeces::Equirectangular)])
};
const QList<QString> QualityTypes = {
    "Low (256)",
    "Medium (512)",
    "High (1K)",
    "1.5K (1536)",
    "2K (2048)",
    "4K (4096)",
    "8K (8192)",
    "16K (16384)",
    "32K (32768)",
    "64K (65536)"
};

WindowControl::WindowControl(unsigned int monitorIndex, unsigned int windowIndex,
                             std::vector<QRect>& monitorDims,
                             const QColor& winColor, QWidget *parent)
    : QWidget(parent)
    , _monIndex(monitorIndex)
    , _index(windowIndex)
    , _monitorResolutions(monitorDims)
    , _colorForWindow(winColor)
{
    _nMonitors = static_cast<unsigned int>(_monitorResolutions.size());
    createWidgets(parent);
}

WindowControl::~WindowControl() {
    delete _layoutFullWindow;
}

void WindowControl::createWidgets(QWidget* parent) {
    _windowDims = defaultWindowSizes[_index];
    _sizeX = new QLineEdit(QString::number(_windowDims.width()), parent);
    _sizeY = new QLineEdit(QString::number(_windowDims.height()), parent);
    _offsetX = new QLineEdit(QString::number(_windowDims.x()), parent);
    _offsetY = new QLineEdit(QString::number(_windowDims.y()), parent);
    {
        QIntValidator* validatorSizeX = new QIntValidator(10, _maxWindowSizePixels);
        QIntValidator* validatorSizeY = new QIntValidator(10, _maxWindowSizePixels);
        QIntValidator* validatorOffsetX = new QIntValidator(
            -_maxWindowSizePixels,
            _maxWindowSizePixels
        );
        QIntValidator* validatorOffsetY = new QIntValidator(
            -_maxWindowSizePixels,
            _maxWindowSizePixels
        );
        _sizeX->setValidator(validatorSizeX);
        _sizeY->setValidator(validatorSizeY);
        _offsetX->setValidator(validatorOffsetX);
        _offsetY->setValidator(validatorOffsetY);
    }
    if (_nMonitors > 1) {
        _comboMonitorSelect = new QComboBox(this);
        _comboMonitorSelect->addItems(_monitorNames);
        _comboMonitorSelect->setCurrentIndex(_monIndex);
    }
    _fullscreenButton = new QPushButton(this);
    _fullscreenButton->setText("Set to Fullscreen");
    _checkBoxWindowDecor = new QCheckBox("Window Decoration", this);
    _checkBoxWindowDecor->setCheckState(Qt::CheckState::Checked);
    _checkBoxWebGui = new QCheckBox("WebGUI only this window", this);
    _checkBoxSpoutOutput = new QCheckBox("Spout Output", this);
    _comboProjection = new QComboBox(this);
    _comboProjection->addItems(ProjectionTypes);

    _comboQuality = new QComboBox(this);
    _comboQuality->addItems(QualityTypes);

    {
        _lineFovH = new QLineEdit("80.0", parent);
        _lineFovV = new QLineEdit("50.534", parent);
        QDoubleValidator* validatorFovH = new QDoubleValidator(-180.0, 180.0, 10);
        _lineFovH->setValidator(validatorFovH);
        QDoubleValidator* validatorFovV = new QDoubleValidator(-90.0, 90.0, 10);
        _lineFovV->setValidator(validatorFovV);
        _lineHeightOffset = new QLineEdit("0.0", parent);
        QDoubleValidator* validatorHtOff= new QDoubleValidator(-1000000.0, 1000000.0, 12);
        _lineHeightOffset->setValidator(validatorHtOff);
    }

    connect(_sizeX, &QLineEdit::textChanged, this, &WindowControl::onSizeXChanged);
    connect(_sizeY, &QLineEdit::textChanged, this, &WindowControl::onSizeYChanged);
    connect(_offsetX, &QLineEdit::textChanged, this, &WindowControl::onOffsetXChanged);
    connect(_offsetY, &QLineEdit::textChanged, this, &WindowControl::onOffsetYChanged);
    connect(
        _comboMonitorSelect,
        qOverload<int>(&QComboBox::currentIndexChanged),
        this,
        &WindowControl::onMonitorChanged
    );
    connect(
        _comboProjection,
        qOverload<int>(&QComboBox::currentIndexChanged),
        this,
        &WindowControl::onProjectionChanged
    );
    connect(
        _checkBoxSpoutOutput,
        &QCheckBox::stateChanged,
        this,
        &WindowControl::onSpoutSelection
    );
    connect(
        _checkBoxWebGui,
        &QCheckBox::stateChanged,
        this,
        &WindowControl::onWebGuiSelection
    );
    connect(
        _fullscreenButton,
        &QPushButton::released,
        this,
        &WindowControl::onFullscreenClicked
    );
}

QVBoxLayout* WindowControl::initializeLayout() {
    _layoutFullWindow = new QVBoxLayout;
    //Window size
    QVBoxLayout* layoutWindowCtrl = new QVBoxLayout;

    _labelWinNum = new QLabel;
    _labelWinNum->setText("Window " + QString::number(_index + 1));
    QString colorStr =  QString::fromStdString(
        fmt::format("QLabel {{ color : #{:02x}{:02x}{:02x}; }}",
        _colorForWindow.red(), _colorForWindow.green(), _colorForWindow.blue()));
    _labelWinNum->setStyleSheet(colorStr);

    QHBoxLayout* layoutWinNum = new QHBoxLayout;
    layoutWinNum->addStretch(1);
    layoutWinNum->addWidget(_labelWinNum);
    layoutWinNum->addStretch(1);
    layoutWindowCtrl->addLayout(layoutWinNum);

    {
        QHBoxLayout* layoutName = new QHBoxLayout;
        QString tip("Enter a name for the window (displayed in title bar)");
        QLabel* labelName = new QLabel(this);
        labelName->setText("Name: ");
        labelName->setToolTip(tip);
        _windowName = new QLineEdit(this);
        _windowName->setFixedWidth(160);
        _windowName->setToolTip(tip);
        layoutName->addWidget(labelName);
        layoutName->addWidget(_windowName);
        layoutName->addStretch(1);
        layoutWindowCtrl->addLayout(layoutName);
    }

    if (_nMonitors > 1) {
        QHBoxLayout* layoutMonitorNum = new QHBoxLayout;
        QLabel* labelLocation = new QLabel(this);
        labelLocation->setText("Monitor: ");
        QString tip("Select monitor where this window is located");
        labelLocation->setToolTip(tip);
        _comboMonitorSelect->setToolTip(tip);
        layoutMonitorNum->addWidget(labelLocation);
        layoutMonitorNum->addWidget(_comboMonitorSelect);
        layoutMonitorNum->addStretch(1);
        layoutWindowCtrl->addLayout(layoutMonitorNum);
    }
    _sizeX->setFixedWidth(_lineEditWidthFixed);
    _sizeY->setFixedWidth(_lineEditWidthFixed);
    {
        QLabel* labelSize = new QLabel(this);
        QLabel* labelDelim = new QLabel(this);
        QLabel* labelUnit = new QLabel(this);
        QHBoxLayout* layoutSize = new QHBoxLayout;
        labelSize->setToolTip("Enter window width & height in pixels");
        _sizeX->setToolTip("Enter window width (pixels)");
        _sizeY->setToolTip("Enter window height (pixels)");
        layoutSize->addWidget(labelSize);
        labelSize->setText("Size:");
        labelSize->setFixedWidth(55);
        layoutSize->addWidget(_sizeX);
        layoutSize->addWidget(labelDelim);
        layoutSize->addWidget(_sizeY);
        layoutSize->addWidget(labelUnit);
        layoutSize->addStretch(1);
        labelDelim->setText("x");
        labelDelim->setFixedWidth(9);
        labelUnit->setText(" px");
        layoutWindowCtrl->addLayout(layoutSize);
    }

    _offsetX->setFixedWidth(_lineEditWidthFixed);
    _offsetY->setFixedWidth(_lineEditWidthFixed);
    {
        QLabel* labelOffset = new QLabel(this);
        QLabel* labelComma = new QLabel(this);
        QLabel* labelUnit = new QLabel(this);
        QHBoxLayout* layoutOffset = new QHBoxLayout;
        std::string baseTip = "Enter {} location of window's upper left "
            "corner from monitor's {} (pixels)";
        labelOffset->setToolTip(QString::fromStdString(fmt::format(
            baseTip, "x,y", "upper-left corner origin")));
        _offsetX->setToolTip(QString::fromStdString(fmt::format(
            baseTip, "x", "left side")));
        _offsetY->setToolTip(QString::fromStdString(fmt::format(
            baseTip, "y", "top edge")));
        layoutOffset->addWidget(labelOffset);
        labelOffset->setText("Offset:");
        labelOffset->setFixedWidth(55);
        layoutOffset->addWidget(_offsetX);
        layoutOffset->addWidget(labelComma);
        layoutOffset->addWidget(_offsetY);
        layoutOffset->addWidget(labelUnit);
        layoutOffset->addStretch(1);
        labelComma->setText(",");
        labelComma->setFixedWidth(9);
        labelUnit->setText(" px");
        layoutWindowCtrl->addLayout(layoutOffset);
    }
    {
        QHBoxLayout* layoutCheckboxesFull1 = new QHBoxLayout;
        QVBoxLayout* layoutCheckboxesFull2 = new QVBoxLayout;
        QHBoxLayout* layoutFullscreenButton = new QHBoxLayout;
        _fullscreenButton->setToolTip("If enabled, the window will be created in an "
            "exclusive fullscreen mode. The size of this\nwindow will be set to the "
            "screen resolution, and the window decoration automatically disabled.");
        layoutFullscreenButton->addWidget(_fullscreenButton);
        layoutFullscreenButton->addStretch(1);
        layoutCheckboxesFull2->addLayout(layoutFullscreenButton);
        QHBoxLayout* layoutCBoxWindowDecor = new QHBoxLayout;
        _checkBoxWindowDecor->setToolTip("If enabled, the window will not have a border "
            "frame or title bar, and no\n controls for minimizing/maximizing, "
            "resizing, or closing the window.");
        layoutCBoxWindowDecor->addWidget(_checkBoxWindowDecor);
        layoutCBoxWindowDecor->addStretch(1);
        layoutCheckboxesFull2->addLayout(layoutCBoxWindowDecor);
        QHBoxLayout* _layoutCBoxWebGui= new QHBoxLayout;
        _checkBoxWebGui->setToolTip("If enabled, the window will be dedicated solely to "
            "displaying the GUI controls, and will not\nrender any 3D content. All other "
            "window(s) will render in 3D but will not have GUI controls.");
        _layoutCBoxWebGui->addWidget(_checkBoxWebGui);
        _layoutCBoxWebGui->addStretch(1);
        layoutCheckboxesFull2->addLayout(_layoutCBoxWebGui);
        QVBoxLayout* layoutProjectionGroup = new QVBoxLayout;
        QHBoxLayout* layoutComboProjection = new QHBoxLayout;
        _comboProjection->setToolTip("Select from the supported window projection types");
        layoutComboProjection->addWidget(_comboProjection);
        layoutComboProjection->addStretch(1);
        layoutProjectionGroup->addLayout(layoutComboProjection);
        QFrame* borderProjectionGroup = new QFrame;
        borderProjectionGroup->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
        borderProjectionGroup->setLayout(layoutProjectionGroup);
        borderProjectionGroup->setVisible(true);
        QHBoxLayout* layoutCBoxSpoutOutput= new QHBoxLayout;
        QString spoutTip = "This projection method provides the ability to share the "
            "reprojected image using the Spout library.\nThis library only supports the "
            "Windows operating system. Spout makes it possible to make the rendered\n"
            "images available to other real-time applications on the same machine for "
            "further processing.\nThe SpoutOutputProjection option can work with either "
            "Fisheye or Equirectangular projection.";
        _checkBoxSpoutOutput->setToolTip(spoutTip);
        layoutCBoxSpoutOutput->addWidget(_checkBoxSpoutOutput);
        layoutCBoxSpoutOutput->addStretch(1);
        layoutProjectionGroup->addLayout(layoutCBoxSpoutOutput);
        QHBoxLayout* layoutComboQuality = new QHBoxLayout;
        _labelQuality = new QLabel;
        _labelQuality->setText("Quality:");
        QString qualityTip = "Determines the pixel resolution of the projection "
            "rendering. The higher resolution,\nthe better the rendering quality, but at "
            "the expense of increased rendering times.";
        _labelQuality->setToolTip(qualityTip);
        _comboQuality->setToolTip(qualityTip);
        layoutComboQuality->addWidget(_labelQuality);
        layoutComboQuality->addWidget(_comboQuality);
        layoutComboQuality->addStretch(1);
        layoutProjectionGroup->addLayout(layoutComboQuality);
        QHBoxLayout* layoutFovH = new QHBoxLayout;
        _labelFovH = new QLabel;
        _labelFovH->setText("Horizontal FOV:");
        QString hfovTip = "The total horizontal field of view of the viewport (degrees). "
            "Internally,\nthe values for 'left' & 'right' will each be half this value.";
        _labelFovH->setToolTip(hfovTip);
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
        _lineFovV->setToolTip(vfovTip);
        layoutFovV->addWidget(_labelFovV);
        layoutFovV->addStretch(1);
        layoutFovV->addWidget(_lineFovV);
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
        _lineHeightOffset->setToolTip(heightTip);
        layoutHeightOffset->addWidget(_labelHeightOffset);
        layoutHeightOffset->addWidget(_lineHeightOffset);
        layoutHeightOffset->addStretch(1);
        layoutProjectionGroup->addLayout(layoutHeightOffset);
        layoutCheckboxesFull2->addWidget(borderProjectionGroup);
        layoutCheckboxesFull1->addLayout(layoutCheckboxesFull2);
        layoutCheckboxesFull1->addStretch(1);
        layoutWindowCtrl->addLayout(layoutCheckboxesFull1);
    }
    layoutWindowCtrl->addStretch(1);
    _layoutFullWindow->addLayout(layoutWindowCtrl);

    _comboProjection->setCurrentIndex(0);
    onProjectionChanged(static_cast<unsigned int>(ProjectionIndeces::Planar));
    _comboQuality->setCurrentIndex(2);

    return _layoutFullWindow;
}

void WindowControl::showWindowLabel(bool show) {
    _labelWinNum->setVisible(show);
}

void WindowControl::onSizeXChanged(const QString& newText) {
    _windowDims.setWidth(newText.toInt());
    if (_windowChangeCallback) {
        _windowChangeCallback(_monIndex, _index, _windowDims);
    }
}

void WindowControl::onSizeYChanged(const QString& newText) {
    _windowDims.setHeight(newText.toInt());
    if (_windowChangeCallback) {
        _windowChangeCallback(_monIndex, _index, _windowDims);
    }
}

void WindowControl::onOffsetXChanged(const QString& newText) {
    float prevWidth = _windowDims.width();
    try {
        _windowDims.setX(newText.toInt());
        _windowDims.setWidth(prevWidth);
        if (_windowChangeCallback) {
            _windowChangeCallback(_monIndex, _index, _windowDims);
        }
    }
    catch (std::exception) {
        //The QIntValidator ensures that the range is a +/- integer
        //However, it's possible to enter only a - character which
        //causes an exception throw, which is ignored here (when user
        //enters an integer after the - then the value will be updated).
    }
}

void WindowControl::onOffsetYChanged(const QString& newText) {
    float prevHeight = _windowDims.height();
    try {
        _windowDims.setY(newText.toInt());
        _windowDims.setHeight(prevHeight);
        if (_windowChangeCallback) {
            _windowChangeCallback(_monIndex, _index, _windowDims);
        }
    }
    catch (std::exception) {
        //See comment in onOffsetXChanged
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
    if (_windowGuiCheckCallback && (selectionState == Qt::Checked)) {
        _windowGuiCheckCallback(_index);
    }
}

void WindowControl::onSpoutSelection(int selectionState) {
    if (selectionState == Qt::Checked) {
        WindowControl::ProjectionIndeces currentProjectionSelection;
        currentProjectionSelection = static_cast<WindowControl::ProjectionIndeces>(
            _comboProjection->currentIndex()
        );
        if ((currentProjectionSelection != ProjectionIndeces::Equirectangular) &&
            (currentProjectionSelection != ProjectionIndeces::Fisheye))
        {
            _comboProjection->setCurrentIndex(
                static_cast<int>(ProjectionIndeces::Equirectangular)
            );
        }
    }
}

void WindowControl::onMonitorChanged(int newSelection) {
    _monIndex = newSelection;
    if (_windowChangeCallback) {
        _windowChangeCallback(_monIndex, _index, _windowDims);
    }
}

void WindowControl::onProjectionChanged(int newSelection) {
    WindowControl::ProjectionIndeces selected
        = static_cast<WindowControl::ProjectionIndeces>(newSelection);
    _comboQuality->setVisible(selected != ProjectionIndeces::Planar);
    _labelQuality->setVisible(selected != ProjectionIndeces::Planar);
    _labelFovH->setVisible(selected == ProjectionIndeces::Planar);
    _lineFovH->setVisible(selected == ProjectionIndeces::Planar);
    _labelFovV->setVisible(selected == ProjectionIndeces::Planar);
    _lineFovV->setVisible(selected == ProjectionIndeces::Planar);
    _labelHeightOffset->setVisible(selected == ProjectionIndeces::Cylindrical);
    _lineHeightOffset->setVisible(selected == ProjectionIndeces::Cylindrical);
    _checkBoxSpoutOutput->setVisible(selected == ProjectionIndeces::Fisheye
        || selected == ProjectionIndeces::Equirectangular);
}

void WindowControl::setWindowChangeCallback(
                                          std::function<void(int, int, const QRectF&)> cb)
{
    _windowChangeCallback = std::move(cb);
}

void WindowControl::setWebGuiChangeCallback(std::function<void(unsigned int)> cb)
{
    _windowGuiCheckCallback = std::move(cb);
}

void WindowControl::uncheckWebGuiOption() {
    _checkBoxWebGui->setCheckState(Qt::Unchecked);
}

QRectF& WindowControl::dimensions() {
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

WindowControl::ProjectionIndeces WindowControl::projectionSelectedIndex() const {
    return
          static_cast<WindowControl::ProjectionIndeces>(_comboProjection->currentIndex());
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

