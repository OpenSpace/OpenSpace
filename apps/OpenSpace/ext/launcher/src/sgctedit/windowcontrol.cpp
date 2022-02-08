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

#include "sgctedit/display.h"
#include "sgctedit/monitorbox.h"
#include "sgctedit/windowcontrol.h"

WindowControl::WindowControl(unsigned int monitorIndex, const unsigned int windowIndex,
                             std::vector<QRect>& monitorDims,
                             const std::array<QString, 4> winColors, QWidget *parent)
    : QWidget(parent)
    , _monIndex(monitorIndex)
    , _index(windowIndex)
    , _monitorResolutions(monitorDims)
    , _colorsForWindows(winColors)
{
    _nMonitors = _monitorResolutions.size();
    createWidgets(parent);
}

void WindowControl::createWidgets(QWidget* parent) {
    _windowDims = defaultWindowSizes[_index];
    _size_x = new QLineEdit(
        QString::fromUtf8(std::to_string(int(_windowDims.width())).c_str()), parent);
    _size_y = new QLineEdit(
        QString::fromUtf8(std::to_string(int(_windowDims.height())).c_str()), parent);
    _offset_x = new QLineEdit(
        QString::fromUtf8(std::to_string(int(_windowDims.x())).c_str()), parent);
    _offset_y = new QLineEdit(
        QString::fromUtf8(std::to_string(int(_windowDims.y())).c_str()), parent);
    {
        QIntValidator* validatorSize_x = new QIntValidator(10, _maxWindowSizePixels);
        QIntValidator* validatorSize_y = new QIntValidator(10, _maxWindowSizePixels);
        QIntValidator* validatorOffset_x = new QIntValidator(
            -_maxWindowSizePixels,
            _maxWindowSizePixels
        );
        QIntValidator* validatorOffset_y = new QIntValidator(
            -_maxWindowSizePixels,
            _maxWindowSizePixels
        );
        _size_x->setValidator(validatorSize_x);
        _size_y->setValidator(validatorSize_y);
        _offset_x->setValidator(validatorOffset_x);
        _offset_y->setValidator(validatorOffset_y);
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
    _comboProjection->addItems(_projectionTypes);

    _comboQuality = new QComboBox(this);
    _comboQuality->addItems(_qualityTypes);

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

    connect(
        _size_x,
        SIGNAL(textChanged(const QString&)),
        this,
        SLOT(onSizeXChanged(const QString&))
    );
    connect(
        _size_y,
        SIGNAL(textChanged(const QString&)),
        this,
        SLOT(onSizeYChanged(const QString&))
    );
    connect(
        _offset_x,
        SIGNAL(textChanged(const QString&)),
        this,
        SLOT(onOffsetXChanged(const QString&))
    );
    connect(
        _offset_y,
        SIGNAL(textChanged(const QString&)),
        this,
        SLOT(onOffsetYChanged(const QString&))
    );
    connect(
        _comboMonitorSelect,
        SIGNAL(currentIndexChanged(int)),
        this,
        SLOT(onMonitorChanged(int))
    );
    connect(_comboProjection,
        SIGNAL(currentIndexChanged(int)),
        this,
        SLOT(onProjectionChanged(int))
    );
    connect(_checkBoxSpoutOutput,
        SIGNAL(stateChanged(int)),
        this,
        SLOT(onSpoutSelection(int))
    );
    connect(
        _checkBoxWebGui,
        SIGNAL(stateChanged(int)),
        this,
        SLOT(onWebGuiSelection(int))
    );
    connect(_fullscreenButton, SIGNAL(released()), this, SLOT(onFullscreenClicked()));
}

QVBoxLayout* WindowControl::initializeLayout() {
    _layoutFullWindow = new QVBoxLayout();
    //Window size
    QVBoxLayout* layoutWindowCtrl = new QVBoxLayout();

    _labelWinNum = new QLabel();
    _labelWinNum->setText("Window " + QString::number(_index + 1));
    QString colorStr = "QLabel { color : " + _colorsForWindows[_index] + "; }";
    _labelWinNum->setStyleSheet(colorStr);

    QHBoxLayout* layoutWinNum = new QHBoxLayout();
    layoutWinNum->addStretch(1);
    layoutWinNum->addWidget(_labelWinNum);
    layoutWinNum->addStretch(1);
    layoutWindowCtrl->addLayout(layoutWinNum);

    {
        QHBoxLayout* layoutName = new QHBoxLayout();
        QLabel* labelName = new QLabel(this);
        labelName->setText("Name: ");
        _windowName = new QLineEdit(this);
        _windowName->setFixedWidth(160);
        layoutName->addWidget(labelName);
        layoutName->addWidget(_windowName);
        layoutName->addStretch(1);
        layoutWindowCtrl->addLayout(layoutName);
    }

    if (_nMonitors > 1) {
        QHBoxLayout* layoutMonitorNum = new QHBoxLayout();
        layoutMonitorNum->addWidget(_comboMonitorSelect);
        layoutMonitorNum->addStretch(1);
        layoutWindowCtrl->addLayout(layoutMonitorNum);
    }
    _size_x->setFixedWidth(_lineEditWidthFixed);
    _size_y->setFixedWidth(_lineEditWidthFixed);
    {
        QLabel* labelSize = new QLabel(this);
        QLabel* labelDelim = new QLabel(this);
        QHBoxLayout* layoutSize = new QHBoxLayout();
        layoutSize->addWidget(labelSize);
        labelSize->setText("Size:");
        labelSize->setFixedWidth(55);
        layoutSize->addWidget(_size_x);
        layoutSize->addWidget(labelDelim);
        layoutSize->addWidget(_size_y);
        layoutSize->addStretch(1);
        labelDelim->setText("x");
        labelDelim->setFixedWidth(9);
        layoutWindowCtrl->addLayout(layoutSize);
    }

    _offset_x->setFixedWidth(_lineEditWidthFixed);
    _offset_y->setFixedWidth(_lineEditWidthFixed);
    {
        QLabel* labelOffset = new QLabel(this);
        QLabel* labelComma = new QLabel(this);
        QHBoxLayout* layoutOffset = new QHBoxLayout();
        layoutOffset->addWidget(labelOffset);
        labelOffset->setText("Offset:");
        labelOffset->setFixedWidth(55);
        layoutOffset->addWidget(_offset_x);
        layoutOffset->addWidget(labelComma);
        layoutOffset->addWidget(_offset_y);
        layoutOffset->addStretch(1);
        labelComma->setText(",");
        labelComma->setFixedWidth(9);
        layoutWindowCtrl->addLayout(layoutOffset);
    }
    {
        QHBoxLayout* layoutCheckboxesFull1 = new QHBoxLayout();
        QVBoxLayout* layoutCheckboxesFull2 = new QVBoxLayout();
        QHBoxLayout* layoutFullscreenButton = new QHBoxLayout();
        layoutFullscreenButton->addWidget(_fullscreenButton);
        layoutFullscreenButton->addStretch(1);
        layoutCheckboxesFull2->addLayout(layoutFullscreenButton);
        QHBoxLayout* layoutCBoxWindowDecor = new QHBoxLayout();
        layoutCBoxWindowDecor->addWidget(_checkBoxWindowDecor);
        layoutCBoxWindowDecor->addStretch(1);
        layoutCheckboxesFull2->addLayout(layoutCBoxWindowDecor);
        QHBoxLayout* _layoutCBoxWebGui= new QHBoxLayout();
        _layoutCBoxWebGui->addWidget(_checkBoxWebGui);
        _layoutCBoxWebGui->addStretch(1);
        layoutCheckboxesFull2->addLayout(_layoutCBoxWebGui);
        QVBoxLayout* layoutProjectionGroup = new QVBoxLayout();
        QHBoxLayout* layoutComboProjection = new QHBoxLayout();
        layoutComboProjection->addWidget(_comboProjection);
        layoutComboProjection->addStretch(1);
        layoutProjectionGroup->addLayout(layoutComboProjection);
        QFrame* borderProjectionGroup = new QFrame;
        borderProjectionGroup->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
        borderProjectionGroup->setLayout(layoutProjectionGroup);
        borderProjectionGroup->setVisible(true);
        QHBoxLayout* layoutCBoxSpoutOutput= new QHBoxLayout();
        layoutCBoxSpoutOutput->addWidget(_checkBoxSpoutOutput);
        layoutCBoxSpoutOutput->addStretch(1);
        layoutProjectionGroup->addLayout(layoutCBoxSpoutOutput);
        QHBoxLayout* layoutComboQuality = new QHBoxLayout();
        _labelQuality = new QLabel();
        _labelQuality->setText("Quality:");
        layoutComboQuality->addWidget(_labelQuality);
        layoutComboQuality->addWidget(_comboQuality);
        layoutComboQuality->addStretch(1);
        layoutProjectionGroup->addLayout(layoutComboQuality);
        QHBoxLayout* layoutFovH = new QHBoxLayout();
        _labelFovH = new QLabel();
        _labelFovH->setText("Horizontal FOV:");
        layoutFovH->addWidget(_labelFovH);
        layoutFovH->addWidget(_lineFovH);
        layoutFovH->addStretch(1);
        QHBoxLayout* layoutFovV = new QHBoxLayout();
        _labelFovV = new QLabel();
        _labelFovV->setText("Vertical FOV:");
        layoutFovV->addWidget(_labelFovV);
        layoutFovV->addWidget(_lineFovV);
        layoutFovV->addStretch(1);
        layoutProjectionGroup->addLayout(layoutFovH);
        layoutProjectionGroup->addLayout(layoutFovV);
        QHBoxLayout* layoutHeightOffset = new QHBoxLayout();
        _labelHeightOffset = new QLabel();
        _labelHeightOffset->setText("Height Offset:");
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
    onProjectionChanged(0);
    _comboQuality->setCurrentIndex(2);

    return _layoutFullWindow;
}

void WindowControl::showWindowLabel(const bool show) {
    _labelWinNum->setVisible(show);
}

void WindowControl::onSizeXChanged(const QString& newText) {
    std::string x = newText.toStdString();
    if (!x.empty()) {
        _windowDims.setWidth(std::stoi(x));
    }
    if (_windowChangeCallback) {
        _windowChangeCallback(_monIndex, _index, _windowDims);
    }
}

void WindowControl::onSizeYChanged(const QString& newText) {
    std::string y = newText.toStdString();
    if (!y.empty()) {
        _windowDims.setHeight(std::stoi(y));
    }
    if (_windowChangeCallback) {
        _windowChangeCallback(_monIndex, _index, _windowDims);
    }
}

void WindowControl::onOffsetXChanged(const QString& newText) {
    std::string xOffset = newText.toStdString();
    float prevWidth = _windowDims.width();
    try {
        if (!xOffset.empty()) {
            _windowDims.setX(std::stoi(xOffset));
            _windowDims.setWidth(prevWidth);
        }
        if (_windowChangeCallback) {
            _windowChangeCallback(_monIndex, _index, _windowDims);
        }
    }
    catch (...) {
        //The QIntValidator ensures that the range is a +/- integer
        //However, it's possible to enter only a - character which
        //causes an exception throw, which is ignored here (when user
        //enters an integer after the - then the value will be updated).
    }
}

void WindowControl::onOffsetYChanged(const QString& newText) {
    std::string yOffset = newText.toStdString();
    float prevHeight = _windowDims.height();
    try {
        if (!yOffset.empty()) {
            _windowDims.setY(std::stoi(yOffset));
            _windowDims.setHeight(prevHeight);
        }
        if (_windowChangeCallback) {
            _windowChangeCallback(_monIndex, _index, _windowDims);
        }
    }
    catch (...) {
        //See comment in onOffsetXChanged
    }
}

void WindowControl::onFullscreenClicked() {
    _offset_x->setText("0");
    _offset_y->setText("0");
    _size_x->setText(QString::number(_monitorResolutions[_monIndex].width()));
    _size_y->setText(QString::number(_monitorResolutions[_monIndex].height()));
    _checkBoxWindowDecor->setCheckState(Qt::Unchecked);
}

void WindowControl::enableGuiWindowSelection(bool enabled) {
    _checkBoxWebGui->setEnabled(enabled);
}

void WindowControl::onWebGuiSelection(int selectionState) {
    if (_windowGuiCheckCallback && (selectionState == Qt::Checked)) {
        _windowGuiCheckCallback(_index);
    }
}

void WindowControl::onSpoutSelection(int selectionState) {
    if (selectionState == Qt::Checked) {
        int currentProjectionSelection = _comboProjection->currentIndex();
        if ((currentProjectionSelection != ProjectionIndeces::Equirectangular) &&
            (currentProjectionSelection != ProjectionIndeces::Fisheye))
        {
            _comboProjection->setCurrentIndex(ProjectionIndeces::Equirectangular);
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
    _comboQuality->setVisible(newSelection != ProjectionIndeces::Planar);
    _labelQuality->setVisible(newSelection != ProjectionIndeces::Planar);
    _labelFovH->setVisible(newSelection == ProjectionIndeces::Planar);
    _lineFovH->setVisible(newSelection == ProjectionIndeces::Planar);
    _labelFovV->setVisible(newSelection == ProjectionIndeces::Planar);
    _lineFovV->setVisible(newSelection == ProjectionIndeces::Planar);
    _labelHeightOffset->setVisible(newSelection == ProjectionIndeces::Cylindrical);
    _lineHeightOffset->setVisible(newSelection == ProjectionIndeces::Cylindrical);
    _checkBoxSpoutOutput->setVisible(newSelection == ProjectionIndeces::Fisheye
        || newSelection == ProjectionIndeces::Equirectangular);
}

void WindowControl::setDimensions(const QRectF& dimensions) {
    _windowDims = dimensions;
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

QLineEdit* WindowControl::lineEditSizeWidth() {
    return _size_x;
}

QLineEdit* WindowControl::lineEditSizeHeight() {
    return _size_y;
}

QLineEdit* WindowControl::lineEditSizeOffsetX() {
    return _offset_x;
}

QLineEdit* WindowControl::lineEditSizeOffsetY() {
    return _offset_y;
}

QCheckBox* WindowControl::checkBoxWindowDecor() {
    return _checkBoxWindowDecor;
}

QCheckBox* WindowControl::checkBoxWebGui() {
    return _checkBoxWebGui;
}

QCheckBox* WindowControl::checkBoxSpoutOutput() {
    return _checkBoxSpoutOutput;
}

std::string WindowControl::windowName() {
    return _windowName->text().toStdString();
}

sgct::ivec2 WindowControl::windowSize() {
    return {
        stoi(_size_x->text().toStdString()),
        stoi(_size_y->text().toStdString())
    };
}

sgct::ivec2 WindowControl::windowPos() {
    return {
        stoi(_offset_x->text().toStdString()),
        stoi(_offset_y->text().toStdString())
    };
}

bool WindowControl::isDecorated() {
    return (_checkBoxWindowDecor->checkState() == Qt::Checked);
}

bool WindowControl::isGuiWindow() {
    return (_checkBoxWebGui->checkState() == Qt::Checked);
}

bool WindowControl::isSpoutSelected() {
    return (_checkBoxSpoutOutput->checkState() == Qt::Checked);
}

int WindowControl::projectionSelectedIndex() {
    return _comboProjection->currentIndex();
}

int WindowControl::qualitySelectedIndex() {
    return _comboQuality->currentIndex();
}

int WindowControl::qualitySelectedValue() {
    return QualityValues[_comboQuality->currentIndex()];
}

float WindowControl::fovH() {
    return _lineFovH->text().toFloat();
}

float WindowControl::fovV() {
    return _lineFovV->text().toFloat();
}

float WindowControl::heightOffset() {
    return _lineHeightOffset->text().toFloat();
}

unsigned int WindowControl::monitorNum() {
    return _monIndex;
}

WindowControl::~WindowControl()
{
    delete _layoutFullWindow;
}

