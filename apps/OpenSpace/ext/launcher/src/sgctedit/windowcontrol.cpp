#include "display.h"
#include "monitorbox.h"
#include "windowcontrol.h"

WindowControl::WindowControl(unsigned int monitorIndex, unsigned int windowIndex,
                                                       QRect& widgetDims, QWidget *parent)
    : _monIndex(monitorIndex)
    , _index(windowIndex)
    , QWidget(parent)
{
    _windowDims = defaultWindowSizes[windowIndex];
    _size_x = new QLineEdit(
        QString::fromUtf8(std::to_string(int(_windowDims.width())).c_str()), parent);
    _size_y = new QLineEdit(
        QString::fromUtf8(std::to_string(int(_windowDims.height())).c_str()), parent);
    _offset_x = new QLineEdit(
        QString::fromUtf8(std::to_string(int(_windowDims.x())).c_str()), parent);
    _offset_y = new QLineEdit(
        QString::fromUtf8(std::to_string(int(_windowDims.y())).c_str()), parent);
    QIntValidator* _validatorSize_x
        = new QIntValidator(10, _maxWindowSizePixels);
    QIntValidator* _validatorSize_y
        = new QIntValidator(10, _maxWindowSizePixels);
    QIntValidator* _validatorOffset_x
        = new QIntValidator(10, _maxWindowSizePixels);
    QIntValidator* _validatorOffset_y
        = new QIntValidator(10, _maxWindowSizePixels);
    _size_x->setValidator(_validatorSize_x);
    _size_y->setValidator(_validatorSize_y);
    _offset_x->setValidator(_validatorSize_y);
    _offset_y->setValidator(_validatorSize_y);

    _checkBoxWindowDecor = new QCheckBox("Window Decoration", this);
    _checkBoxVsync = new QCheckBox("VSync", this);
    _checkBoxWebGui = new QCheckBox("WebGUI here", this);
    _checkBoxSpoutOutput = new QCheckBox("Spout Output", this);
    _comboProjection = new QComboBox(this);
    _comboProjection->addItems(_projectionTypes);

    _comboQuality = new QComboBox(this);
    _comboQuality->addItems(_qualityTypes);

    _lineFov = new QLineEdit("0.0", parent);
    _validatorFov = new QDoubleValidator(-180.0, 180.0, 10);
    _lineFov->setValidator(_validatorFov);
    _lineHeightOffset = new QLineEdit("0.0", parent);
    _validatorHeightOffset = new QDoubleValidator(-1000000.0, 1000000.0, 12);
    _lineHeightOffset->setValidator(_validatorHeightOffset);

    connect(_size_x, SIGNAL(textChanged(const QString&)), this,
            SLOT(onSizeXChanged(const QString&)));
    connect(_size_y, SIGNAL(textChanged(const QString&)), this,
            SLOT(onSizeYChanged(const QString&)));
    connect(_offset_x, SIGNAL(textChanged(const QString&)), this,
            SLOT(onOffsetXChanged(const QString&)));
    connect(_offset_y, SIGNAL(textChanged(const QString&)), this,
            SLOT(onOffsetYChanged(const QString&)));

    connect(_comboProjection, SIGNAL(currentIndexChanged(int)),
            this, SLOT(onProjectionChanged(int)));
    connect(_comboQuality, SIGNAL(currentIndexChanged(int)),
            this, SLOT(onQualityChanged(int)));
}

QVBoxLayout* WindowControl::initializeLayout(QWidget* parentWidget) {
    _layoutFullWindow = new QVBoxLayout();
    //Window size
    _layoutWindowCtrl = new QVBoxLayout();

    _labelWinNum = new QLabel();
    _labelWinNum->setText("Window " + QString::number(_index + 1));
    _layoutWinNum = new QHBoxLayout();
    _layoutWinNum->addStretch(1);
    _layoutWinNum->addWidget(_labelWinNum);
    _layoutWinNum->addStretch(1);
    _layoutWindowCtrl->addLayout(_layoutWinNum);

    _layoutName = new QHBoxLayout();
    _labelName = new QLabel(this);
    _labelName->setText("Window Name: ");
    _windowName = new QLineEdit(this);
    _windowName->setFixedWidth(100);
//    _layoutName->addStretch(1);
    _layoutName->addWidget(_labelName);
    _layoutName->addWidget(_windowName);
    _layoutName->addStretch(1);
    _layoutWindowCtrl->addLayout(_layoutName);
    _size_x->setFixedWidth(_lineEditWidthFixed);
    _size_y->setFixedWidth(_lineEditWidthFixed);
    _labelSize = new QLabel(this);
    _labelDelim = new QLabel(this);
    QGridLayout* _layoutSize = new QGridLayout;
    //_layoutSize = new QHBoxLayout();
    //_layoutSize->addStretch(1);
//    _layoutSize->addRow(_labelSize, _size_x, _labelDelim, _size_y);
    _layoutSize->addWidget(_labelSize, 0, 0);
    _layoutGridSizeValues = new QHBoxLayout();
    _layoutGridSizeValues->addWidget(_size_x);
    _layoutGridSizeValues->addWidget(_labelDelim);
    _layoutGridSizeValues->addWidget(_size_y);
    _layoutGridSizeValues->addStretch(1);
    _layoutSize->addLayout(_layoutGridSizeValues, 0, 1);
//    _layoutSize->addWidget(_labelSize);
    //_labelSize->setFixedWidth(50);
    _labelSize->setText("Size:");
//    _layoutSize->addWidget(_size_x);
//    _layoutSize->addWidget(_labelDelim);
//    _layoutSize->addWidget(_size_y);
//    _layoutSize->addStretch(1);
    _labelDelim->setText("x");
    _labelDelim->setFixedWidth(10);
    _layoutWindowCtrl->addLayout(_layoutSize);

    //Window offset
    _offset_x->setFixedWidth(_lineEditWidthFixed);
    _offset_y->setFixedWidth(_lineEditWidthFixed);
    _labelOffset = new QLabel(this);
    _labelComma = new QLabel(this);
//    _layoutOffset = new QHBoxLayout();
//    _layoutOffset->addStretch(1);
//    _layoutOffset->addWidget(_labelOffset);
//    _layoutSize->addRow(_labelOffset, _offset_x, _labelComma, _offset_y);
    _layoutSize->addWidget(_labelOffset, 1, 0);
    _layoutGridOffsetValues = new QHBoxLayout();
    _layoutGridOffsetValues->addWidget(_offset_x);
    _layoutGridOffsetValues->addWidget(_labelComma);
    _layoutGridOffsetValues->addWidget(_offset_y);
    _layoutGridOffsetValues->addStretch(1);
    _layoutSize->addLayout(_layoutGridOffsetValues, 1, 1);
    //_labelOffset->setFixedWidth(50);
    _labelOffset->setText("Offset:");
//    _layoutOffset->addWidget(_offset_x);
//    _layoutOffset->addWidget(_labelComma);
//    _layoutOffset->addWidget(_offset_y);
//    _layoutOffset->addStretch(1);
    _labelComma->setText(",");
    _labelComma->setFixedWidth(10);
    _layoutGridFrame = new QHBoxLayout();
    _layoutGridFrame->addStretch(1);
    _layoutGridFrame->addLayout(_layoutSize);
    _layoutGridFrame->addStretch(1);

    _layoutWindowCtrl->addLayout(_layoutGridFrame);

    //Window options
    _layoutCheckboxesFull1 = new QHBoxLayout();
    _layoutCheckboxesFull2 = new QVBoxLayout();
//    _layoutCheckboxesFull1->addStretch(1);
    _layoutCBoxWindowDecor = new QHBoxLayout();
    //_layoutCBoxWindowDecor->addStretch(1);
    _layoutCBoxWindowDecor->addWidget(_checkBoxWindowDecor);
    _layoutCBoxWindowDecor->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutCBoxWindowDecor);
    _layoutCBoxVsync= new QHBoxLayout();
    //_layoutCBoxVsync->addStretch(1);
    _layoutCBoxVsync->addWidget(_checkBoxVsync);
    _layoutCBoxVsync->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutCBoxVsync);
    _layoutCBoxWebGui= new QHBoxLayout();
    //_layoutCBoxWebGui->addStretch(1);
    _layoutCBoxWebGui->addWidget(_checkBoxWebGui);
    _layoutCBoxWebGui->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutCBoxWebGui);
    _layoutCBoxSpoutOutput= new QHBoxLayout();
    //_layoutCBoxSpoutOutput->addStretch(1);
    _layoutCBoxSpoutOutput->addWidget(_checkBoxSpoutOutput);
    _layoutCBoxSpoutOutput->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutCBoxSpoutOutput);

    _layoutComboProjection = new QHBoxLayout();
    _layoutComboProjection->addWidget(_comboProjection);
    _layoutComboProjection->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutComboProjection);

    _layoutComboQuality = new QHBoxLayout();
    _layoutComboQuality->addWidget(_comboQuality);
    _layoutComboQuality->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutComboQuality);

    _layoutFov = new QHBoxLayout();
    _labelFov = new QLabel();
    _labelFov->setText("Horizontal FOV:");
    _layoutFov->addWidget(_labelFov);
    _layoutFov->addWidget(_lineFov);
    _layoutFov->addStretch(1);
    _layoutFovWrapper = new QWidget();
    _layoutFovWrapper->setLayout(_layoutFov);
    _layoutCheckboxesFull2->addWidget(_layoutFovWrapper);
    _layoutHeightOffset = new QHBoxLayout();
    _labelHeightOffset = new QLabel();
    _labelHeightOffset->setText("Height Offset:");
    _layoutHeightOffset->addWidget(_labelHeightOffset);
    _layoutHeightOffset->addWidget(_lineHeightOffset);
    _layoutHeightOffset->addStretch(1);
    _layoutHeightOffsetWrapper = new QWidget();
    _layoutHeightOffsetWrapper->setLayout(_layoutHeightOffset);
    _layoutCheckboxesFull2->addWidget(_layoutHeightOffsetWrapper);

    _layoutCheckboxesFull1->addLayout(_layoutCheckboxesFull2);
    _layoutCheckboxesFull1->addStretch(1);
    _layoutWindowCtrl->addLayout(_layoutCheckboxesFull1);
    _layoutFullWindow->addLayout(_layoutWindowCtrl);

    _comboProjection->setCurrentIndex(0);
    onProjectionChanged(0);
    _comboQuality->setCurrentIndex(2);
    onQualityChanged(2);

    return _layoutFullWindow;
}

void WindowControl::showWindowLabel(bool show) {
    _labelWinNum->setVisible(show);
}

void WindowControl::cleanupLayouts() {
    int labelSize1 = _labelSize->width();
    int labelSize2 = _labelOffset->width();
    int labelWidthStandard = std::max(labelSize1, labelSize2);
    _labelSize->setFixedWidth(labelWidthStandard);
    _labelOffset->setFixedWidth(labelWidthStandard);
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
    if (!xOffset.empty()) {
        _windowDims.setX(std::stoi(xOffset));
        _windowDims.setWidth(prevWidth);
    }
    if (_windowChangeCallback) {
        _windowChangeCallback(_monIndex, _index, _windowDims);
    }
}

void WindowControl::onOffsetYChanged(const QString& newText) {
    std::string yOffset = newText.toStdString();
    float prevHeight = _windowDims.height();
    if (!yOffset.empty()) {
        _windowDims.setY(std::stoi(yOffset));
        _windowDims.setHeight(prevHeight);
    }
    if (_windowChangeCallback) {
        _windowChangeCallback(_monIndex, _index, _windowDims);
    }
}

void WindowControl::onProjectionChanged(int newSelection) {
    switch (newSelection) {
    case 0:
        _labelFov->setEnabled(true);
        _lineFov->setEnabled(true);
        _labelHeightOffset->setEnabled(false);
        _lineHeightOffset->setEnabled(false);
        break;

    case 1:
        _labelFov->setEnabled(false);
        _lineFov->setEnabled(false);
        _labelHeightOffset->setEnabled(false);
        _lineHeightOffset->setEnabled(false);
        break;

    case 2:
        _labelFov->setEnabled(false);
        _lineFov->setEnabled(false);
        _labelHeightOffset->setEnabled(false);
        _lineHeightOffset->setEnabled(false);
        break;

    case 3:
        _labelFov->setEnabled(false);
        _lineFov->setEnabled(false);
        _labelHeightOffset->setEnabled(true);
        _lineHeightOffset->setEnabled(true);
        break;

    case 4:
        _labelFov->setEnabled(false);
        _lineFov->setEnabled(false);
        _labelHeightOffset->setEnabled(false);
        _lineHeightOffset->setEnabled(false);
        break;
    }
}

void WindowControl::onQualityChanged(int newSelection) {
}

void WindowControl::setDimensions(const QRectF& dimensions) {
    _windowDims = dimensions;
}

void WindowControl::setWindowChangeCallback(
                        std::function<void(unsigned int, unsigned int, const QRectF&)> cb)
{
    _windowChangeCallback = cb;
}

void WindowControl::setWindowScaleFactor(float scaleFactor) {
    _monitorScaleFactor = scaleFactor;
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

QCheckBox* WindowControl::checkBoxVsync() {
    return _checkBoxVsync;
}

QCheckBox* WindowControl::checkBoxWebGui() {
    return _checkBoxWebGui;
}

QCheckBox* WindowControl::checkBoxSpoutOutput() {
    return _checkBoxSpoutOutput;
}

WindowControl::~WindowControl()
{
    delete _size_x;
    delete _size_y;
    delete _validatorSize_x;
    delete _validatorSize_y;
    delete _offset_x;
    delete _offset_y;
    delete _validatorOffset_x;
    delete _validatorOffset_y;
    delete _layoutName;
    delete _labelName;
    delete _windowName;
    delete _labelSize;
    delete _labelDelim;
    delete _layoutSize;
    delete _labelOffset;
    delete _labelComma;
//    delete _layoutOffset;
    delete _layoutGridFrame;
    delete _layoutGridSizeValues;
    delete _layoutGridOffsetValues;
    delete _checkBoxWindowDecor;
    delete _checkBoxVsync;
    delete _checkBoxWebGui;
    delete _checkBoxSpoutOutput;
    delete _comboProjection;
    delete _comboQuality;
    delete _labelFov;
    delete _lineFov;
    delete _validatorFov;
    delete _labelHeightOffset;
    delete _lineHeightOffset;
    delete _validatorHeightOffset;
    delete _layoutCBoxWindowDecor;
    delete _layoutCBoxVsync;
    delete _layoutCBoxWebGui;
    delete _layoutCBoxSpoutOutput;
    delete _layoutComboProjection;
    delete _layoutComboQuality;
    delete _layoutFov;
    delete _layoutFovWrapper;
    delete _layoutHeightOffset;
    delete _layoutHeightOffsetWrapper;
    delete _layoutCheckboxesFull2;
    delete _layoutCheckboxesFull1;
    delete _layoutWindowCtrl;
}

