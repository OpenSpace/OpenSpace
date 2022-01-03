#include "display.h"
#include "monitorbox.h"
#include "windowcontrol.h"

WindowControl::WindowControl(unsigned int windowIndex, QRect& widgetDims,
                                                QRect& monitorResolution, QWidget *parent)
    : _index(windowIndex)
    , _monitorResolution(monitorResolution)
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
        = new QIntValidator(10, _monitorResolution.width());
    QIntValidator* _validatorSize_y
        = new QIntValidator(10, _monitorResolution.height());
    QIntValidator* _validatorOffset_x
        = new QIntValidator(10, _monitorResolution.width() - 10);
    QIntValidator* _validatorOffset_y
        = new QIntValidator(10, _monitorResolution.height() - 10);
    _size_x->setValidator(_validatorSize_x);
    _size_y->setValidator(_validatorSize_y);
    _offset_x->setValidator(_validatorSize_y);
    _offset_y->setValidator(_validatorSize_y);

    _checkBoxFullscreen = new QCheckBox("Fullscreen", this);
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

QVBoxLayout* WindowControl::initializeLayout(QWidget* parentWidget/*, QHBoxLayout* layout*/) {
    _layoutFullWindow = new QVBoxLayout();
    //Window size
    _layoutWindowCtrl = new QVBoxLayout();
    _layoutName = new QHBoxLayout();
    _labelName = new QLabel(this);
    _labelName->setText("Window Name: ");
    _windowName = new QLineEdit(this);
    _windowName->setFixedWidth(100);
    _layoutName->addStretch(1);
    _layoutName->addWidget(_labelName);
    _layoutName->addWidget(_windowName);
    _layoutName->addStretch(1);
    _layoutWindowCtrl->addLayout(_layoutName);
    _size_x->setFixedWidth(_lineEditWidthFixed);
    _size_y->setFixedWidth(_lineEditWidthFixed);
    _labelSize = new QLabel(this);
    _labelDelim = new QLabel(this);
    _layoutSize = new QHBoxLayout();
    _layoutSize->addStretch(1);
    _layoutSize->addWidget(_labelSize);
    //_labelSize->setFixedWidth(50);
    _labelSize->setText("Size:");
    _layoutSize->addWidget(_size_x);
    _layoutSize->addWidget(_labelDelim);
    _layoutSize->addWidget(_size_y);
    _layoutSize->addStretch(1);
    _labelDelim->setText("x");
    _layoutWindowCtrl->addLayout(_layoutSize);

    //Window offset
    _offset_x->setFixedWidth(_lineEditWidthFixed);
    _offset_y->setFixedWidth(_lineEditWidthFixed);
    _labelOffset = new QLabel(this);
    _labelComma = new QLabel(this);
    _layoutOffset = new QHBoxLayout();
    _layoutOffset->addStretch(1);
    _layoutOffset->addWidget(_labelOffset);
    //_labelOffset->setFixedWidth(50);
    _labelOffset->setText("Offset:");
    _layoutOffset->addWidget(_offset_x);
    _layoutOffset->addWidget(_labelComma);
    _layoutOffset->addWidget(_offset_y);
    _layoutOffset->addStretch(1);
    _labelComma->setText(",");
    _layoutWindowCtrl->addLayout(_layoutOffset);

    //Window options
    _layoutCheckboxesFull1 = new QHBoxLayout();
    _layoutCheckboxesFull2 = new QVBoxLayout();
    _layoutCheckboxesFull1->addStretch(1);
    _layoutCBoxFullscreen = new QHBoxLayout();
    //_layoutCBoxFullscreen->addStretch(1);
    _layoutCBoxFullscreen->addWidget(_checkBoxFullscreen);
    _layoutCBoxFullscreen->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutCBoxFullscreen);
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
        _windowChangeCallback(_index, _windowDims);
    }
}

void WindowControl::onSizeYChanged(const QString& newText) {
    std::string y = newText.toStdString();
    if (!y.empty()) {
        _windowDims.setHeight(std::stoi(y));
    }
    if (_windowChangeCallback) {
        _windowChangeCallback(_index, _windowDims);
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
        _windowChangeCallback(_index, _windowDims);
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
        _windowChangeCallback(_index, _windowDims);
    }
}

void WindowControl::onProjectionChanged(int newSelection) {
    switch (newSelection) {
    case 0:
        _layoutFovWrapper->setVisible(true);
        _layoutHeightOffsetWrapper->setVisible(false);
        break;

    case 1:
        _layoutFovWrapper->setVisible(false);
        _layoutHeightOffsetWrapper->setVisible(false);
        break;

    case 2:
        _layoutFovWrapper->setVisible(false);
        _layoutHeightOffsetWrapper->setVisible(false);
        break;

    case 3:
        _layoutFovWrapper->setVisible(false);
        _layoutHeightOffsetWrapper->setVisible(true);
        break;

    case 4:
        _layoutFovWrapper->setVisible(false);
        _layoutHeightOffsetWrapper->setVisible(false);
        break;
    }
}

void WindowControl::onQualityChanged(int newSelection) {
}

void WindowControl::setDimensions(const QRectF& dimensions) {
    _windowDims = dimensions;
}

void WindowControl::setWindowChangeCallback(std::function<void(unsigned int, const QRectF&)> cb) {
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

QCheckBox* WindowControl::checkBoxFullscreen() {
    return _checkBoxFullscreen;
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
    delete _layoutOffset;
    delete _checkBoxFullscreen;
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
    delete _layoutCBoxFullscreen;
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

