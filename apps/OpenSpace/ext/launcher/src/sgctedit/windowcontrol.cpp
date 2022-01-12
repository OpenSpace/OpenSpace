#include "sgctedit/display.h"
#include "sgctedit/monitorbox.h"
#include "sgctedit/windowcontrol.h"

WindowControl::WindowControl(unsigned int monitorIndex, unsigned int windowIndex,
                                    QRect& widgetDims, QRect& monitorDims, QWidget *parent)
    : _monIndex(monitorIndex)
    , _index(windowIndex)
    , _monitorResolution(monitorDims)
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
        = new QIntValidator(-_maxWindowSizePixels, _maxWindowSizePixels);
    QIntValidator* _validatorOffset_y
        = new QIntValidator(-_maxWindowSizePixels, _maxWindowSizePixels);
    _size_x->setValidator(_validatorSize_x);
    _size_y->setValidator(_validatorSize_y);
    _offset_x->setValidator(_validatorOffset_x);
    _offset_y->setValidator(_validatorOffset_y);

    _fullscreenButton = new QPushButton(this);
    _fullscreenButton->setText("Set to Fullscreen");
    _checkBoxWindowDecor = new QCheckBox("Window Decoration", this);
    _checkBoxWebGui = new QCheckBox("WebGUI only this window", this);
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
    connect(_checkBoxSpoutOutput, SIGNAL(stateChanged(int)),
            this, SLOT(onSpoutSelection(int)));
    connect(_checkBoxWebGui, SIGNAL(stateChanged(int)),
            this, SLOT(onWebGuiSelection(int)));

    connect(_fullscreenButton, SIGNAL(released()), this, SLOT(onFullscreenClicked()));
}

QVBoxLayout* WindowControl::initializeLayout(QWidget* parentWidget) {
    _layoutFullWindow = new QVBoxLayout();
    //Window size
    _layoutWindowCtrl = new QVBoxLayout();

    _labelWinNum = new QLabel();
    _labelWinNum->setText("Window " + QString::number(_index + 1));
    if (_index == 1) {
        _labelWinNum->setStyleSheet("QLabel { color : #CD6D1D; }");
    }
    else {
        _labelWinNum->setStyleSheet("QLabel { color : #1C1B8B; }");
    }

    _layoutWinNum = new QHBoxLayout();
    _layoutWinNum->addStretch(1);
    _layoutWinNum->addWidget(_labelWinNum);
    _layoutWinNum->addStretch(1);
    _layoutWindowCtrl->addLayout(_layoutWinNum);

    _layoutName = new QHBoxLayout();
    _labelName = new QLabel(this);
    _labelName->setText("Name: ");
    _windowName = new QLineEdit(this);
    _windowName->setFixedWidth(160);
    _layoutName->addWidget(_labelName);
    _layoutName->addWidget(_windowName);
    _layoutName->addStretch(1);
    _layoutWindowCtrl->addLayout(_layoutName);
    _size_x->setFixedWidth(_lineEditWidthFixed);
    _size_y->setFixedWidth(_lineEditWidthFixed);
    _labelSize = new QLabel(this);
    _labelDelim = new QLabel(this);
    _layoutSize = new QHBoxLayout();
    _layoutSize->addWidget(_labelSize);
    _labelSize->setText("Size:");
    _labelSize->setFixedWidth(55);
    _layoutSize->addWidget(_size_x);
    _layoutSize->addWidget(_labelDelim);
    _layoutSize->addWidget(_size_y);
    _layoutSize->addStretch(1);
    _labelDelim->setText("x");
    _labelDelim->setFixedWidth(9);
    _layoutWindowCtrl->addLayout(_layoutSize);

    //Window offset
    _offset_x->setFixedWidth(_lineEditWidthFixed);
    _offset_y->setFixedWidth(_lineEditWidthFixed);
    _labelOffset = new QLabel(this);
    _labelComma = new QLabel(this);
    _layoutOffset = new QHBoxLayout();
    _layoutOffset->addWidget(_labelOffset);
    _labelOffset->setText("Offset:");
    _labelOffset->setFixedWidth(55);
    _layoutOffset->addWidget(_offset_x);
    _layoutOffset->addWidget(_labelComma);
    _layoutOffset->addWidget(_offset_y);
    _layoutOffset->addStretch(1);
    _labelComma->setText(",");
    _labelComma->setFixedWidth(9);
    _layoutWindowCtrl->addLayout(_layoutOffset);

    //Window options
    _layoutCheckboxesFull1 = new QHBoxLayout();
    _layoutCheckboxesFull2 = new QVBoxLayout();
    _layoutFullscreenButton = new QHBoxLayout();
    _layoutFullscreenButton->addWidget(_fullscreenButton);
    _layoutFullscreenButton->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutFullscreenButton);
    _layoutCBoxWindowDecor = new QHBoxLayout();
    _layoutCBoxWindowDecor->addWidget(_checkBoxWindowDecor);
    _layoutCBoxWindowDecor->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutCBoxWindowDecor);
    _layoutCBoxWebGui= new QHBoxLayout();
    _layoutCBoxWebGui->addWidget(_checkBoxWebGui);
    _layoutCBoxWebGui->addStretch(1);
    _layoutCheckboxesFull2->addLayout(_layoutCBoxWebGui);
    _layoutCBoxSpoutOutput= new QHBoxLayout();
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
    _size_x->setText(QString::number(_monitorResolution.width()));
    _size_y->setText(QString::number(_monitorResolution.height()));
}

void WindowControl::enableGuiWindowSelection(bool enabled) {
    _checkBoxWebGui->setEnabled(enabled);
}

void WindowControl::onWebGuiSelection(int selectionState) {
    if (_windowGuiCheckCallback && (selectionState == Qt::Checked)) {
        _windowGuiCheckCallback(_monIndex, _index);
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
        enableSpoutProjectionOptions(_comboProjection, false);
    }
    else {
        enableSpoutProjectionOptions(_comboProjection, true);
    }
}

void WindowControl::enableSpoutProjectionOptions(QComboBox* comboBox, bool enable) {
    auto * comboModel = qobject_cast<QStandardItemModel*>(comboBox->model());
    if (comboModel) {
        enableProjectionOption(comboModel, ProjectionIndeces::Planar, enable);
        enableProjectionOption(comboModel, ProjectionIndeces::Spherical_Mirror, enable);
        enableProjectionOption(comboModel, ProjectionIndeces::Cylindrical, enable);
    }
}

template <typename T>
void WindowControl::enableProjectionOption(T* comboModel, int selectionIndex, bool enable)
{
    auto* item = comboModel->item(selectionIndex);
    if (item) {
        item->setEnabled(enable);
    }
}

void WindowControl::onProjectionChanged(int newSelection) {
    switch (newSelection) {
    case ProjectionIndeces::Planar:
        _comboQuality->setEnabled(false);
        _labelFov->setEnabled(true);
        _lineFov->setEnabled(true);
        _labelHeightOffset->setEnabled(false);
        _lineHeightOffset->setEnabled(false);
        _checkBoxSpoutOutput->setEnabled(false);
        break;

    case ProjectionIndeces::Fisheye:
        _comboQuality->setEnabled(true);
        _labelFov->setEnabled(false);
        _lineFov->setEnabled(false);
        _labelHeightOffset->setEnabled(false);
        _lineHeightOffset->setEnabled(false);
        _checkBoxSpoutOutput->setEnabled(true);
        break;

    case ProjectionIndeces::Spherical_Mirror:
        _comboQuality->setEnabled(true);
        _labelFov->setEnabled(false);
        _lineFov->setEnabled(false);
        _labelHeightOffset->setEnabled(false);
        _lineHeightOffset->setEnabled(false);
        _checkBoxSpoutOutput->setEnabled(false);
        break;

    case ProjectionIndeces::Cylindrical:
        _comboQuality->setEnabled(true);
        _labelFov->setEnabled(false);
        _lineFov->setEnabled(false);
        _labelHeightOffset->setEnabled(true);
        _lineHeightOffset->setEnabled(true);
        _checkBoxSpoutOutput->setEnabled(false);
        break;

    case ProjectionIndeces::Equirectangular:
        _comboQuality->setEnabled(true);
        _labelFov->setEnabled(false);
        _lineFov->setEnabled(false);
        _labelHeightOffset->setEnabled(false);
        _lineHeightOffset->setEnabled(false);
        _checkBoxSpoutOutput->setEnabled(true);
        break;
    }
}

void WindowControl::setDimensions(const QRectF& dimensions) {
    _windowDims = dimensions;
}

void WindowControl::setWindowChangeCallback(
                        std::function<void(int, int, const QRectF&)> cb)
{
    _windowChangeCallback = cb;
}

void WindowControl::setWebGuiChangeCallback(
                        std::function<void(unsigned int, unsigned int)> cb)
{
    _windowGuiCheckCallback = cb;
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

float WindowControl::fov() {
    return _lineFov->text().toFloat();
}

float WindowControl::heightOffset() {
    return _lineHeightOffset->text().toFloat();
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
    delete _labelWinNum;
    delete _labelSize;
    delete _labelDelim;
    delete _layoutSize;
    delete _labelOffset;
    delete _labelComma;
    delete _layoutOffset;
    delete _checkBoxWindowDecor;
    delete _checkBoxWebGui;
    delete _checkBoxSpoutOutput;
    delete _comboProjection;
    delete _comboQuality;
    delete _fullscreenButton;
    delete _labelFov;
    delete _lineFov;
    delete _validatorFov;
    delete _labelHeightOffset;
    delete _lineHeightOffset;
    delete _validatorHeightOffset;
    delete _layoutFullscreenButton;
    delete _layoutCBoxWindowDecor;
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
    delete _layoutWinNum;
    delete _layoutWindowCtrl;
    delete _layoutFullWindow;
}

