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

    connect(_size_x, SIGNAL(textChanged(const QString&)), this,
            SLOT(onSizeXChanged(const QString&)));
    connect(_size_y, SIGNAL(textChanged(const QString&)), this,
            SLOT(onSizeYChanged(const QString&)));
    connect(_offset_x, SIGNAL(textChanged(const QString&)), this,
            SLOT(onOffsetXChanged(const QString&)));
    connect(_offset_y, SIGNAL(textChanged(const QString&)), this,
            SLOT(onOffsetYChanged(const QString&)));
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
    delete _offset_x;
    delete _offset_y;
}

