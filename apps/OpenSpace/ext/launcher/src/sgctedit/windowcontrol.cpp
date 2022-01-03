#include "mainwindow.h"
#include "monitorbox.h"

WindowControl::WindowControl(unsigned int windowIndex, QRect& widgetDims,
                                                QRect& monitorResolution, QWidget *parent)
    : _index(windowIndex)
    , _monitorResolution(monitorResolution)
    , QWidget(parent)
{
    if (windowIndex == 0) {
        _size_x = new QLineEdit("800", parent);
        _size_y = new QLineEdit("600", parent);
        _offset_x = new QLineEdit("50", parent);
        _offset_y = new QLineEdit("50", parent);
    }
    else {
        _size_x = new QLineEdit("640", parent);
        _size_y = new QLineEdit("480", parent);
        _offset_x = new QLineEdit("900", parent);
        _offset_y = new QLineEdit("400", parent);
    }
    QIntValidator* _validatorSize_x = new QIntValidator(10, _monitorResolution.width());
    QIntValidator* _validatorSize_y = new QIntValidator(10, _monitorResolution.height());
    QIntValidator* _validatorOffset_x = new QIntValidator(10, _monitorResolution.width() - 10);
    QIntValidator* _validatorOffset_y = new QIntValidator(10, _monitorResolution.height() - 10);

    _size_x->setValidator(_validatorSize_x);
    _size_y->setValidator(_validatorSize_y);
    _offset_x->setValidator(_validatorSize_y);
    _offset_y->setValidator(_validatorSize_y);

    connect(_size_x, SIGNAL(textChanged(const QString&)), this,
            SLOT(onSizeXChanged(const QString&)));
    connect(_size_y, SIGNAL(textChanged(const QString&)), this,
            SLOT(onSizeYChanged(const QString&)));
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

void WindowControl::setDimensions(const QRect& dimensions) {
    _windowDims = dimensions;
}

void WindowControl::setWindowChangeCallback(std::function<void(unsigned int, const QRectF&)> cb) {
    _windowChangeCallback = cb;
}

void WindowControl::setWindowScaleFactor(float scaleFactor) {
    _monitorScaleFactor = scaleFactor;
}

QRect* WindowControl::dimensions() {
    return &_windowDims;
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

WindowControl::~WindowControl()
{
    delete _size_x;
    delete _size_y;
    delete _offset_x;
    delete _offset_y;
}

