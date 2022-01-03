#include "mainwindow.h"
#include "monitorbox.h"

MonitorBox::MonitorBox(QLineEdit* size_text_x, QLineEdit* size_text_y, QWidget *parent)
  : QWidget(parent)
  , _monitorWidgetSize({300.0, 300.0})
  , _monitorDimensions(_monitorWidgetSize._width * (1.0 - _marginFractionOfWidgetSize * 2.0),
                       _monitorWidgetSize._height * (1.0 - _marginFractionOfWidgetSize * 2.0))
  , _size_x(size_text_x)
  , _size_y(size_text_y)
{
    setFixedSize(_monitorWidgetSize._width, _monitorWidgetSize._height);

    _monitorRect = {
        _monitorWidgetSize._width * _marginFractionOfWidgetSize,
        _monitorWidgetSize._height * _marginFractionOfWidgetSize,
        _monitorWidgetSize._width * (1.0 - _marginFractionOfWidgetSize * 2.0),
        _monitorWidgetSize._height * (1.0 - _marginFractionOfWidgetSize * 2.0)
    };

    connect(_size_x, SIGNAL(textChanged(const QString&)), this,
            SLOT(onSizeXChanged(const QString&)));
    connect(_size_y, SIGNAL(textChanged(const QString&)), this,
            SLOT(onSizeYChanged(const QString&)));
}

MonitorBox::~MonitorBox()
{ }

void MonitorBox::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event);
    QPainter painter(this);
    QPen pen = painter.pen();
    painter.setPen(pen);
    painter.setPen(Qt::black);
    painter.drawRect(_monitorRect);
    painter.setPen(Qt::blue);
    for (QRectF w : _windowRect) {
        painter.drawRect(w);
    }
}

void MonitorBox::onSizeXChanged(const QString& newText) {
    ConfigResolution offset(50, 50);
    std::string x = newText.toStdString();
    if (!x.empty()) {
        _windowResolutions[0]._width = std::stoi(x);
    }
    setWindowSize(0, _windowResolutions[0], offset);
}

void MonitorBox::onSizeYChanged(const QString& newText) {
    ConfigResolution size(0, 400);
    ConfigResolution offset(50, 50);
    std::string y = newText.toStdString();
    if (!y.empty()) {
        _windowResolutions[0]._height = std::stoi(y);
    }
    setWindowSize(0, _windowResolutions[0], offset);
}

void MonitorBox::setMonitorResolution(ConfigResolution r) {
    _monitorDimensions = r;
    float aspectRatio = r._width / r._height;
    float margin = _monitorWidgetSize._width * _marginFractionOfWidgetSize;
    if (aspectRatio >= 1.0) {
        float newWidth = _monitorWidgetSize._width
            * (1.0 - _marginFractionOfWidgetSize * 2.0);
        _monitorScaleFactor = newWidth / static_cast<float>(r._width);
        float newHeight = newWidth / aspectRatio;
        _monitorRect = {
            margin,
            margin + (_monitorWidgetSize._height - newHeight) / 2.0,
            newWidth,
            newHeight
        };
    }
    else {
        float newHeight = _monitorWidgetSize._height
            * (1.0 - _marginFractionOfWidgetSize * 2.0);
        _monitorScaleFactor = newHeight / static_cast<float>(r._height);
        float newWidth = newHeight * aspectRatio;
        _monitorRect = {
            margin + (_monitorWidgetSize._width - newWidth) / 2.0,
            margin,
            newWidth,
            newHeight
        };
    }
    this->update();
}

void MonitorBox::setNumWindows(int nWindows) {
    if (nWindows >= 1 && nWindows <= 2) {
        _nWindows = nWindows;
        while (_windowResolutions.size() < _nWindows) {
            _windowResolutions.push_back({0, 0});
            _windowRect.push_back({0, 0, 0, 0});
        }
        if (_windowResolutions.size() > _nWindows) {
            _windowResolutions.pop_back();
            _windowRect.pop_back();
        }
    }
}

int MonitorBox::numWindows() {
    return _nWindows;
}

void MonitorBox::setWindowSize(int index, ConfigResolution r, ConfigResolution offset) {
    if (index <= _nWindows - 1) {
        _windowResolutions[index] = r;
    }
    _windowRect[index] = {
        _monitorRect.topLeft().x() + offset._width * _monitorScaleFactor,
        _monitorRect.topLeft().y() + offset._height * _monitorScaleFactor,
        r._width * _monitorScaleFactor,
        r._height * _monitorScaleFactor
    };
    this->update();
}
