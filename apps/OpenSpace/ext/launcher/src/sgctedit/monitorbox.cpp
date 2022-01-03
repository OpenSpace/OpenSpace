#include "monitorbox.h"



MonitorBox::MonitorBox(QRect widgetDims, QRect monitorResolution, QWidget *parent)
    : QWidget(parent)
    , _monitorWidgetSize(widgetDims)
{
    mapMonitorResolutionToWidgetCoordinates(monitorResolution);
}

MonitorBox::~MonitorBox()
{
    while (_windowRendering.size() > 0) {
        delete _windowRendering.back();
        _windowRendering.pop_back();
    }
}

void MonitorBox::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event);
    QPainter painter(this);
    QPen pen = painter.pen();
    painter.setPen(pen);
    //Draw border
    painter.setPen(Qt::gray);
    painter.drawRoundedRect(0, 0, width() - 1, height() - 1, 10, 10);
    //Draw monitor outline
    painter.setPen(Qt::black);
    painter.drawRect(_monitorDimensionsScaled);
    //Draw window(s)
    painter.setPen(Qt::blue);
    for (QRectF* w : _windowRendering) {
        painter.drawRect(*w);
    }
}

void MonitorBox::windowDimensionsChanged(unsigned int index, const QRectF newDimensions) {
    mapWindowResolutionToWidgetCoordinates(index, newDimensions);
}

void MonitorBox::mapMonitorResolutionToWidgetCoordinates(QRect r) {
//    _monitorResolution = r;
    float aspectRatio = static_cast<float>(r.width()) /
        static_cast<float>(r.height());
    _marginWidget = _monitorWidgetSize.width() * _marginFractionOfWidgetSize;
    if (aspectRatio >= 1.0) {
        float newWidth = _monitorWidgetSize.width()
            * (1.0 - _marginFractionOfWidgetSize * 2.0);
        _monitorScaleFactor = newWidth / static_cast<float>(r.width());
        float newHeight = newWidth / aspectRatio;
        _monitorDimensionsScaled = {
            _marginWidget,
            _marginWidget + (_monitorWidgetSize.height() - newHeight) / 2.0,
            newWidth,
            newHeight
        };
    }
    else {
        float newHeight = _monitorWidgetSize.height()
            * (1.0 - _marginFractionOfWidgetSize * 2.0);
        _monitorScaleFactor = newHeight / static_cast<float>(r.height());
        float newWidth = newHeight * aspectRatio;
        _monitorDimensionsScaled = {
            _marginWidget + (_monitorWidgetSize.width() - newWidth) / 2.0,
            _marginWidget,
            newWidth,
            newHeight
        };
    }
    this->update();
}

void MonitorBox::setResolution(QRect& res) {
    _monitorResolution = res;
}

void MonitorBox::mapWindowResolutionToWidgetCoordinates(unsigned int index, const QRectF& w) {
    if (index > 1) {
        return;
    }
    while ((index + 1) > _windowRendering.size()) {
        _windowRendering.push_back(new QRectF());
    }
    if (_windowRendering[index]) {
        QRectF wF = w;
        *_windowRendering[index] = {
            _monitorDimensionsScaled.x() + wF.left() * _monitorScaleFactor,
            _monitorDimensionsScaled.y() + wF.top() * _monitorScaleFactor,
            wF.width() * _monitorScaleFactor,
            wF.height() * _monitorScaleFactor
        };
    }
    this->update();
}

