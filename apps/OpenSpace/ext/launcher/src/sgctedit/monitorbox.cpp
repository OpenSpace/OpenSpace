#include "mainwindow.h"
#include "monitorbox.h"



MonitorBox::MonitorBox(QRect widgetDims, QRect monitorResolution, QWidget *parent)
    : QWidget(parent)
    , _monitorWidgetSize(widgetDims)
{
    mapMonitorResolutionToWidgetCoordinates(monitorResolution);
}

MonitorBox::~MonitorBox()
{ }

void MonitorBox::addWindowControl(WindowControl* wCtrl) {
    if (_nWindows < 2) {
        _windowControl.push_back(wCtrl);
        _windowControl.back()->setWindowChangeCallback(windowDimensionsChanged);
        _nWindows++;
    }
}

void MonitorBox::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event);
    QPainter painter(this);
    QPen pen = painter.pen();
    painter.setPen(pen);
    //Draw monitor outline
    painter.setPen(Qt::black);
    painter.drawRect(_monitorDimensionsScaled);
    //Draw window(s)
    painter.setPen(Qt::blue);
    for (QRectF* w : _windowRendering) {
        painter.drawRect(*w);
    }
}

void MonitorBox::windowDimensionsChanged(unsigned int index, const QRectF& newDimensions) {
    mapWindowResolutionToWidgetCoordinates(index, newDimensions);
}

void MonitorBox::mapMonitorResolutionToWidgetCoordinates(QRect r) {
//    _monitorResolution = r;
    float aspectRatio = r.width() / r.height();
    _marginWidget = _monitorWidgetSize.width() * _marginFractionOfWidgetSize;
    if (aspectRatio >= 1.0) {
        float newWidth = _monitorWidgetSize.width()
            * (1.0 - _marginFractionOfWidgetSize * 2.0);
        _monitorScaleFactor = newWidth / static_cast<float>(r.width());
        float newHeight = newWidth / aspectRatio;
        _monitorRendering = {
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
        _monitorRendering = {
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
    if (index < _windowRendering.size() ) {
        if (_windowRendering[index]) {
            *_windowRendering[index] = {
                _marginWidget + _monitorBoundaryRect.topLeft().x() + w.left() * _monitorScaleFactor,
                _marginWidget + _monitorBoundaryRect.topLeft().y() + w.top() * _monitorScaleFactor,
                w.width() * _monitorScaleFactor,
                w.height() * _monitorScaleFactor
            };
        }
//        _windowRendering[index] = w * _monitorScaleFactor;
    }
}

//float MonitorBox::monitorScaleFactor() {
//    return _monitorScaleFactor;
//}

int MonitorBox::numWindows() {
    return _nWindows;
}

