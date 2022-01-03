#include "monitorbox.h"



MonitorBox::MonitorBox(QRect widgetDims, QSize monitorResolution, QWidget *parent)
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
    painter.setPen(QPen(Qt::gray, 4));
    painter.drawRoundedRect(0, 0, width() - 1, height() - 1, 10, 10);
    //Draw monitor outline
    painter.setPen(QPen(Qt::black, 2));
    painter.drawRect(_monitorDimensionsScaled);
    //Draw window(s)
    painter.setPen(Qt::blue);
    painter.setFont(QFont("Arial", 16));
    for (unsigned int i = 0; i < _nWindows; ++i) {
        if (i <= _windowRendering.size()) {
            painter.drawRect(*_windowRendering[i]);
            QPointF t = QPointF(_windowRendering[i]->left() + 5,
                _windowRendering[i]->bottom() - 5);
            painter.drawText(t, QString::fromStdString(std::to_string(i + 1)));
        }
    }
}

void MonitorBox::windowDimensionsChanged(unsigned int index, const QRectF newDimensions) {
    mapWindowResolutionToWidgetCoordinates(index, newDimensions);
}

void MonitorBox::mapMonitorResolutionToWidgetCoordinates(QSize r) {
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

void MonitorBox::setResolution(QSize& res) {
    _monitorResolution = res;
}

void MonitorBox::setNumWindowsDisplayed(unsigned int nWindows) {
    if (_nWindows != nWindows) {
        _nWindows = nWindows;
        this->update();
    }
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

void MonitorBox::removeAdditionalWindowDimensions() {
    if (_windowRendering.size() > 1) {
        delete _windowRendering.back();
        _windowRendering.pop_back();
    }
}

