#include "monitorbox.h"


MonitorBox::MonitorBox(QRect widgetDims, std::vector<QRect> monitorResolution,
                                                                    bool showMonitorLabel)
    : _monitorWidgetSize(widgetDims)
    , _monitorResolution(monitorResolution)
    , _showLabel(showMonitorLabel)
{
    _nMonitors = monitorResolution.size();
    mapMonitorResolutionToWidgetCoordinates();
}

MonitorBox::~MonitorBox()
{
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

    //Draw monitor outline(s)
    painter.setPen(QPen(Qt::black, 2));
    painter.setFont(QFont("Arial", 14));
    for (unsigned int i = 0; i < _nMonitors; ++i) {
        if (i <= _monitorDimensionsScaled.size()) {
            painter.drawRect(_monitorDimensionsScaled[i]);
            if (_showLabel) {
                QPointF textPos = QPointF(_monitorDimensionsScaled[i].left() + 5,
                    _monitorDimensionsScaled[i].bottom() - 5);
                painter.drawText(textPos, QString::fromStdString(std::to_string(i + 1)));
            }
        }
    }

    //Draw window outline(s)
    painter.setPen(Qt::blue);
    for (unsigned int i = 0; i < _nMonitors ; ++i) {
        for (unsigned int j = 0; j < _nWindows[i]; ++j) {
            if (j <= _windowRendering[i].size()) {
                painter.drawRect(_windowRendering[i][j]);
                QPointF textPos = QPointF(_windowRendering[i][j].left() + 5,
                    _windowRendering[i][j].bottom() - 5);
                textPos.setX(std::clamp(textPos.x(), 0.0, _monitorWidgetSize.width() - 10));
                textPos.setY(std::clamp(textPos.y(), 0.0, _monitorWidgetSize.height() - 10));
                painter.drawText(textPos, QString::fromStdString(std::to_string(j + 1)));
            }
        }
    }
}

void MonitorBox::windowDimensionsChanged(unsigned int monitorIdx, unsigned int windowIdx,
                                                               const QRectF& newDimensions)
{
    mapWindowResolutionToWidgetCoordinates(monitorIdx, windowIdx, newDimensions);
}

void MonitorBox::mapMonitorResolutionToWidgetCoordinates() {
    QSize virtualDesktopResolution;
    float maxWidth = 0.0;
    float maxHeight = 0.0;
    for (auto m : _monitorResolution) {
        if ((m.x() + m.width()) > maxWidth) {
            maxWidth = m.x() + m.width();
        }
        if ((m.y() + m.height()) > maxHeight) {
            maxHeight = m.y() + m.height();
        }
    }
    float aspectRatio = maxWidth / maxHeight;
    if (aspectRatio >= 1.0) {
        _marginWidget = _monitorWidgetSize.width() * _marginFractionOfWidgetSize;
        float virtualWidth = _monitorWidgetSize.width()
            * (1.0 - _marginFractionOfWidgetSize * 2.0);
        _monitorScaleFactor = virtualWidth / maxWidth;
        float newHeight = virtualWidth / aspectRatio;
        for (size_t m = 0; m < _monitorResolution.size(); ++m) {
            _monitorOffsets.push_back({
                _marginWidget + _monitorResolution[m].x() * _monitorScaleFactor,
                _marginWidget + (_monitorWidgetSize.height() - newHeight) / 2.0 +
                    _monitorResolution[m].y() * _monitorScaleFactor
            });
        }
    }
    else {
        _marginWidget = _monitorWidgetSize.height() * _marginFractionOfWidgetSize;
        float virtualHeight = _monitorWidgetSize.height()
            * (1.0 - _marginFractionOfWidgetSize * 2.0);
        _monitorScaleFactor = virtualHeight / maxHeight;
        float newWidth = virtualHeight * aspectRatio;
        for (size_t m = 0; m < _monitorResolution.size(); ++m) {
            _monitorOffsets.push_back({
                _marginWidget + (_monitorWidgetSize.width() - newWidth) / 2.0
                    + _monitorResolution[m].x() * _monitorScaleFactor,
                _marginWidget + _monitorResolution[m].y() * _monitorScaleFactor
            });
        }
    }

    for (size_t m = 0; m < _monitorResolution.size(); ++m) {
        _monitorDimensionsScaled.push_back({
            _monitorOffsets[m].width(),
            _monitorOffsets[m].height(),
            _monitorResolution[m].width() * _monitorScaleFactor,
            _monitorResolution[m].height() * _monitorScaleFactor,
        });
    }
    this->update();
}

void MonitorBox::setResolution(unsigned int index, QRect& res) {
    _monitorResolution[index] = res;
}

void MonitorBox::setNumWindowsDisplayed(unsigned int mIdx, unsigned int nWindows) {
    if ((mIdx <= (_nMonitors - 1)) && (_nWindows[mIdx] != nWindows)) {
        _nWindows[mIdx] = nWindows;
        this->update();
    }
}

void MonitorBox::mapWindowResolutionToWidgetCoordinates(unsigned int mIdx,
                                                        unsigned int wIdx,
                                                        const QRectF& w)
{
    if (mIdx > (_maxNumMonitors - 1) || wIdx > (_maxNumWindowsPerMonitor - 1)) {
        return;
    }
    QRectF wF = w;
    _windowRendering[mIdx][wIdx] = {
        _monitorDimensionsScaled[mIdx].x() + wF.left() * _monitorScaleFactor,
        _monitorDimensionsScaled[mIdx].y() + wF.top() * _monitorScaleFactor,
        wF.width() * _monitorScaleFactor,
        wF.height() * _monitorScaleFactor
    };
    this->update();
}

