#include "sgctedit/monitorbox.h"


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

    paintWidgetBorder(painter, width(), height());
    paintMonitorOutlines(painter);

    //Draw window outline(s)
    for (unsigned int i = 0; i < _nMonitors ; ++i) {
        for (unsigned int j = 0; j < _nWindows[i]; ++j) {
            paintWindow(painter, i, j);
        }
    }
}

void MonitorBox::paintWidgetBorder(QPainter& painter, int width, int height) {
    painter.setPen(QPen(Qt::gray, 4));
    painter.drawRoundedRect(0, 0, width - 1, height - 1, 10, 10);
}

void MonitorBox::paintMonitorOutlines(QPainter& painter) {
    painter.setPen(QPen(Qt::black, 2));
    painter.setFont(QFont("Arial", 14));
    for (unsigned int i = 0; i < _nMonitors; ++i) {
        if (i <= _monitorDimensionsScaled.size()) {
            painter.drawRect(_monitorDimensionsScaled[i]);
            if (_showLabel) {
                QPointF textPos = QPointF(_monitorDimensionsScaled[i].left() + 5,
                    _monitorDimensionsScaled[i].top() + 18);
                painter.drawText(textPos, QString::fromStdString(std::to_string(i + 1)));
            }
        }
    }
}

void MonitorBox::paintWindow(QPainter& painter, unsigned int monIdx,
                                                                      unsigned int winIdx)
{
    setPenSpecificToWindow(painter, winIdx, true);
    if (winIdx <= _windowRendering[monIdx].size()) {
        painter.drawRect(_windowRendering[monIdx][winIdx]);
        QColor fillColor = _colorWindow[winIdx];
        fillColor.setAlpha(_alphaWindowTransparency);
        QBrush brush(fillColor);
        brush.setStyle(Qt::SolidPattern);
        painter.fillRect(_windowRendering[monIdx][winIdx], brush);
        //Draw areas of window that are past the monitor boundaries
        if (_outOfBoundsRect[monIdx][winIdx].size() > 0) {
            paintOutOfBoundsAreas(painter, monIdx, winIdx);
        }
        paintWindowNumber(painter, monIdx, winIdx);
    }
}

void MonitorBox::paintOutOfBoundsAreas(QPainter& painter, unsigned int monIdx,
                                                                      unsigned int winIdx)
{
    painter.setBrush(Qt::BDiagPattern);
    setPenSpecificToWindow(painter, winIdx, false);
    for (QRectF r : _outOfBoundsRect[monIdx][winIdx]) {
        painter.drawRect(r);
    }
    setPenSpecificToWindow(painter, winIdx, true);
    painter.setBrush(Qt::NoBrush);
}

void MonitorBox::paintWindowNumber(QPainter& painter, unsigned int monIdx,
                                                                      unsigned int winIdx)
{
    QPointF textPos = QPointF(_windowRendering[monIdx][winIdx].left() + 5,
        _windowRendering[monIdx][winIdx].bottom() - 5);
    textPos.setX(std::clamp(textPos.x(), 0.0,
        _monitorWidgetSize.width() - 10));
    textPos.setY(std::clamp(textPos.y(), 0.0,
        _monitorWidgetSize.height() - 10));
    painter.drawText(textPos, QString::fromStdString(std::to_string(winIdx + 1)));
}

void MonitorBox::setPenSpecificToWindow(QPainter& painter, unsigned int windowIdx,
                                                                       bool visibleBorder)
{
    int penWidth = (visibleBorder) ? 1 : -1;
    painter.setPen(QPen(_colorWindow[windowIdx], penWidth));
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
        computeScaledResolution_landscape(aspectRatio, maxWidth);
    }
    else {
        computeScaledResolution_portrait(aspectRatio, maxHeight);
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

void MonitorBox::computeScaledResolution_landscape(float aspectRatio, float maxWidth) {
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

void MonitorBox::computeScaledResolution_portrait(float aspectRatio, float maxHeight) {
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

void MonitorBox::setNumWindowsDisplayed(unsigned int mIdx, unsigned int nWindows) {
    if ((mIdx <= (_nMonitors - 1)) && (_nWindows[mIdx] != nWindows)) {
        _nWindows[mIdx] = nWindows;
        this->update();
    }
}

void printWindowDims(QRectF& r) {
    std::cout << r.width() << "x" << r.height() << " + " << r.x() << "," << r.y();
    std::cout << std::endl;
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
    _outOfBoundsRect[mIdx][wIdx].clear();
    computeOutOfBounds_horizontal(mIdx, wIdx);
    computeOutOfBounds_vertical(mIdx, wIdx);
    this->update();
}

void MonitorBox::computeOutOfBounds_horizontal(unsigned int mIdx, unsigned int wIdx) {
    qreal windowWidthPlusOffset = _windowRendering[mIdx][wIdx].width()
        + _windowRendering[mIdx][wIdx].x() - _monitorDimensionsScaled[mIdx].x();
    if (windowWidthPlusOffset > _monitorDimensionsScaled[mIdx].width()) {
        qreal bounds_x = std::max(
            _monitorDimensionsScaled[mIdx].width() + _monitorDimensionsScaled[mIdx].x(),
            _windowRendering[mIdx][wIdx].x()
        );
        qreal extent_x = std::min(
            _windowRendering[mIdx][wIdx].width(),
            windowWidthPlusOffset - _monitorDimensionsScaled[mIdx].width()
        );
        addOutOfBoundsArea_horizontal(mIdx, wIdx, bounds_x, extent_x);
    }
    if (_windowRendering[mIdx][wIdx].x() < _monitorDimensionsScaled[mIdx].x() ) {
        qreal extent_x = std::min(
            _windowRendering[mIdx][wIdx].width(),
            _monitorDimensionsScaled[mIdx].x() - _windowRendering[mIdx][wIdx].x()
        );
        addOutOfBoundsArea_horizontal(mIdx, wIdx, _windowRendering[mIdx][wIdx].x(),
            extent_x);
    }
}

void MonitorBox::addOutOfBoundsArea_horizontal(unsigned int mIdx, unsigned int wIdx,
                                                               qreal bounds, qreal extent)
{
    _outOfBoundsRect[mIdx][wIdx].push_back({
        bounds,
        _windowRendering[mIdx][wIdx].y(),
        extent,
        _windowRendering[mIdx][wIdx].height()
    });
}

void MonitorBox::computeOutOfBounds_vertical(unsigned int mIdx, unsigned int wIdx) {
    qreal windowHeightPlusOffset = _windowRendering[mIdx][wIdx].height()
        + _windowRendering[mIdx][wIdx].y() - _monitorDimensionsScaled[mIdx].y();
    if (windowHeightPlusOffset > _monitorDimensionsScaled[mIdx].height()) {
        qreal bounds_y = std::max(
            _monitorDimensionsScaled[mIdx].height() + _monitorDimensionsScaled[mIdx].y(),
            _windowRendering[mIdx][wIdx].y()
        );
        qreal extent_y = std::min(
            _windowRendering[mIdx][wIdx].height(),
            windowHeightPlusOffset - _monitorDimensionsScaled[mIdx].height()
        );
        addOutOfBoundsArea_vertical(mIdx, wIdx, bounds_y, extent_y);
    }
    if (_windowRendering[mIdx][wIdx].y() < _monitorDimensionsScaled[mIdx].y() ) {
        qreal extent_y = std::min(
            _windowRendering[mIdx][wIdx].height(),
            _monitorDimensionsScaled[mIdx].y() - _windowRendering[mIdx][wIdx].y()
        );
        _outOfBoundsRect[mIdx][wIdx].push_back({
            _windowRendering[mIdx][wIdx].x(),
            _windowRendering[mIdx][wIdx].y(),
            _windowRendering[mIdx][wIdx].width(),
            extent_y
        });
        addOutOfBoundsArea_vertical(mIdx, wIdx, _windowRendering[mIdx][wIdx].y(),
            extent_y);
    }
}

void MonitorBox::addOutOfBoundsArea_vertical(unsigned int mIdx, unsigned int wIdx,
                                                               qreal bounds, qreal extent)
{
    _outOfBoundsRect[mIdx][wIdx].push_back({
        _windowRendering[mIdx][wIdx].x(),
        bounds,
        _windowRendering[mIdx][wIdx].width(),
        extent
    });
}
