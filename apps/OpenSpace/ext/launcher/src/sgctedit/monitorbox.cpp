/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include "sgctedit/monitorbox.h"

constexpr float MarginFractionOfWidgetSize = 0.05f;

MonitorBox::MonitorBox(QRect widgetDims, std::vector<QRect> monitorResolution,
                       unsigned int nWindows, const std::array<QColor, 4>& winColors)
    : _monitorWidgetSize(widgetDims)
    , _monitorResolution(monitorResolution)
    , _nWindows(nWindows)
    , _colorsForWindows(winColors)
{
    _nMonitors = static_cast<unsigned int>(monitorResolution.size());
    _showLabel = (_nMonitors > 1);
    determineMonitorArrangement();
    this->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    float borderMargin = MarginFractionOfWidgetSize * 2.f;
    if (_monitorArrangementAspectRatio > 1.0) {
        borderMargin *= _monitorWidgetSize.width();
        _monitorWidgetSize.setHeight(_monitorWidgetSize.width()
            / _monitorArrangementAspectRatio + borderMargin);
    }
    else {
        borderMargin *= _monitorWidgetSize.height();
        _monitorWidgetSize.setWidth(_monitorWidgetSize.height()
            * _monitorArrangementAspectRatio + borderMargin);
    }
    this->setFixedSize(_monitorWidgetSize.width(), _monitorWidgetSize.height());
    mapMonitorResolutionToWidgetCoordinates();
}

void MonitorBox::paintEvent(QPaintEvent* event) {
    Q_UNUSED(event)
    QPainter painter(this);
    QPen pen = painter.pen();
    painter.setPen(pen);
    paintWidgetBorder(painter, width(), height());
    //Draw window out-of-bounds region(s) first
    for (unsigned int i = 0; i < _nWindows; ++i) {
        paintWindowBeyondBounds(painter, i);
    }
    //Draw & fill monitors over the out-of-bounds regions
    paintMonitorBackgrounds(painter);
    //Draw window number(s) first for darker contrast, then window(s) over both
    //out-of-bounds and monitors
    for (unsigned int i = 0; i < _nWindows; ++i) {
        paintWindowNumber(painter, i);
    }
    for (unsigned int i = 0; i < _nWindows; ++i) {
        paintWindow(painter, i);
    }
}

void MonitorBox::paintWidgetBorder(QPainter& painter, int width, int height) {
    constexpr int Radius = 10;
    painter.setPen(QPen(Qt::gray, 4));
    painter.drawRoundedRect(0, 0, width - 1, height - 1, Radius, Radius);
}

void MonitorBox::paintMonitorBackgrounds(QPainter& painter) {
    painter.setPen(QPen(Qt::black, 2));
    QFont f("Arial");
    f.setPixelSize(24);
    painter.setFont(f);
    for (unsigned int i = 0; i < _nMonitors; ++i) {
        if (i <= _monitorDimensionsScaled.size()) {
            painter.drawRect(_monitorDimensionsScaled[i]);
            QColor fillColor("#DDDDDD");
            QBrush brush(fillColor);
            brush.setStyle(Qt::SolidPattern);
            painter.fillRect(_monitorDimensionsScaled[i], brush);
            if (_showLabel) {
                QPointF textPos = QPointF(
                    _monitorDimensionsScaled[i].left() + 4,
                    _monitorDimensionsScaled[i].top() + 24
                );
                if (i == 0) {
                    painter.drawText(textPos, "Primary");
                }
            }
        }
    }
}

void MonitorBox::paintWindowBeyondBounds(QPainter& painter, unsigned int winIdx) {
    painter.setBrush(Qt::BDiagPattern);
    setPenSpecificToWindow(painter, winIdx, false);
    if (winIdx <= _windowRendering.size()) {
        painter.drawRect(_windowRendering[winIdx]);
    }
    setPenSpecificToWindow(painter, winIdx, true);
    painter.setBrush(Qt::NoBrush);
}

void MonitorBox::paintWindow(QPainter& painter, size_t winIdx) {
    setPenSpecificToWindow(painter, static_cast<unsigned int>(winIdx), true);
    if (winIdx <= _windowRendering.size()) {
        painter.drawRect(_windowRendering[winIdx]);
        QColor fillColor = _colorsForWindows[winIdx];
        fillColor.setAlpha(_alphaWindowOpacity);
        QBrush brush(fillColor);
        brush.setStyle(Qt::SolidPattern);
        painter.fillRect(_windowRendering[winIdx], brush);
    }
}

void MonitorBox::paintWindowNumber(QPainter& painter, unsigned int winIdx) {
    QPointF textPos = QPointF(_windowRendering[winIdx].left() + 5,
        _windowRendering[winIdx].bottom() - 5);
    textPos.setX(std::clamp(textPos.x(), 0.0, _monitorWidgetSize.width() - 10));
    textPos.setY(std::clamp(textPos.y(), 20.0, _monitorWidgetSize.height()));
    painter.drawText(textPos, QString::fromStdString(std::to_string(winIdx + 1)));
}

void MonitorBox::setPenSpecificToWindow(QPainter& painter, unsigned int windowIdx,
                                        bool visibleBorder)
{
    int penWidth = (visibleBorder) ? 1 : -1;
    painter.setPen(QPen(_colorsForWindows[windowIdx], penWidth));
}

void MonitorBox::windowDimensionsChanged(unsigned int mIdx, unsigned int wIdx,
                                         const QRectF& newDimensions)
{
    mapWindowResolutionToWidgetCoordinates(mIdx, wIdx, newDimensions);
}

void MonitorBox::determineMonitorArrangement() {
    for (const QRect& m : _monitorResolution) {
        if (m.x() < _negativeCorrectionOffsets.x()) {
            _negativeCorrectionOffsets.setX(m.x());
        }
        if (m.y() < _negativeCorrectionOffsets.y()) {
            _negativeCorrectionOffsets.setY(m.y());
        }
    }
    for (const QRect& m : _monitorResolution) {
        if ((m.x() + m.width() - _negativeCorrectionOffsets.x())
            > _monitorArrangementDimensions.width())
        {
            _monitorArrangementDimensions.setWidth(
                m.x() + m.width() - _negativeCorrectionOffsets.x());
        }
        if ((m.y() + m.height() - _negativeCorrectionOffsets.y())
            > _monitorArrangementDimensions.height())
        {
            _monitorArrangementDimensions.setHeight(
                m.y() + m.height() - _negativeCorrectionOffsets.y());
        }
    }
    _monitorArrangementAspectRatio = _monitorArrangementDimensions.width()
        / _monitorArrangementDimensions.height();
}

void MonitorBox::mapMonitorResolutionToWidgetCoordinates() {
    if (_monitorArrangementAspectRatio >= 1.0) {
        computeScaledResolutionLandscape(
            _monitorArrangementAspectRatio,
            _monitorArrangementDimensions.width()
        );
    }
    else {
        computeScaledResolutionPortrait(
            _monitorArrangementAspectRatio,
            _monitorArrangementDimensions.height()
        );
    }
    for (size_t m = 0; m < _monitorResolution.size(); ++m) {
        _monitorDimensionsScaled.push_back({
            _monitorOffsets[m].width(),
            _monitorOffsets[m].height(),
            _monitorResolution[m].width() * _monitorScaleFactor,
            _monitorResolution[m].height() * _monitorScaleFactor,
        });
    }
    update();
}

void MonitorBox::computeScaledResolutionLandscape(float aspectRatio, float maxWidth) {
    _marginWidget = _monitorWidgetSize.width() * MarginFractionOfWidgetSize;
    float virtualWidth = _monitorWidgetSize.width()
        * (1.0 - MarginFractionOfWidgetSize * 2.0);
    _monitorScaleFactor = virtualWidth / maxWidth;
    float newHeight = virtualWidth / aspectRatio;
    for (size_t m = 0; m < _monitorResolution.size(); ++m) {
        _monitorOffsets.push_back({
            _marginWidget + (_monitorResolution[m].x() - _negativeCorrectionOffsets.x())
                * _monitorScaleFactor,
            _marginWidget + (_monitorWidgetSize.height() - newHeight - _marginWidget) / 4.0
                + (_monitorResolution[m].y() - _negativeCorrectionOffsets.y())
                * _monitorScaleFactor
        });
    }
}

void MonitorBox::computeScaledResolutionPortrait(float aspectRatio, float maxHeight) {
    _marginWidget = _monitorWidgetSize.height() * MarginFractionOfWidgetSize;
    float virtualHeight = _monitorWidgetSize.height()
        * (1.0 - MarginFractionOfWidgetSize * 2.0);
    _monitorScaleFactor = virtualHeight / maxHeight;
    float newWidth = virtualHeight * aspectRatio;
    for (size_t m = 0; m < _monitorResolution.size(); ++m) {
        _monitorOffsets.push_back({
            _marginWidget + (_monitorWidgetSize.width() - newWidth - _marginWidget) / 4.0
                + (_monitorResolution[m].x() - _negativeCorrectionOffsets.x())
                * _monitorScaleFactor,
            _marginWidget + (_monitorResolution[m].y() - _negativeCorrectionOffsets.y())
                * _monitorScaleFactor
        });
    }
}

void MonitorBox::setNumWindowsDisplayed(unsigned int nWindows) {
    if (_nWindows != nWindows) {
        _nWindows = nWindows;
        update();
    }
}

void MonitorBox::mapWindowResolutionToWidgetCoordinates(unsigned int mIdx,
                                                        unsigned int wIdx,
                                                        const QRectF& winDimensions)
{
    QRectF wF = winDimensions;
    _windowRendering[wIdx] = {
        _monitorDimensionsScaled[mIdx].x() + wF.left() * _monitorScaleFactor,
        _monitorDimensionsScaled[mIdx].y() + wF.top() * _monitorScaleFactor,
        wF.width() * _monitorScaleFactor,
        wF.height() * _monitorScaleFactor
    };
    update();
}
