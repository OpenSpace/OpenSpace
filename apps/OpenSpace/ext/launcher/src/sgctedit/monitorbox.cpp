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

#include <QPainter>

namespace {
    constexpr float MarginFractionOfWidgetSize = 0.05f;
    constexpr int WindowOpacity = 170;

    QRectF computeUnion(const std::vector<QRect>& monitorResolutions) {
        QRectF res = { 0.f, 0.f, 0.f, 0.f };
        for (const QRect& m : monitorResolutions) {
            res |= m;
        }
        return res;
    }
} // namespace

MonitorBox::MonitorBox(QRect widgetDims, std::vector<QRect> monitorResolutions,
                       unsigned int nWindows, const std::array<QColor, 4>& windowColors)
    : _monitorWidgetSize(widgetDims)
    , _monitorResolutions(std::move(monitorResolutions))
    , _nWindows(nWindows)
    , _colorsForWindows(windowColors)
{
    setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    
    QRectF monitorArrangement = computeUnion(_monitorResolutions);
    float aspectRatio = monitorArrangement.width() / monitorArrangement.height();
    
    if (aspectRatio > 1.0) {
        float borderMargin =
            2.f * MarginFractionOfWidgetSize * _monitorWidgetSize.width();
        _monitorWidgetSize.setHeight(
            _monitorWidgetSize.width() / aspectRatio + borderMargin
        );
    }
    else {
        float borderMargin =
            2.f * MarginFractionOfWidgetSize * _monitorWidgetSize.height();
        _monitorWidgetSize.setWidth(
            _monitorWidgetSize.height() * aspectRatio + borderMargin
        );
    }
    setFixedSize(_monitorWidgetSize.width(), _monitorWidgetSize.height());
    mapMonitorResolutionToWidgetCoordinates(monitorArrangement);
    update();
}

void MonitorBox::paintEvent(QPaintEvent*) {
    QPainter painter(this);
    
    //
    // Draw widget border
    constexpr int Radius = 10;
    painter.setPen(QPen(Qt::gray, 4));
    painter.drawRoundedRect(0, 0, width() - 1, height() - 1, Radius, Radius);

    //
    // Draw window out-of-bounds region(s) first
    for (unsigned int i = 0; i < _nWindows; ++i) {
        painter.setBrush(Qt::BDiagPattern);
        painter.setPen(QPen(_colorsForWindows[i], 0));
        painter.drawRect(_windowRendering[i]);
    }

    // Draw & fill monitors over the out-of-bounds regions
    painter.setPen(QPen(Qt::black, 2));
    painter.setBrush(Qt::NoBrush);

    for (size_t i = 0; i < _monitorDimensionsScaled.size(); ++i) {
        const QColor Grey = QColor(0xDD, 0xDD, 0xDD);

        painter.drawRect(_monitorDimensionsScaled[i]);
        painter.fillRect(_monitorDimensionsScaled[i], QBrush(Grey, Qt::SolidPattern));

        if (_monitorDimensionsScaled.size() > 1 && i == 0) {
            // We only want to render the "Primary" if there are multiple windows
            QPointF textPos = QPointF(
                _monitorDimensionsScaled[i].left() + 4.0,
                _monitorDimensionsScaled[i].top() + 24.0
            );
            QFont f("Arial");
            f.setPixelSize(24);
            painter.setFont(f);
            painter.drawText(textPos, "Primary");
        }
    }

    // Draw window number(s) first for darker contrast, then window(s) over both
    // out-of-bounds and monitors
    for (unsigned int i = 0; i < _nWindows; ++i) {
        QPointF textPos = QPointF(
            _windowRendering[i].left() + 5.0,
            _windowRendering[i].bottom() - 5.0
        );
        textPos.setX(std::clamp(textPos.x(), 0.0, _monitorWidgetSize.width() - 10));
        textPos.setY(std::clamp(textPos.y(), 20.0, _monitorWidgetSize.height()));
        painter.drawText(textPos, QString::number(i + 1));
    }

    //
    // Paint window
    for (unsigned int i = 0; i < _nWindows; ++i) {
        painter.setPen(QPen(_colorsForWindows[i], 1));
        painter.drawRect(_windowRendering[i]);
        
        QColor fillColor = _colorsForWindows[i];
        fillColor.setAlpha(WindowOpacity);
        painter.fillRect(_windowRendering[i], QBrush(fillColor, Qt::SolidPattern));
    }
}

void MonitorBox::windowDimensionsChanged(unsigned int mIdx, unsigned int wIdx,
                                         const QRectF& newDimensions)
{
    _windowRendering[wIdx] = {
        _monitorDimensionsScaled[mIdx].x() + newDimensions.left() * _monitorScaleFactor,
        _monitorDimensionsScaled[mIdx].y() + newDimensions.top() * _monitorScaleFactor,
        newDimensions.width() * _monitorScaleFactor,
        newDimensions.height() * _monitorScaleFactor
    };
    update();
}

void MonitorBox::mapMonitorResolutionToWidgetCoordinates(QRectF monitorArrangement) {
    float aspectRatio = monitorArrangement.width() / monitorArrangement.height();

    std::vector<QSizeF> offsets =
        aspectRatio >= 1.f ?
        computeScaledResolutionLandscape(monitorArrangement, monitorArrangement.width()) :
        computeScaledResolutionPortrait(monitorArrangement, monitorArrangement.height());
    
    for (size_t i = 0; i < _monitorResolutions.size(); ++i) {
        _monitorDimensionsScaled.emplace_back(
            offsets[i].width(),
            offsets[i].height(),
            _monitorResolutions[i].width() * _monitorScaleFactor,
            _monitorResolutions[i].height() * _monitorScaleFactor
        );
    }
}

std::vector<QSizeF> MonitorBox::computeScaledResolutionLandscape(
                                                                QRectF monitorArrangement,
                                                                           float maxWidth)
{
    std::vector<QSizeF> offsets;

    float marginWidget = _monitorWidgetSize.width() * MarginFractionOfWidgetSize;
    float virtualWidth =
        _monitorWidgetSize.width() * (1.f - MarginFractionOfWidgetSize * 2.f);
    _monitorScaleFactor = virtualWidth / maxWidth;

    float aspectRatio = monitorArrangement.width() / monitorArrangement.height();
    float newHeight = virtualWidth / aspectRatio;

    for (const QRect& res : _monitorResolutions) {
        float x = marginWidget + (res.x() - monitorArrangement.x()) * _monitorScaleFactor;
        float y =
            marginWidget +
            (_monitorWidgetSize.height() - newHeight - marginWidget) / 4.f +
            (res.y() - monitorArrangement.y()) * _monitorScaleFactor;
        offsets.emplace_back(x, y);
    }

    return offsets;
}

std::vector<QSizeF> MonitorBox::computeScaledResolutionPortrait(QRectF monitorArrangement,
                                                                float maxHeight)
{
    std::vector<QSizeF> offsets;
    
    float marginWidget = _monitorWidgetSize.height() * MarginFractionOfWidgetSize;
    float virtualHeight =
        _monitorWidgetSize.height() * (1.f - MarginFractionOfWidgetSize * 2.f);
    _monitorScaleFactor = virtualHeight / maxHeight;
    
    float aspectRatio = monitorArrangement.width() / monitorArrangement.height();
    float newWidth = virtualHeight * aspectRatio;

    for (const QRect& res : _monitorResolutions) {
        float x = marginWidget +
            (_monitorWidgetSize.width() - newWidth - marginWidget) / 4.f +
            (res.x() - monitorArrangement.x()) * _monitorScaleFactor;
        float y = marginWidget + (res.y() - monitorArrangement.y()) * _monitorScaleFactor;
        offsets.emplace_back(x, y);
    }
    
    return offsets;
}

void MonitorBox::setNumWindowsDisplayed(unsigned int nWindows) {
    if (_nWindows != nWindows) {
        _nWindows = nWindows;
        update();
    }
}
