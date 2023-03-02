/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
        QRectF res = QRectF(0.f, 0.f, 0.f, 0.f);
        for (const QRect& m : monitorResolutions) {
            res |= m;
        }
        return res;
    }
} // namespace

MonitorBox::MonitorBox(QRect widgetDims, const std::vector<QRect>& monitorResolutions,
                       unsigned int nWindows, const std::array<QColor, 4>& windowColors,
                       QWidget* parent)
    : QWidget(parent)
    , _nWindows(nWindows)
    , _colorsForWindows(windowColors)
{
    setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    
    QRectF monitorArrangement = computeUnion(monitorResolutions);
    
    const float aspectRatio = monitorArrangement.width() / monitorArrangement.height();
    if (aspectRatio > 1.0) {
        float borderMargin = 2.f * MarginFractionOfWidgetSize * widgetDims.width();
        widgetDims.setHeight(widgetDims.width() / aspectRatio + borderMargin);
    }
    else {
        float borderMargin = 2.f * MarginFractionOfWidgetSize * widgetDims.height();
        widgetDims.setWidth(widgetDims.height() * aspectRatio + borderMargin);
    }
    setFixedSize(widgetDims.width(), widgetDims.height());
    
    //
    // Map monitor resolution to widget coordinates
    std::vector<QSizeF> offsets =
        aspectRatio >= 1.f ?
        computeScaledResolutionLandscape(monitorArrangement, monitorResolutions) :
        computeScaledResolutionPortrait(monitorArrangement, monitorResolutions);

    for (size_t i = 0; i < monitorResolutions.size(); ++i) {
        _monitorDimensionsScaled.emplace_back(
            offsets[i].width(),
            offsets[i].height(),
            monitorResolutions[i].width() * _monitorScaleFactor,
            monitorResolutions[i].height() * _monitorScaleFactor
        );
    }
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
    for (int i = 0; i < _nWindows; ++i) {
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
    for (int i = 0; i < _nWindows; ++i) {
        QPointF p = QPointF(
            _windowRendering[i].left() + 5.0,
            _windowRendering[i].bottom() - 5.0
        );
        p.setX(std::clamp(p.x(), 0.0, static_cast<double>(size().width()) - 10.0));
        p.setY(std::clamp(p.y(), 20.0, static_cast<double>(size().height())));
        painter.drawText(p, QString::number(i + 1));
    }

    //
    // Paint window
    for (int i = 0; i < _nWindows; ++i) {
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
    _windowRendering[wIdx] = QRectF(
        _monitorDimensionsScaled[mIdx].x() + newDimensions.left() * _monitorScaleFactor,
        _monitorDimensionsScaled[mIdx].y() + newDimensions.top() * _monitorScaleFactor,
        newDimensions.width() * _monitorScaleFactor,
        newDimensions.height() * _monitorScaleFactor
    );
    update();
}

std::vector<QSizeF> MonitorBox::computeScaledResolutionLandscape(QRectF arrangement,
                                                    const std::vector<QRect>& resolutions)
{
    std::vector<QSizeF> offsets;

    float marginWidget = size().width() * MarginFractionOfWidgetSize;
    float virtualWidth = size().width() * (1.f - MarginFractionOfWidgetSize * 2.f);
    _monitorScaleFactor = virtualWidth / arrangement.width();

    const float aspectRatio = arrangement.width() / arrangement.height();
    const float newHeight = virtualWidth / aspectRatio;

    for (const QRect& res : resolutions) {
        float x = marginWidget + (res.x() - arrangement.x()) * _monitorScaleFactor;
        float y = marginWidget + (size().height() - newHeight - marginWidget) / 4.f +
            (res.y() - arrangement.y()) * _monitorScaleFactor;
        offsets.emplace_back(x, y);
    }

    return offsets;
}

std::vector<QSizeF> MonitorBox::computeScaledResolutionPortrait(QRectF arrangement,
                                                    const std::vector<QRect>& resolutions)
{
    std::vector<QSizeF> offsets;
    
    float marginWidget = size().height() * MarginFractionOfWidgetSize;
    float virtualHeight = size().height() * (1.f - MarginFractionOfWidgetSize * 2.f);
    _monitorScaleFactor = virtualHeight / arrangement.height();
    
    const float aspectRatio = arrangement.width() / arrangement.height();
    const float newWidth = virtualHeight * aspectRatio;

    for (const QRect& res : resolutions) {
        float x = marginWidget + (size().width() - newWidth - marginWidget) / 4.f +
            (res.x() - arrangement.x()) * _monitorScaleFactor;
        float y = marginWidget + (res.y() - arrangement.y()) * _monitorScaleFactor;
        offsets.emplace_back(x, y);
    }
    
    return offsets;
}

void MonitorBox::nWindowsDisplayedChanged(int nWindows) {
    if (_nWindows != nWindows) {
        _nWindows = nWindows;
        update();
    }
}
