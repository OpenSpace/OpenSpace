/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <ghoul/misc/assert.h>
#include "windowcolors.h"
#include <QPainter>

MonitorBox::MonitorBox(QRect widgetSize, const std::vector<QRect>& monitorResolutions,
                       QWidget* parent)
    : QWidget(parent)
{
    constexpr float MarginFractionWidgetSize = 0.02f;

    //
    // Calculate the collective size of the monitors
    QRectF monitorArrangement = QRectF(0.f, 0.f, 0.f, 0.f);
    for (const QRect& m : monitorResolutions) {
        monitorArrangement |= m;
    }

    //
    // Set the size of the widget according to the aspect ratio of the total size
    const float aspectRatio = monitorArrangement.width() / monitorArrangement.height();
    if (aspectRatio > 1.f) {
        const float borderMargin = 2.f * MarginFractionWidgetSize * widgetSize.width();
        widgetSize.setHeight(widgetSize.width() / aspectRatio + borderMargin);
    }
    else {
        const float borderMargin = 2.f * MarginFractionWidgetSize * widgetSize.height();
        widgetSize.setWidth(widgetSize.height() * aspectRatio + borderMargin);
    }
    setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    setFixedSize(widgetSize.width(), widgetSize.height());
    
    //
    // Map monitor resolution to widget coordinates
    const float margin = size().width() * MarginFractionWidgetSize;
    const float virtualWidth = size().width() * (1.f - MarginFractionWidgetSize * 2.f);
    _monitorScaleFactor = virtualWidth / monitorArrangement.width();

    const float newHeight = virtualWidth / aspectRatio;
    for (const QRect& res : monitorResolutions) {
        const float x = margin + (res.x() - monitorArrangement.x()) * _monitorScaleFactor;
        const float y = margin + (size().height() - newHeight - margin) / 4.f +
            (res.y() - monitorArrangement.y()) * _monitorScaleFactor;
        const float width = res.width() * _monitorScaleFactor;
        const float height = res.height() * _monitorScaleFactor;
        _monitorDimensionsScaled.emplace_back(x, y, width, height);
    }
}

void MonitorBox::windowDimensionsChanged(unsigned int monitorIdx, unsigned int windowIdx,
                                         const QRectF& dimension)
{
    if (windowIdx >= _windowRendering.size()) {
        // We were told about a window change for a window we haven't created yet, so
        // initialize the vector so that we have enough windows available
        _windowRendering.resize(windowIdx + 1, QRectF(0.f, 0.f, 0.f, 0.f));
    }

    _windowRendering[windowIdx] = QRectF(
        _monitorDimensionsScaled[monitorIdx].x() + dimension.left() * _monitorScaleFactor,
        _monitorDimensionsScaled[monitorIdx].y() + dimension.top() * _monitorScaleFactor,
        dimension.width() * _monitorScaleFactor,
        dimension.height() * _monitorScaleFactor
    );
    update();
}

void MonitorBox::nWindowsDisplayedChanged(int nWindows) {
    if (_nWindows != nWindows) {
        _nWindows = nWindows;
        update();
    }
}

void MonitorBox::paintEvent(QPaintEvent*) {
    QPainter painter(this);

    //
    // Draw widget border
    constexpr double RectRadius = 10.0;
    painter.setPen(QPen(Qt::gray, 4));
    painter.drawRoundedRect(0, 0, width() - 1, height() - 1, RectRadius, RectRadius);

    //
    // Draw window out-of-bounds region(s) first
    for (int i = 0; i < _nWindows; i++) {
        painter.setBrush(Qt::BDiagPattern);
        painter.setPen(QPen(colorForWindow(i), 0));
        painter.drawRect(_windowRendering[i]);
    }

    //
    // Draw & fill monitors over the out-of-bounds regions
    painter.setPen(QPen(Qt::black, 2));
    painter.setBrush(Qt::NoBrush);

    for (size_t i = 0; i < _monitorDimensionsScaled.size(); i++) {
        constexpr QColor Grey = QColor(221, 221, 221);

        painter.drawRect(_monitorDimensionsScaled[i]);
        painter.fillRect(_monitorDimensionsScaled[i], QBrush(Grey, Qt::SolidPattern));

        // We only want to render the "Primary" if there are multiple windows
        if (_monitorDimensionsScaled.size() > 1 && i == 0) {
            const QPointF textPos = QPointF(
                _monitorDimensionsScaled[i].left() + 4.0,
                _monitorDimensionsScaled[i].top() + 24.0
            );
            QFont f = QFont("Arial");
            f.setPixelSize(18);
            painter.setFont(f);
            painter.drawText(textPos, "Primary");
        }
    }

    //
    // Draw window number(s) first for darker contrast, then window(s) over both
    // out-of-bounds and monitors
    for (int i = 0; i < _nWindows; i++) {
        const double x = _windowRendering[i].left() + 5.0;
        const double y = _windowRendering[i].bottom() - 5.0;
        const QPointF p = QPointF(
            std::clamp(x, 0.0, static_cast<double>(size().width()) - 10.0),
            std::clamp(y, 20.0, static_cast<double>(size().height()))
        );
        QFont f = QFont("Arial");
        f.setPixelSize(18);
        painter.setFont(f);
        painter.drawText(p, QString::number(i + 1));
    }

    //
    // Paint window
    for (int i = 0; i < _nWindows; i++) {
        constexpr int WindowOpacity = 170;

        QColor color = colorForWindow(i);
        painter.setPen(QPen(color, 1));
        painter.drawRect(_windowRendering[i]);
        
        QColor fillColor = color;
        fillColor.setAlpha(WindowOpacity);
        painter.fillRect(_windowRendering[i], QBrush(fillColor, Qt::SolidPattern));
    }
}
