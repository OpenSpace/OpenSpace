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

#ifndef __OPENSPACE_UI_LAUNCHER___MONITORBOX___H__
#define __OPENSPACE_UI_LAUNCHER___MONITORBOX___H__

#include <QWidget>

#include "windowcontrol.h"
#include <QColor>
#include <QIntValidator>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPoint>
#include <QVector>
#include <algorithm>
#include <vector>
#include <iostream>

class MonitorBox : public QWidget {
Q_OBJECT
public:
    MonitorBox(QRect widgetDims, std::vector<QRect> monitorResolution,
        unsigned int nWindows, QString* winColors);
    void mapMonitorResolutionToWidgetCoordinates();
    void mapWindowResolutionToWidgetCoordinates(unsigned int mIdx, unsigned int wIdx,
        const QRectF& w);
    void setNumWindowsDisplayed(unsigned int nWindows);
    void windowDimensionsChanged(unsigned int monitorIdx, unsigned int windowIdx,
            const QRectF& newDimensions);

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    void paintWidgetBorder(QPainter& painter, const int width, const int height);
    void paintMonitorBackgrounds(QPainter& painter);
    void paintWindow(QPainter& painter, const size_t winIdx);
    void paintWindowBeyondBounds(QPainter& painter, const unsigned int winIdx);
    void paintWindowNumber(QPainter& painter, const unsigned int winIdx);
    void setPenSpecificToWindow(QPainter& painter, const unsigned int windowIdx,
        bool visibleBorder);
    void computeScaledResolution_landscape(const float aspectRatio, const float maxWidth);
    void computeScaledResolution_portrait(const float aspectRatio, const float maxHeight);

    unsigned int _maxNumMonitors = 2;
    QRectF _monitorWidgetSize;
    QRectF _monitorBoundaryRect;
    unsigned int _nMonitors = 1;
    std::vector<QRect> _monitorResolution;
    std::vector<QRectF> _monitorDimensionsScaled;
    std::vector<QRectF> _windowResolutions;
    std::vector<QRectF> _windowRendering = {
        {0.f, 0.f, 0.f, 0.f},
        {0.f, 0.f, 0.f, 0.f},
        {0.f, 0.f, 0.f, 0.f},
        {0.f, 0.f, 0.f, 0.f}
    };
    unsigned int _nWindows = 1;
    QString* _colorsForWindows;
    int _alphaWindowOpacity = 170;
    float _monitorScaleFactor = 1.0;
    bool _showLabel = false;
    float _marginWidget = 5.0;
    std::vector<QSizeF> _monitorOffsets;
};

#endif // __OPENSPACE_UI_LAUNCHER___MONITORBOX___H__
