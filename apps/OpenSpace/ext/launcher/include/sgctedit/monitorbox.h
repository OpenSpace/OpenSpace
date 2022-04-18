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

#include <QColor>
#include <array>
#include <vector>

class MonitorBox : public QWidget {
Q_OBJECT
public:
    /**
     * Constructor for MonitorBox class, which displays the system's monitor(s),
     * their relative position and size, and window(s) that they contain
     *
     * \param widgetDims The size of the display widget in pixels, stored in QRect
     * \param monitorResolution A vector containing the monitor's maximum display
     *                          size in pixels in a QRect object
     * \param nWindows The current number of windows that has been selected by the user
     * \param winColors An array of QColor objects for window colors. The indexing of
     *                  this array matches the window indexing used elsewhere in the
     *                  class. This allows for a unique color for each window.
     */
    MonitorBox(QRect widgetDims, std::vector<QRect> monitorResolution,
        unsigned int nWindows, const std::array<QColor, 4>& winColors);

    /**
     * Sets the number of windows to be displayed
     *
     * \param nWindows Number of windows to be displayed
     */
    void setNumWindowsDisplayed(unsigned int nWindows);
    
public slots:
    /**
     * Called when window dimensions or monitor location have changed, requiring redraw.
     * This will also map the window resolution into the scaled resolution of the display
     * widget.
     *
     * \param mIdx The zero-based monitor index (primary monitor is 0)
     * \param wIdx The zero-based window index
     * \param newDimensions Dimensions (pixels) of window to be mapped in QRect
     */
    void windowDimensionsChanged(unsigned int mIdx, unsigned int wIdx,
        const QRectF& newDimensions);

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    void determineMonitorArrangement();
    void mapMonitorResolutionToWidgetCoordinates();
    std::vector<QSizeF> computeScaledResolutionLandscape(float maxWidth);
    std::vector<QSizeF> computeScaledResolutionPortrait(float maxHeight);

    QRectF _monitorWidgetSize;
    float _monitorArrangementAspectRatio = 1.f;
    QSizeF _monitorArrangementDimensions = { 0.0, 0.0 };
    const std::vector<QRect> _monitorResolution;
    std::vector<QRectF> _monitorDimensionsScaled;
    QRectF _negativeCorrectionOffsets = { 0.f, 0.f, 0.f, 0.f };
    std::array<QRectF, 4> _windowRendering = {
        QRectF{ 0.f, 0.f, 0.f, 0.f },
        QRectF{ 0.f, 0.f, 0.f, 0.f },
        QRectF{ 0.f, 0.f, 0.f, 0.f },
        QRectF{ 0.f, 0.f, 0.f, 0.f }
    };
    unsigned int _nWindows = 1;
    const std::array<QColor, 4> _colorsForWindows;
    float _monitorScaleFactor = 1.0;
};

#endif // __OPENSPACE_UI_LAUNCHER___MONITORBOX___H__
