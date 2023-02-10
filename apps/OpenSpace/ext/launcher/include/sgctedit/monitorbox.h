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

#ifndef __OPENSPACE_UI_LAUNCHER___MONITORBOX___H__
#define __OPENSPACE_UI_LAUNCHER___MONITORBOX___H__

#include <QWidget>

#include <QColor>
#include <array>
#include <vector>

class MonitorBox final : public QWidget {
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
     * \param parent The parent which to which this MonitorBox belongs
     */
    MonitorBox(QRect widgetDims, const std::vector<QRect>& monitorResolutions,
        unsigned int nWindows, const std::array<QColor, 4>& windowColors,
        QWidget* parent = nullptr);

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

    /**
     * Called when the number of windows that should be displayed changes.
     * 
     * \param newCount The new number of windows included
     */
    void nWindowsDisplayedChanged(int newCount);

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    std::vector<QSizeF> computeScaledResolutionLandscape(QRectF arrangement,
        const std::vector<QRect>& resolutions);
    std::vector<QSizeF> computeScaledResolutionPortrait(QRectF arrangement,
        const std::vector<QRect>& resolutions);

    std::vector<QRectF> _monitorDimensionsScaled;
    std::array<QRectF, 4> _windowRendering = {
        QRectF(0.f, 0.f, 0.f, 0.f),
        QRectF(0.f, 0.f, 0.f, 0.f),
        QRectF(0.f, 0.f, 0.f, 0.f),
        QRectF(0.f, 0.f, 0.f, 0.f)
    };
    int _nWindows = 1;
    const std::array<QColor, 4> _colorsForWindows;
    float _monitorScaleFactor = 1.0;
};

#endif // __OPENSPACE_UI_LAUNCHER___MONITORBOX___H__
