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
     * Constructor for MonitorBox class, which displays the system's monitor(s), their
     * relative position and size, and window(s) that they contain.
     *
     * \param widgetSize The size of the display widget in pixels
     * \param monitorResolution A vector containing each monitor's maximum display size in
     *        pixels
     * \param parent The parent which to which this MonitorBox belongs
     */
    MonitorBox(QRect widgetSize, const std::vector<QRect>& monitorResolutions,
        QWidget* parent = nullptr);

public slots:
    /**
     * Called when window dimensions or monitor location have changed, requiring redraw.
     * This will also map the window resolution into the scaled resolution of the display
     * widget.
     *
     * \param monitorIdx The zero-based monitor index (primary monitor is 0)
     * \param windowIdx The zero-based window index
     * \param dimension Dimensions (pixels) of window to be mapped in QRect
     */
    void windowDimensionsChanged(unsigned int monitorIdx, unsigned int windowIdx,
        const QRectF& dimension);

    /**
     * Called when the number of windows that should be displayed changes.
     *
     * \param nWindows The new number of windows included
     */
    void nWindowsDisplayedChanged(int nWindows);

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    std::vector<QRectF> _monitorDimensionsScaled;
    std::vector<QRectF> _windowRendering;
    int _nWindows = 0;
    float _monitorScaleFactor = 1.f;
};

#endif // __OPENSPACE_UI_LAUNCHER___MONITORBOX___H__
