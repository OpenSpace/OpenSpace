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

#ifndef __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__
#define __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__

#include <QWidget>

#include "monitorbox.h"
#include "windowcontrol.h"
#include <memory>
#include <vector>

class QFrame;
class QVBoxLayout;

class DisplayWindowUnion : public QWidget {
Q_OBJECT
public:
    /**
     * Constructor for DisplayWindowUnion class, which manages the overall control layout
     * including monitorBox, multiple WindowControl columns, and additional controls
     *
     * \param monitorSizeList A vector containing QRect objects containing pixel dims
     *                        of each monitor
     * \param nMaxWindows The maximum number of windows allowed (depends on the number
     *                    of monitors in the system)
     * \param winColors An array of QColor objects for window colors. The indexing of
     *                  this array matches the window indexing used elsewhere in the
     *                  class. This allows for a unique color for each window.
     * \param parent The parent to which this widget belongs
     */
    DisplayWindowUnion(const std::vector<QRect>& monitorSizeList,
        unsigned int nMaxWindows, const std::array<QColor, 4>& windowColors,
        QWidget* parent = nullptr);
    
    /**
     * Returns a vector of pointers to the WindowControl objects for all visible windows.
     *
     * \return vector of pointers of WindowControl objects
     */
    std::vector<WindowControl*> windowControls() const;
    
signals:
    void windowChanged(int monitorIndex, int windowIndex, const QRectF& newDimensions);
    void nWindowsChanged(int newCount);

public slots:
    void addWindow();
    void removeWindow();

private:
    void createWidgets();
    void showWindows();

    const std::vector<QRect>& _monitorResolutions;
    const unsigned int _nMaxWindows = 3;
    const std::array<QColor, 4>& _windowColors;

    unsigned int _nWindowsDisplayed = 0;
    std::vector<WindowControl*> _windowControl;
    QPushButton* _addWindowButton = nullptr;
    QPushButton* _removeWindowButton = nullptr;
    unsigned int _monitorIdx = 0;
    std::vector<QFrame*> _frameBorderLines;
};

#endif // __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__
