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
#ifndef __OPENSPACE_UI_LAUNCHER___DISPLAY___H__
#define __OPENSPACE_UI_LAUNCHER___DISPLAY___H__

#include <QWidget>

#include "windowcontrol.h"
#include "monitorbox.h"
#include <QCheckBox>
#include <QComboBox>
#include <QIntValidator>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPoint>
#include <QPushButton>
#include <QTextBrowser>
#include <QVector>
#include <vector>

class Display : public QWidget {
Q_OBJECT
public:
    /**
     * Constructor for Display class, which manages the overall control layout including
     * monitorBox, multiple WindowControl columns, and additional controls
     *
     * \param monitorRenderBox pointer to the MonitorBox object
     * \param monitorSizeList A vector containing QRect objects containing pixel dims
     *                        of each monitor
     * \param nMaxWindows The maximum number of windows allowed (depends on the number
     *                    of monitors in the system)
     * \param winColors An array of QColor objects for window colors. The indexing of
     *                  this array matches the window indexing used elsewhere in the
     *                  class. This allows for a unique color for each window.
    */
    Display(std::shared_ptr<MonitorBox> monitorRenderBox,
        std::vector<QRect>& monitorSizeList, unsigned int nMaxWindows,
        const std::array<QColor, 4>& winColors);
    /**
     * Returns a vector of pointers to all WindowControl objects for all windows
     *
     * \return vector of pointers of WindowControl objects
    */
    std::vector<std::shared_ptr<WindowControl>> windowControls() const;
    /**
     * Returns the current number of windows
     *
     * \return the currently-selected number of windows in unsigned int
    */
    unsigned int nWindows() const;

private slots:
    void addWindow();
    void removeWindow();

private:
    void initializeWindowControl();
    void initializeLayout();
    void showWindows();
    std::shared_ptr<MonitorBox> _monBox;
    std::vector<QRect>& _monitorResolutions;
    QRect _widgetDims = {0, 0, 400, 400};
    unsigned int _nWindowsAllocated = 0;
    unsigned int _nWindowsDisplayed = 0;
    unsigned int _nMaxWindows = 3;
    const std::array<QColor, 4>& _winColors;
    std::vector<std::shared_ptr<WindowControl>> _windowControl;
    QPushButton* _addWindowButton = nullptr;
    QPushButton* _removeWindowButton = nullptr;
    unsigned int _monitorIdx = 0;
    std::vector<QVBoxLayout*> _winCtrlLayouts;
    std::vector<QWidget*> _layoutWindowWrappers;
    std::vector<QFrame*> _frameBorderLines;
    QFrame* _borderFrame = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___DISPLAY___H__
