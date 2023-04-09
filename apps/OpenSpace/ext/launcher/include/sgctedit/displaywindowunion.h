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

#ifndef __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__
#define __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__

#include <QWidget>

#include <vector>

class QFrame;
class QPushButton;
class QVBoxLayout;
class WindowControl;

class DisplayWindowUnion final : public QWidget {
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
     * \param resetToDefault If set to true, all display and window settings will be
     *                       initialized to their default values.
     * \param parent The parent to which this widget belongs
     */
    DisplayWindowUnion(const std::vector<QRect>& monitorSizeList,
        int nMaxWindows, const std::array<QColor, 4>& windowColors, bool resetToDefault,
        QWidget* parent = nullptr);
    
    /**
     * Returns a vector of pointers to the WindowControl objects for all visible windows.
     *
     * \return vector of pointers of WindowControl objects
     */
    std::vector<WindowControl*> activeWindowControls() const;

    /**
     * Returns a vector of pointers to the WindowControl objects for all windows, whether
     * they are visible or not.
     *
     * \return vector of pointers of all WindowControl objects
     */
    std::vector<WindowControl*>& windowControls();

    /**
     * When called will add a new window to the set of windows, which will, in turn, send
     * out all of the corresponding signals described below.
     */
    void addWindow();

    /**
     * When called will remove the last window from the set of windows, which will, in
     * turn, send out all of the corresponding signals described below.
     */
    void removeWindow();

    /**
     * Returns the number of windows that are displayed (there can be more window
     * objects than are currently displayed).
     *
     * \return the number of displayed windows in the current configuration
     */
    unsigned int numWindowsDisplayed() const;

signals:
    /**
     * This signal is emitted when a windowhas changed.
     * 
     * \param monitorIndex The 0-based index of the monitor to which the window belongs to
     * \param windowIndex The 0-based index of the window that was changed
     * \param newDimensions The pixel sizes of the window after the change
     */
    void windowChanged(int monitorIndex, int windowIndex, const QRectF& newDimensions);

    /**
     * This signal is emitted when the total number of windows has changed.
     * 
     * \param newCount The new total number of windows
     */
    void nWindowsChanged(int newCount);

private:
    void createWidgets(int nMaxWindows, std::vector<QRect> monitorResolutions,
        std::array<QColor, 4> windowColors, bool resetToDefault);
    void showWindows();

    unsigned int _nWindowsDisplayed = 0;

    std::vector<WindowControl*> _windowControl;
    QPushButton* _addWindowButton = nullptr;
    QPushButton* _removeWindowButton = nullptr;
    std::vector<QFrame*> _frameBorderLines;
};

#endif // __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__
