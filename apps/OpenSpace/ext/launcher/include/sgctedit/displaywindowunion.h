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

#ifndef __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__
#define __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__

#include <QWidget>

#include <sgct/config.h>
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
     * including monitorBox, multiple WindowControl columns, and additional controls.
     *
     * \param monitorSizeList A vector containing QRect objects containing pixel dims of
     *        each monitor
     * \param nMaxWindows The maximum number of windows allowed (depends on the number of
     *        monitors in the system)
     * \param resetToDefault If set to true, all display and window settings will be
     *        initialized to their default values
     * \param parent The parent to which this widget belongs
     */
    DisplayWindowUnion(const std::vector<QRect>& monitorResolutions,
        int nMaxWindows, QWidget* parent = nullptr);

    void initialize(const std::vector<QRect>& monitorSizeList,
        const sgct::config::Cluster& cluster);

    void applyWindowSettings(std::vector<sgct::config::Window>& windows);

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

signals:
    /**
     * This signal is emitted when a window has changed.
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
    void updateWindows();

    unsigned int _nWindowsDisplayed = 0;

    std::vector<WindowControl*> _windowControls;
    QPushButton* _addWindowButton = nullptr;
    QPushButton* _removeWindowButton = nullptr;
    std::vector<QFrame*> _frameBorderLines;
};

#endif // __OPENSPACE_UI_LAUNCHER___DISPLAYWINDOWUNION___H__
