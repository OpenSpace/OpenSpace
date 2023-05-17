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

#ifndef __OPENSPACE_UI_LAUNCHER___SETTINGSWIDGET___H__
#define __OPENSPACE_UI_LAUNCHER___SETTINGSWIDGET___H__

#include <QComboBox>
#include <QVBoxLayout>
#include <QWidget>

#include <sgct/math.h>

class QCheckBox;
class QLabel;

class SettingsWidget final : public QWidget {
Q_OBJECT
public:
    /**
     * Constructor for Orientation class, which manages the overall control layout
     * including monitorBox, multiple WindowControl columns, and additional controls
     */
    SettingsWidget(sgct::quat orientation, QWidget* parent = nullptr);
   
    /**
     * Gets the user-provided x,y,z orientation values (degrees)
     *
     * \return the orientation angles provided in sgct::quat object
     */
    sgct::quat orientation() const;
   
    /**
     * Gets the value for if VSync is enabled
     *
     * \return true if the VSync option is checked/enabled
     */
    bool vsync() const;

    /**
     * Gets whether the UI should be restricted to the first window
     * 
     * \return true if the UI should only be on the first window
     */
    bool showUiOnFirstWindow() const;

    /**
     * Sets the value of the checkbox for putting the GUI only on the first window.
     * If this is enabled, then the first window will draw2D but not draw3D. All
     * subsequent windows will be the opposite of this.
     * 
     * \param setUiOnFirstWindow boolean value, if set true then the GUI will only
     *                           be on the first window
     */
    void setShowUiOnFirstWindow(bool setUiOnFirstWindow);

    /**
     * Sets value for whether or not the checkbox for having the UI only on the first
     * window is enabled. This checkbox should only be clickable if the number of
     * windows is 2 or more. 
     */
    void setEnableShowUiOnFirstWindowCheckbox(bool enable);

    /**
     * Gets the value of the selection for which display first window should mirror if
     * the option to show the Ui in the first window is enabled. Note that this will
     * return a value even if the checkbox is not enabled.
     * 
     * \return -1 if in a disabled state (e.g. when showUiOnFirstWindow() returns false)
     *          0 if no window is being mirrored (only the UI will appear in first
     *            window, with no 3D rendering)
     *          1 if the second window's viewport is mirrored in the first window
     *          2 if the third window's viewport is mirrored in the first window
     *          3 if the fourth window's viewport is mirrored in the first window
     */
    int graphicsSelectionForShowUiOnFirstWindow() const;

    /**
     * Sets the value of the checkbox for enabling VSync.
     * 
     * \param enableVsync boolean value, if set true then VSync is enabled
     */
    void setVsync(bool enableVsync);

    /**
     * Called when the number of windows that should be displayed changes.
     * 
     * \param newCount The new number of windows included
     */
    void nWindowsDisplayedChanged(int newCount);

private:
    sgct::quat _orientationValue = sgct::quat(0.f, 0.f, 0.f, 0.f);
    QCheckBox* _checkBoxVsync = nullptr;
    QCheckBox* _showUiOnFirstWindow = nullptr;
    QComboBox* _firstWindowGraphicsSelection = nullptr;
    QBoxLayout* _firstWindowSelectionLayout = nullptr;
    int _stateOfUiOnFirstWindowPreviousCount = 1;
    bool _stateOfUiOnFirstWindowWhenDisabled = false;

    void showUiOnFirstWindowClicked(bool checked);
};

#endif // __OPENSPACE_UI_LAUNCHER___SETTINGSWIDGET___H__
