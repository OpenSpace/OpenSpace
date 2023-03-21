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

private:
    sgct::quat _orientationValue = sgct::quat(0.f, 0.f, 0.f, 0.f);
    QCheckBox* _checkBoxVsync = nullptr;
    QCheckBox* _showUiOnFirstWindow = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___SETTINGSWIDGET___H__
