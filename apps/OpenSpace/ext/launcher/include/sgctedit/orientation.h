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

#ifndef __OPENSPACE_UI_LAUNCHER___ORIENTATION___H__
#define __OPENSPACE_UI_LAUNCHER___ORIENTATION___H__

#include <QWidget>

#include <sgctedit/orientationdialog.h>
#include <sgct/config.h>
#include <sgct/math.h>
#include <QCheckBox>
#include <QLayout>
#include <QPushButton>

class Orientation : public QWidget {
Q_OBJECT
public:
    /**
     * Constructor for Orientation class, which manages the overall control layout
     * including monitorBox, multiple WindowControl columns, and additional controls
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
    Orientation();
    /**
     * Add Orientation controls to the parent layout
     *
     * \param parentLayout the layout to which the Orientation's controls will be added
    */
    void addControlsToParentLayout(QVBoxLayout* parentLayout);
    /**
     * Gets the user-provided x,y,z orientation values (degrees)
     *
     * \return the orientation angles provided in sgct::quat object
    */
    sgct::quat orientationValue() const;
    /**
     * Gets the value for if VSync is enabled
     *
     * \return true if the VSync option is checked/enabled
    */
    bool vsyncValue() const;

private slots:
    void orientationDialog();

private:
    sgct::quat _orientationValue = {0.f, 0.f, 0.f, 0.f};
    OrientationDialog _orientationDialog;
    QHBoxLayout* _layoutOrientationFull = nullptr;
    QCheckBox* _checkBoxVsync = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___ORIENTATION___H__
