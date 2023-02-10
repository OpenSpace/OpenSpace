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

#ifndef __OPENSPACE_UI_LAUNCHER___ORIENTATIONDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___ORIENTATIONDIALOG___H__

#include <QDialog>

#include <sgct/math.h>

class QLineEdit;
class QWidget;

class OrientationDialog final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for OrientationDialog object which contains the input text boxes for
     * orientation x,y,z values
     *
     * \param orientation x,y,z angles in degrees contained in sgct::quat object
     * \param parent pointer to Qt QWidget parent object
     */
    OrientationDialog(sgct::quat& orientation, QWidget* parent);

private:
    void ok();
    
    QLineEdit* _linePitch = nullptr;
    QLineEdit* _lineRoll = nullptr;
    QLineEdit* _lineYaw = nullptr;
    sgct::quat& _orientationValue;
};

#endif // __OPENSPACE_UI_LAUNCHER___ORIENTATIONDIALOG___H__
