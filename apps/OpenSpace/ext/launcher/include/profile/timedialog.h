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

#ifndef __OPENSPACE_UI_LAUNCHER___TIMEDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___TIMEDIALOG___H__

#include <QDialog>

#include <openspace/scene/profile.h>

class QCheckBox;
class QDateTimeEdit;
class QLabel;
class QLineEdit;
class QTabWidget;

class TimeDialog final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for ostime class.
     *
     * \param parent Pointer to parent Qt widget
     * \param time The #openspace::Profile::Time object containing all data of the new or
     *        imported profile.
     */
    TimeDialog(QWidget* parent, std::optional<openspace::Profile::Time>* time);

private slots:
    void approved();

private:
    void createWidgets();

    std::optional<openspace::Profile::Time>* _time = nullptr;
    openspace::Profile::Time _timeData;

    QTabWidget* _tabWidget = nullptr;
    QLabel* _absoluteLabel = nullptr;
    QDateTimeEdit* _absoluteEdit = nullptr;
    QLabel* _relativeLabel = nullptr;
    QLineEdit* _relativeEdit = nullptr;
    QCheckBox* _startPaused = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___TIMEDIALOG___H__
