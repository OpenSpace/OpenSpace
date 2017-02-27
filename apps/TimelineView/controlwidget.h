/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_APP_TIMELINEVIEW___CONTROLWIDGET___H__
#define __OPENSPACE_APP_TIMELINEVIEW___CONTROLWIDGET___H__

#include <QWidget>

class QComboBox;
class QLabel;
class QPushButton;
class QSlider;

class ControlWidget : public QWidget {
Q_OBJECT
public:
    ControlWidget(QWidget* parent);

    void update(QString currentTime, QString currentDelta);

    void socketConnected();
    void socketDisconnected();

signals:
    void scriptActivity(QString script);

private slots:
    void onValueChange();
    void onDateChange();
    void onFocusChange();
    void onPauseButton();
    void onPlayButton();
    void onFocusToTargetButton();
    void onFocusToNewHorizonsButton();

private:
    QLabel* _currentTime;
    QComboBox* _setTime;
    QLabel* _currentDelta;
    QSlider* _setDelta;
    QPushButton* _pause;
    QPushButton* _play;
    QComboBox* _focusNode;
    QPushButton* _setFocusToNextTarget;
    QPushButton* _setFocusToNewHorizons;
};

#endif // __OPENSPACE_APP_TIMELINEVIEW___CONTROLWIDGET___H__
