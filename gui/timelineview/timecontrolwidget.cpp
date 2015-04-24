/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include "timecontrolwidget.h"

#include <QComboBox>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSlider>

TimeControlWidget::TimeControlWidget(QWidget* parent)
    : QWidget(parent)
    , _currentTime(new QLabel("Current Time"))
    , _setTime(new QComboBox)
    , _currentDelta(new QLabel("Current Delta"))
    , _setDelta(new QSlider(Qt::Horizontal))
    , _rewind(new QPushButton("<<"))
    , _pause(new QPushButton("||"))
    , _play(new QPushButton("|>"))
    , _forward(new QPushButton(">>"))
{
    _setDelta->setMinimum(-100);
    _setDelta->setMaximum(100);
    _setDelta->setValue(0);
    QObject::connect(
        _setDelta,
        SIGNAL(valueChanged(int)),
        this,
        SLOT(onValueChange())
    );

    QObject::connect(
        _rewind,
        SIGNAL(clicked()),
        this,
        SLOT(onRewindButton())
    );

    QObject::connect(
        _pause,
        SIGNAL(clicked()),
        this,
        SLOT(onPauseButton())
    );

    QObject::connect(
        _play,
        SIGNAL(clicked()),
        this,
        SLOT(onPlayButton())
    );

    QObject::connect(
        _forward,
        SIGNAL(clicked()),
        this,
        SLOT(onForwardButton())
    );

    QGridLayout* layout = new QGridLayout;

    layout->addWidget(_currentTime, 0, 0);
    layout->addWidget(_setTime, 0, 1);
    layout->addWidget(_currentDelta, 1, 0);
    layout->addWidget(_setDelta, 2, 0, 1, 2);

    QWidget* controlContainer = new QWidget;
    QHBoxLayout* controlContainerLayout = new QHBoxLayout;
    controlContainerLayout->addWidget(_rewind);
    controlContainerLayout->addWidget(_pause);
    controlContainerLayout->addWidget(_play);
    controlContainerLayout->addWidget(_forward);
    controlContainer->setLayout(controlContainerLayout);
    layout->addWidget(controlContainer, 3, 0, 1, 2);

    setLayout(layout);
}

void TimeControlWidget::update(QString currentTime, QString currentDelta) {
    _currentTime->setText(currentTime);
    _currentDelta->setText(currentDelta);
}

void TimeControlWidget::onValueChange() {
    QString script = "openspace.time.setDeltaTime(" + QString::number(_setDelta->value()) + ");";
    emit scriptActivity(script);
}

void TimeControlWidget::onRewindButton() {
    QString script = "openspace.time.setDeltaTime(-openspace.time.deltaTime());";
    emit scriptActivity(script);
}

void TimeControlWidget::onPauseButton() {
    QString script = "openspace.time.setPause(true);";
    emit scriptActivity(script);
}

void TimeControlWidget::onPlayButton() {
    QString script = "openspace.time.setPause(false);";
    emit scriptActivity(script);
}

void TimeControlWidget::onForwardButton() {
    QString script = "openspace.time.setDeltaTime(-openspace.time.deltaTime());";
    emit scriptActivity(script);

}
