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

#include "controlwidget.h"

#include "mainwindow.h"

#include <QComboBox>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSlider>

#include <math.h>

namespace {
    struct ImportantDate {
        QString date;
        QString focus;
        QString coordinateSystem;
    };

    const ImportantDate ImportantDates[] = {
        { "", "", "" },
        { "2007-02-27T16:40:00.00", "JupiterProjection", "Jupiter" },
        { "2015-07-14T10:10:00.00", "PlutoProjection", "Pluto" },
        { "2015-07-14T10:50:00.00", "PlutoProjection", "Pluto" },
        { "2015-07-14T11:22:00.00", "PlutoProjection", "Pluto" },
        { "2015-07-14T11:36:40.00", "PlutoProjection", "Pluto" },
        { "2015-07-14T11:48:43.00", "PlutoProjection", "Pluto" },
        { "2015-07-14T12:04:35.00", "PlutoProjection", "Pluto" },
        { "2015-07-14T15:02:46.00", "PlutoProjection", "Pluto" }
    };

    struct FocusNode {
        QString guiName;
        QString name;
        QString coordinateSystem;
    };
    const FocusNode FocusNodes[] = {
        { "Earth", "Earth", "Sun" },
        { "Sun", "Sun", "Sun" },
        { "Pluto", "PlutoProjection", "Pluto" },
        { "Charon", "Charon", "Pluto" },
        { "Jupiter", "JupiterProjection", "Jupiter" },
        { "Nix", "Nix", "Pluto" },
        { "Kerberos", "Kerberos", "Pluto" },
        { "Hydra", "Hydra", "Pluto" },
    };
}

ControlWidget::ControlWidget(QWidget* parent)
    : QWidget(parent)
    , _currentTime(new QLabel(""))
    , _setTime(new QComboBox)
    , _currentDelta(new QLabel(""))
    , _setDelta(new QSlider(Qt::Horizontal))
    , _pause(new QPushButton("Pause"))
    , _play(new QPushButton("Play"))
    , _focusNode(new QComboBox)
    , _setFocusToNextTarget(new QPushButton("Set Focus to the next Target"))
    , _setFocusToNewHorizons(new QPushButton("Set Focus to New Horizons"))
{
    _pause->setObjectName("pause");
    _play->setObjectName("play");

    _currentTime->setObjectName("value");
    _currentDelta->setObjectName("value");

    for (const ImportantDate& d : ImportantDates)
        _setTime->addItem(d.date);
    QObject::connect(
        _setTime,
        SIGNAL(currentIndexChanged(int)),
        this,
        SLOT(onDateChange())
    );

    for (const FocusNode& f : FocusNodes)
        _focusNode->addItem(f.guiName);
    QObject::connect(
        _focusNode,
        SIGNAL(currentIndexChanged(int)),
        this,
        SLOT(onFocusChange())
    );

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
        _setFocusToNextTarget,
        SIGNAL(clicked()),
        this,
        SLOT(onFocusToTargetButton())
    );

    QObject::connect(
        _setFocusToNewHorizons,
        SIGNAL(clicked()),
        this,
        SLOT(onFocusToNewHorizonsButton())
);

    QVBoxLayout* mainLayout = new QVBoxLayout;

    {
        QGroupBox* box = new QGroupBox("Time", this);
        QGridLayout* layout = new QGridLayout;

        //layout->setRowStretch(1, 5);
        box->setLayout(layout);

        {
            QLabel* l = new QLabel("Current Time (UTC):");
            l->setObjectName("label");
            layout->addWidget(l, 0, 0, Qt::AlignLeft);
            layout->addWidget(_currentTime, 0, 1, Qt::AlignRight);
        }
        {
            QLabel* l = new QLabel("Bookmarked Times:");
            l->setObjectName("label");
            layout->addWidget(l, 1, 0, Qt::AlignLeft);
            layout->addWidget(_setTime, 1, 1, Qt::AlignRight);
        }
        layout->addItem(new QSpacerItem(0, 7), 2, 0, 1, 2);
        {
            QLabel* l = new QLabel("Current Time Increment\n(seconds per second):");
            l->setObjectName("label");
            layout->addWidget(l, 3, 0, Qt::AlignLeft);
            layout->addWidget(_currentDelta, 3, 1, Qt::AlignRight);
        }

        _setDelta->setObjectName("background");
        layout->addWidget(_setDelta, 4, 0, 1, 2);
        

        QWidget* controlContainer = new QWidget;
        controlContainer->setObjectName("background");
        QHBoxLayout* controlContainerLayout = new QHBoxLayout;
        controlContainerLayout->addWidget(_pause);
        controlContainerLayout->addWidget(_play);
        controlContainer->setLayout(controlContainerLayout);
        layout->addWidget(controlContainer, 5, 0, 1, 2);

        mainLayout->addWidget(box);
    }

    {
        QGroupBox* box = new QGroupBox("Focus");
        QGridLayout* layout = new QGridLayout;
        box->setLayout(layout);

        {
            QLabel* l = new QLabel("Set Focus:");
            l->setObjectName("label");
            layout->addWidget(l, 0, 0, Qt::AlignLeft);
            _focusNode->setMinimumWidth(200);
            layout->addWidget(_focusNode, 0, 1, Qt::AlignRight);
        }
        layout->addWidget(_setFocusToNextTarget, 1, 0, 1, 2);
        layout->addWidget(_setFocusToNewHorizons, 2, 0, 1, 2);

        mainLayout->addWidget(box);
    }

    setLayout(mainLayout);
}

void ControlWidget::update(QString currentTime, QString currentDelta) {
    currentTime.replace("T", " ");
    _currentTime->setText(currentTime);
    _currentDelta->setText(currentDelta);
}

void ControlWidget::onValueChange() {
    float value = static_cast<float>(_setDelta->value());

    float delta;
    if (value < 0.f) {
        value = -value;
        float d = pow(3, value / 10) - 1.f;
        delta = -d;
    }
    else {
        float d = pow(3, value / 10) - 1.f;
        delta = d;
    }

    QString script = "openspace.time.setDeltaTime(" + QString::number(delta) + ");";
    emit scriptActivity(script);
}

void ControlWidget::onPauseButton() {
    QString script = "openspace.time.setPause(true);";
    emit scriptActivity(script);
}

void ControlWidget::onPlayButton() {
    QString script = "openspace.time.setPause(false);";
    emit scriptActivity(script);
}

void ControlWidget::onDateChange() {
    int index = _setTime->currentIndex();
    if (index != 0) {
        QString date = ImportantDates[index].date;
        QString focus = ImportantDates[index].focus;
        QString coordinateSystem = ImportantDates[index].coordinateSystem;
        QString script =
            "openspace.time.setTime('" + date + "');\
             openspace.setOrigin('" + focus + "');\
             openspace.changeCoordinateSystem('" + coordinateSystem + "');";
        emit scriptActivity(script);
    }
    _setTime->blockSignals(true);
    _setTime->setCurrentIndex(0);
    _setTime->blockSignals(false);
}

void ControlWidget::onFocusChange() {
    int index = _focusNode->currentIndex();
    QString name = FocusNodes[index].name;
    QString coordinateSystem = FocusNodes[index].coordinateSystem;
    QString script = "openspace.setOrigin('" + name + "');openspace.changeCoordinateSystem('" + coordinateSystem + "');";
    emit scriptActivity(script);
}

void ControlWidget::onFocusToTargetButton() {
    std::string target = reinterpret_cast<MainWindow*>(parent())->nextTarget();
    if (!target.empty()) {
        auto it = std::find_if(std::begin(FocusNodes), std::end(FocusNodes), [target](const FocusNode& n) { return n.guiName.toLower() == QString::fromStdString(target).toLower(); });
        if (it != std::end(FocusNodes)) {
            QString name = it->name;
            QString coordinateSystem = it->coordinateSystem;
            QString script = "openspace.setOrigin('" + name + "');openspace.changeCoordinateSystem('" + coordinateSystem + "');";
            emit scriptActivity(script);
        }
    }
}

void ControlWidget::onFocusToNewHorizonsButton() {
    QString coordinateSystem;
    int date = _currentTime->text().left(4).toInt();
    if (date < 2008)
        coordinateSystem = "Jupiter";
    else if (date < 2014)
        coordinateSystem = "Sun";
    else
        coordinateSystem = "Pluto";


    QString script = "openspace.setOrigin('NewHorizons');openspace.changeCoordinateSystem('" + coordinateSystem + "');";
    emit scriptActivity(script);
}

void ControlWidget::socketConnected() {
    setDisabled(false);
}

void ControlWidget::socketDisconnected() {
    setDisabled(true);
}

