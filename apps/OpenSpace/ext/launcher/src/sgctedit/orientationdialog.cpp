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

#include "sgctedit/orientationdialog.h"

#include "sgctedit/displaywindowunion.h"
#include <sgct/config.h>
#include <QLineEdit>
#include <ghoul/glm.h>

OrientationDialog::OrientationDialog(sgct::quat& orientation, QWidget* parent)
    : QDialog(parent)
    , _orientationValue(orientation)
{
    setWindowTitle("Global Orientation");
    QVBoxLayout* layoutWindow = new QVBoxLayout(this);

    _linePitch = new QLineEdit;
    _lineRoll = new QLineEdit;
    _lineYaw = new QLineEdit;
    _linePitch->setText(QString::number(glm::degrees(_orientationValue.x)));
    _lineRoll->setText(QString::number(glm::degrees(_orientationValue.z)));
    _lineYaw->setText(QString::number(glm::degrees(_orientationValue.y)));
    {
        QDoubleValidator* validatorPitch = new QDoubleValidator(-90.0, 90.0, 15);
        validatorPitch->setNotation(QDoubleValidator::StandardNotation);
        _linePitch->setValidator(validatorPitch);
        
        QDoubleValidator* validatorRoll = new QDoubleValidator(-360.0, 360.0, 15);
        validatorRoll->setNotation(QDoubleValidator::StandardNotation);
        _lineRoll->setValidator(validatorRoll);

        QDoubleValidator* validatorYaw = new QDoubleValidator(-180.0, 180.0, 15);
        validatorYaw->setNotation(QDoubleValidator::StandardNotation);
        _lineYaw->setValidator(validatorYaw);
    }
    {
        QLabel* labelPitch = new QLabel;
        labelPitch->setText("Pitch: ");
        QHBoxLayout* layoutPitch = new QHBoxLayout;
        layoutPitch->addStretch(1);
        QString pitchTip = "Pitch or elevation: negative numbers tilt the camera "
            "downwards; positive numbers tilt upwards.\nThe allowed range is [-90, 90]. "
            "Internally, this corresponds to the x value in the quaternion.";
        labelPitch->setToolTip(pitchTip);
        _linePitch->setToolTip(pitchTip);
        layoutPitch->addWidget(labelPitch);
        layoutPitch->addWidget(_linePitch);
        layoutWindow->addLayout(layoutPitch);

        QLabel* labelRoll = new QLabel;
        labelRoll ->setText("Roll: ");
        QHBoxLayout* layoutRoll = new QHBoxLayout;
        layoutRoll->addStretch(1);
        QString rollTip = "Roll or bank: negative numbers rotate the camera counter-"
            "clockwise; positive numbers clockwise.\nThe allowed range is [-180, 180]. "
            "Internally, this corresponds to the z value in the quaternion.";
        labelRoll->setToolTip(rollTip);
        _lineRoll->setToolTip(rollTip);
        layoutRoll->addWidget(labelRoll);
        layoutRoll->addWidget(_lineRoll);
        layoutWindow->addLayout(layoutRoll);

        QLabel* labelYaw = new QLabel;
        labelYaw ->setText("Yaw: ");
        QHBoxLayout* layoutYaw = new QHBoxLayout;
        layoutYaw->addStretch(1);
        QString yawTip = "Yaw, heading, or azimuth: negative numbers pan the camera "
            "to the left; positive numbers pan to the\nright. The allowed range is "
            "[-360, 360]. Internally, this corresponds to the y value in the quaternion.";
        labelYaw->setToolTip(yawTip);
        _lineYaw->setToolTip(yawTip);
        layoutYaw->addWidget(labelYaw);
        layoutYaw->addWidget(_lineYaw);

        layoutWindow->addLayout(layoutYaw);
    }
    {
        QHBoxLayout* layoutButtonBox = new QHBoxLayout;
        QPushButton* buttonSave = new QPushButton("OK");
        buttonSave->setToolTip("Save global orientation changes");
        buttonSave->setFocusPolicy(Qt::NoFocus);
        layoutButtonBox->addStretch(1);
        layoutButtonBox->addWidget(buttonSave);
        QPushButton* buttonCancel = new QPushButton("Cancel");
        buttonCancel->setToolTip("Cancel global orientation changes");
        buttonCancel->setFocusPolicy(Qt::NoFocus);
        layoutButtonBox->addWidget(buttonCancel);
        layoutButtonBox->addStretch(1);
        connect(buttonSave, &QPushButton::released, this, &OrientationDialog::ok);
        connect(buttonCancel, &QPushButton::released, this, &OrientationDialog::reject);
        layoutWindow->addLayout(layoutButtonBox);
    }
}

void OrientationDialog::ok() {
    _orientationValue.x = glm::radians(_linePitch->text().toFloat());
    _orientationValue.y = glm::radians(_lineYaw->text().toFloat());
    _orientationValue.z = glm::radians(_lineRoll->text().toFloat());
    _orientationValue.w = 1.0;
    accept();
}
