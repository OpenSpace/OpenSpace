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

#include "sgctedit/orientationdialog.h"

#include <QDialogButtonBox>
#include <QDoubleValidator>
#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <ghoul/glm.h>

OrientationDialog::OrientationDialog(sgct::quat& orientation, QWidget* parent)
    : QDialog(parent)
    , _orientationValue(orientation)
{
    setWindowTitle("Global Orientation");
    QGridLayout* layoutWindow = new QGridLayout(this);

    {
        QString pitchTip = "Pitch or elevation: negative numbers tilt the camera "
            "downwards; positive numbers tilt upwards.\nThe allowed range is [-90, 90]. "
            "Internally, this corresponds to the x value in the quaternion";

        QLabel* labelPitch = new QLabel("Pitch");
        labelPitch->setToolTip(pitchTip);
        layoutWindow->addWidget(labelPitch, 0, 0);
        
        _linePitch = new QLineEdit;
        _linePitch->setText(QString::number(glm::degrees(_orientationValue.x)));
        _linePitch->setToolTip(pitchTip);
        QDoubleValidator* validatorPitch = new QDoubleValidator(-90.0, 90.0, 15);
        validatorPitch->setNotation(QDoubleValidator::StandardNotation);
        _linePitch->setValidator(validatorPitch);
        layoutWindow->addWidget(_linePitch, 0, 1);
    }
    {
        QString rollTip = "Roll or bank: negative numbers rotate the camera counter-"
            "clockwise; positive numbers clockwise.\nThe allowed range is [-180, 180]. "
            "Internally, this corresponds to the z value in the quaternion";

        QLabel* labelRoll = new QLabel("Roll");
        labelRoll->setToolTip(rollTip);
        layoutWindow->addWidget(labelRoll, 1, 0);
        
        _lineRoll = new QLineEdit;
        _lineRoll->setText(QString::number(glm::degrees(_orientationValue.z)));
        _lineRoll->setToolTip(rollTip);
        QDoubleValidator* validatorRoll = new QDoubleValidator(-360.0, 360.0, 15);
        validatorRoll->setNotation(QDoubleValidator::StandardNotation);
        _lineRoll->setValidator(validatorRoll);
        layoutWindow->addWidget(_lineRoll, 1, 1);
    }
    {
        QString yawTip = "Yaw, heading, or azimuth: negative numbers pan the camera "
            "to the left; positive numbers pan to the\nright. The allowed range is "
            "[-360, 360]. Internally, this corresponds to the y value in the quaternion";

        QLabel* labelYaw = new QLabel;
        labelYaw ->setText("Yaw");
        labelYaw->setToolTip(yawTip);
        layoutWindow->addWidget(labelYaw, 2, 0);

        _lineYaw = new QLineEdit;
        _lineYaw->setText(QString::number(glm::degrees(_orientationValue.y)));
        _lineYaw->setToolTip(yawTip);
        QDoubleValidator* validatorYaw = new QDoubleValidator(-180.0, 180.0, 15, this);
        validatorYaw->setNotation(QDoubleValidator::StandardNotation);
        _lineYaw->setValidator(validatorYaw);
        layoutWindow->addWidget(_lineYaw, 2, 1);
    }
    {
        QDialogButtonBox* buttons = new QDialogButtonBox(
            QDialogButtonBox::Ok | QDialogButtonBox::Cancel
        );
        connect(buttons, &QDialogButtonBox::accepted, this, &OrientationDialog::ok);
        connect(buttons, &QDialogButtonBox::rejected, this, &OrientationDialog::reject);
        layoutWindow->addWidget(buttons, 3, 0, 1, 2);
    }
}

void OrientationDialog::ok() {
    _orientationValue.x = glm::radians(_linePitch->text().toFloat());
    _orientationValue.y = glm::radians(_lineYaw->text().toFloat());
    _orientationValue.z = glm::radians(_lineRoll->text().toFloat());
    _orientationValue.w = 1.0;
    accept();
}
