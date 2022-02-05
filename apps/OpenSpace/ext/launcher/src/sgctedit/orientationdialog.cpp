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

#include "sgctedit/display.h"

OrientationDialog::OrientationDialog(sgct::quat& orientation, QWidget* parent)
    : QDialog(parent)
    , _orientationValue(orientation)
{
    setWindowTitle("Global Orientation");
    QVBoxLayout* layoutWindow = new QVBoxLayout(this);

    _linePitch = new QLineEdit(this);
    _lineRoll = new QLineEdit(this);
    _lineYaw = new QLineEdit(this);
    _linePitch->setText(QString::number(_orientationValue.x));
    _lineRoll->setText(QString::number(_orientationValue.z));
    _lineYaw->setText(QString::number(_orientationValue.y));
    {
        QDoubleValidator* validatorPitch = new QDoubleValidator(-90.0, 90.0, 15);
        QDoubleValidator* validatorRoll = new QDoubleValidator(-360.0, 360.0, 15);
        QDoubleValidator* validatorYaw = new QDoubleValidator(-180.0, 180.0, 15);
        validatorPitch->setNotation(QDoubleValidator::StandardNotation);
        validatorRoll->setNotation(QDoubleValidator::StandardNotation);
        validatorYaw->setNotation(QDoubleValidator::StandardNotation);
        _linePitch->setValidator(validatorPitch);
        _lineRoll->setValidator(validatorRoll);
        _lineYaw->setValidator(validatorYaw);
    }
    {
        QLabel* labelPitch = new QLabel(this);
        labelPitch->setText("Pitch: ");
        QHBoxLayout* layoutPitch = new QHBoxLayout;
        layoutPitch->addStretch(1);
        layoutPitch->addWidget(labelPitch);
        layoutPitch->addWidget(_linePitch);
        layoutWindow->addLayout(layoutPitch);
        QLabel* labelRoll = new QLabel(this);
        labelRoll ->setText("Roll: ");
        QHBoxLayout* layoutRoll = new QHBoxLayout;
        layoutRoll->addStretch(1);
        layoutRoll->addWidget(labelRoll);
        layoutRoll->addWidget(_lineRoll);
        layoutWindow->addLayout(layoutRoll);
        QLabel* labelYaw = new QLabel(this);
        labelYaw ->setText("Yaw: ");
        QHBoxLayout* layoutYaw = new QHBoxLayout;
        layoutYaw->addStretch(1);
        layoutYaw->addWidget(labelYaw);
        layoutYaw->addWidget(_lineYaw);
        layoutWindow->addLayout(layoutYaw);
    }
    {
        QHBoxLayout* layoutButtonBox = new QHBoxLayout;
        QPushButton* buttonSave = new QPushButton("OK");
        buttonSave->setToolTip("Save global orientation changes");
        layoutButtonBox->addStretch(1);
        layoutButtonBox->addWidget(buttonSave);
        QPushButton* buttonCancel = new QPushButton("Cancel");
        buttonCancel->setToolTip("Cancel global orientation changes");
        layoutButtonBox->addWidget(buttonCancel);
        layoutButtonBox->addStretch(1);
        connect(buttonSave, SIGNAL(released()), this, SLOT(ok()));
        connect(buttonCancel, SIGNAL(released()), this, SLOT(cancel()));
        layoutWindow->addLayout(layoutButtonBox);
    }
}

void OrientationDialog::ok() {
    _orientationValue.x = _linePitch->text().toFloat() / 180.0 * glm::pi<float>();
    _orientationValue.y = _lineYaw->text().toFloat() / 180.0 * glm::pi<float>();
    _orientationValue.z = _lineRoll->text().toFloat() / 180.0 * glm::pi<float>();
    _orientationValue.w = 1.0;
    accept();
}

void OrientationDialog::cancel() {
    reject();
}
