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

#include "sgctedit/display.h"
#include "sgctedit/orientationdialog.h"

OrientationDialog::OrientationDialog(sgct::quat& orientation, QWidget* parent)
    : QDialog(parent)
    , _orientationValue(orientation)
{
    setWindowTitle("Global Orientation");
    _layoutWindow = new QVBoxLayout(this);
    _layoutPitch = new QHBoxLayout();
    _layoutRoll = new QHBoxLayout();
    _layoutYaw = new QHBoxLayout();

    _labelPitch = new QLabel(this);
    _labelRoll = new QLabel(this);
    _labelYaw = new QLabel(this);
    _labelPitch->setText("Pitch: ");
    _labelRoll ->setText("Roll: ");
    _labelYaw ->setText("Yaw: ");

    _linePitch = new QLineEdit(this);
    _lineRoll = new QLineEdit(this);
    _lineYaw = new QLineEdit(this);
    _linePitch->setText(QString::number(_orientationValue.x));
    _lineRoll->setText(QString::number(_orientationValue.z));
    _lineYaw->setText(QString::number(_orientationValue.y));
    _validatorPitch = new QDoubleValidator(-90.0, 90.0, 15);
    _validatorPitch->setNotation(QDoubleValidator::StandardNotation);
    _validatorRoll = new QDoubleValidator(-360.0, 360.0, 15);
    _validatorRoll->setNotation(QDoubleValidator::StandardNotation);
    _validatorYaw = new QDoubleValidator(-180.0, 180.0, 15);
    _validatorYaw->setNotation(QDoubleValidator::StandardNotation);
    _linePitch->setValidator(_validatorPitch);
    _lineRoll->setValidator(_validatorRoll);
    _lineYaw->setValidator(_validatorYaw);

    _layoutPitch->addStretch(1);
    _layoutPitch->addWidget(_labelPitch);
    _layoutPitch->addWidget(_linePitch);
    _layoutWindow->addLayout(_layoutPitch);

    _layoutRoll->addStretch(1);
    _layoutRoll->addWidget(_labelRoll);
    _layoutRoll->addWidget(_lineRoll);
    _layoutWindow->addLayout(_layoutRoll);

    _layoutYaw->addStretch(1);
    _layoutYaw->addWidget(_labelYaw);
    _layoutYaw->addWidget(_lineYaw);
    _layoutWindow->addLayout(_layoutYaw);

    _layoutButtonBox = new QHBoxLayout;
    _buttonSave = new QPushButton("OK");
    _buttonSave->setToolTip("Save global orientation changes");
    _layoutButtonBox->addStretch(1);
    _layoutButtonBox->addWidget(_buttonSave);

    _buttonCancel = new QPushButton("Cancel");
    _buttonCancel->setToolTip("Cancel global orientation changes");
    _layoutButtonBox->addWidget(_buttonCancel);
    _layoutButtonBox->addStretch(1);

    connect(_buttonSave, SIGNAL(released()), this, SLOT(ok()));
    connect(_buttonCancel, SIGNAL(released()), this, SLOT(cancel()));

    _layoutWindow->addLayout(_layoutButtonBox);
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

OrientationDialog::~OrientationDialog()
{
    delete _labelPitch;
    delete _labelRoll;
    delete _labelYaw;
    delete _validatorPitch;
    delete _validatorRoll;
    delete _validatorYaw;
    delete _linePitch;
    delete _lineRoll;
    delete _lineYaw;
    delete _buttonSave;
    delete _buttonCancel;
    delete _layoutPitch;
    delete _layoutRoll;
    delete _layoutYaw;
    delete _layoutButtonBox;
    delete _layoutWindow;
}

