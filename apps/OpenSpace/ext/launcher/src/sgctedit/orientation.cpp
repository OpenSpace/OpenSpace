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

#include "sgctedit/orientation.h"

#include "sgctedit/orientationdialog.h"

Orientation::Orientation()
    : _orientationDialog(_orientationValue, this)
{
    _layoutOrientationFull = new QHBoxLayout;
    {
        QVBoxLayout* layoutOrientationControls = new QVBoxLayout;
        QPushButton* orientationButton = new QPushButton("Global Orientation");
        _checkBoxVsync = new QCheckBox("VSync All Windows", this);
        _checkBoxVsync->setToolTip(
            "If enabled, the server will frame lock and wait for all client nodes"
        );
        layoutOrientationControls->addWidget(_checkBoxVsync);
        orientationButton->setToolTip(
            "Opens a separate dialog for setting the pitch, "
            "yaw, and roll of the camera\n(the orientation applies to all viewports)"
        );
        layoutOrientationControls->addWidget(orientationButton);
        _layoutOrientationFull->addStretch(1);
        _layoutOrientationFull->addLayout(layoutOrientationControls);
        _layoutOrientationFull->addStretch(1);
        connect(
            orientationButton,
            &QPushButton::released,
            this,
            &Orientation::orientationDialog
        );
    }
}

void Orientation::addControlsToParentLayout(QVBoxLayout* parentLayout) {
    parentLayout->addLayout(_layoutOrientationFull);
}

void Orientation::orientationDialog() {
    _orientationDialog.exec();
}

sgct::quat Orientation::orientationValue() const {
    return _orientationValue;
}

bool Orientation::vsyncValue() const {
    return (_checkBoxVsync->checkState() == Qt::Checked);
}
