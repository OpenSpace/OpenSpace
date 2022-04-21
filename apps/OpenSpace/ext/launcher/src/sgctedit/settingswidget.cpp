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

#include "sgctedit/settingswidget.h"

#include "sgctedit/orientationdialog.h"
#include <QCheckBox>
#include <QPushButton>
#include <QVBoxLayout>

SettingsWidget::SettingsWidget(sgct::quat orientation, QWidget* parent)
    : QWidget(parent)
    , _orientationValue(std::move(orientation))
{
    QBoxLayout* layout = new QVBoxLayout;
    layout->setContentsMargins(0, 0, 0, 0);
    
    _checkBoxVsync = new QCheckBox("Enable VSync");
    _checkBoxVsync->setToolTip(
        "If enabled, the server will frame lock and wait for all client nodes"
    );
    layout->addWidget(_checkBoxVsync);
    
    QPushButton* orientationButton = new QPushButton("Global Orientation");
    orientationButton->setToolTip(
        "Opens a separate dialog for setting the pitch, yaw, and roll of the camera\n"
        "(the orientation applies to all viewports)"
    );
    orientationButton->setFocusPolicy(Qt::NoFocus);
    layout->addWidget(orientationButton);
    connect(
        orientationButton, &QPushButton::released,
        [this]() {
            OrientationDialog _orientationDialog(_orientationValue, this);
            _orientationDialog.exec();
        }
    );

    setLayout(layout);
}

sgct::quat SettingsWidget::orientation() const {
    return _orientationValue;
}

bool SettingsWidget::vsync() const {
    return _checkBoxVsync->isChecked();
}
