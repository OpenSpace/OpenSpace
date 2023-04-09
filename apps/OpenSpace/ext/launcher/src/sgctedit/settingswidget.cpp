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

#include "sgctedit/settingswidget.h"

#include "sgctedit/orientationdialog.h"
#include <QCheckBox>
#include <QPushButton>
#include <QVBoxLayout>

SettingsWidget::SettingsWidget(sgct::quat orientation, QWidget* parent)
    : QWidget(parent)
    , _orientationValue(std::move(orientation))
{
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    
    _showUiOnFirstWindow = new QCheckBox("Show only user interface on the first window");
    _showUiOnFirstWindow->setChecked(false);
    _showUiOnFirstWindow->setToolTip(
        "If enabled the first window is marked as a GUI window resulting in the user "
        "interface only being shown on that window and the rendering is suppressed on "
        "this first window. The remaining windows will render normally but they will not "
        "show the user interface"
    );
    layout->addWidget(_showUiOnFirstWindow);


    _checkBoxVsync = new QCheckBox("Enable VSync");
    _checkBoxVsync->setToolTip(
        "If enabled the framerate will be locked to the refresh rate of the monitor"
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
}

sgct::quat SettingsWidget::orientation() const {
    return _orientationValue;
}

bool SettingsWidget::vsync() const {
    return _checkBoxVsync->isChecked();
}

bool SettingsWidget::showUiOnFirstWindow() const {
    return _showUiOnFirstWindow->isChecked();
}

void SettingsWidget::setShowUiOnFirstWindow(bool setUiOnFirstWindow) {
    _showUiOnFirstWindow->setChecked(setUiOnFirstWindow);
}

void SettingsWidget::setVsync(bool enableVsync) {
    _checkBoxVsync->setChecked(enableVsync);
}
