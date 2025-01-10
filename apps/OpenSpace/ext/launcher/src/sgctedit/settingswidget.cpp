/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
    , _showUiOnFirstWindow(new QCheckBox(
        "Show user interface only on first window using graphics:"
    ))
    , _firstWindowGraphicsSelection(new QComboBox)
    , _firstWindowSelectionLayout(new QHBoxLayout)
{
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    
    _showUiOnFirstWindow->setChecked(false);
    _showUiOnFirstWindow->setEnabled(false);
    _showUiOnFirstWindow->setToolTip(
        "If enabled the first window is marked as a GUI window resulting in the user "
        "interface only being shown\non that window and the rendering is suppressed on "
        "this first window. The remaining windows will render\nnormally but they will "
        "not show the user interface"
    );

    _firstWindowGraphicsSelection->setToolTip(
        "Select the contents of the first window to match one of the other windows"
    );
    _firstWindowGraphicsSelection->setFixedWidth(150);
    connect(
        _showUiOnFirstWindow, &QCheckBox::clicked,
        this, &SettingsWidget::showUiOnFirstWindowClicked
    );

    _firstWindowSelectionLayout->addWidget(_showUiOnFirstWindow);
    _firstWindowSelectionLayout->addWidget(_firstWindowGraphicsSelection);
    _firstWindowSelectionLayout->addStretch();
    layout->addLayout(_firstWindowSelectionLayout);

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
    return (_showUiOnFirstWindow->isChecked() && _showUiOnFirstWindow->isEnabled());
}

void SettingsWidget::setShowUiOnFirstWindow(bool setUiOnFirstWindow) {
    _showUiOnFirstWindow->setChecked(setUiOnFirstWindow);
}

void SettingsWidget::setEnableShowUiOnFirstWindowCheckbox(bool enable) {
    _showUiOnFirstWindow->setEnabled(enable);
    _firstWindowGraphicsSelection->setEnabled(enable);
}

int SettingsWidget::graphicsSelectionForShowUiOnFirstWindow() const {
    return _firstWindowGraphicsSelection->currentIndex();
}

void SettingsWidget::setGraphicsSelectionForShowUiOnFirstWindow(int selection) {
    _firstWindowGraphicsSelection->setCurrentIndex(selection);
}

void SettingsWidget::setVsync(bool enableVsync) {
    _checkBoxVsync->setChecked(enableVsync);
}

void SettingsWidget::nWindowsDisplayedChanged(int newCount) {
    constexpr int CountOneWindow = 1;
    constexpr int CountTwoWindows = 2;
    int graphicsSelect = _firstWindowGraphicsSelection->currentIndex();
    graphicsSelect = std::max(0, graphicsSelect);

    QList<QString> graphicsOptions = {"None (GUI only)"};
    for (int i = CountOneWindow; i <= newCount; i++) {
        graphicsOptions.append("Window " + QString::number(i));
    }
    _firstWindowGraphicsSelection->clear();
    _firstWindowGraphicsSelection->addItems(graphicsOptions);
    setEnableShowUiOnFirstWindowCheckbox(newCount > CountOneWindow);
    if (graphicsSelect > newCount) {
        graphicsSelect = newCount;
    }
    _firstWindowGraphicsSelection->setCurrentIndex(graphicsSelect);

    if (newCount == CountOneWindow) {
        _stateOfUiOnFirstWindowWhenDisabled = _showUiOnFirstWindow->isChecked();
        _showUiOnFirstWindow->setChecked(false);
        _firstWindowGraphicsSelection->setEnabled(false);
    }
    else if (newCount == CountTwoWindows &&
             _stateOfUiOnFirstWindowPreviousCount == CountOneWindow)
    {
        if (_stateOfUiOnFirstWindowWhenDisabled) {
            _showUiOnFirstWindow->setChecked(true);
        }
        _firstWindowGraphicsSelection->setEnabled(_showUiOnFirstWindow->isChecked());
    }
    else {
        _firstWindowGraphicsSelection->setEnabled(_showUiOnFirstWindow->isChecked());
    }
    _stateOfUiOnFirstWindowPreviousCount = newCount;
}

void SettingsWidget::showUiOnFirstWindowClicked(bool checked) {
    _firstWindowGraphicsSelection->setEnabled(checked);
}

QComboBox* SettingsWidget::firstWindowGraphicsSelection() {
    return _firstWindowGraphicsSelection;
}

QCheckBox* SettingsWidget::showUiOnFirstWindowCheckbox() {
    return _showUiOnFirstWindow;
}
