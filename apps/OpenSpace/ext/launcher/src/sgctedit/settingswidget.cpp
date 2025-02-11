/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <QCheckBox>
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>

SettingsWidget::SettingsWidget(sgct::quat orientation, QWidget* parent)
    : QWidget(parent)
{
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    //
    // Show UI in specific window
    _showUiOnFirstWindow = new QCheckBox(
        "Show user interface only on first window using graphics:"
    );
    _showUiOnFirstWindow->setChecked(false);
    _showUiOnFirstWindow->setEnabled(false);
    _showUiOnFirstWindow->setToolTip(
        "If enabled the first window is marked as a GUI window resulting in the user "
        "interface only being shown\non that window and the rendering is suppressed on "
        "this first window. The remaining windows will render\nnormally but they will "
        "not show the user interface"
    );

    _firstWindowGraphicsSelection = new QComboBox;
    _firstWindowGraphicsSelection->setToolTip(
        "Select the contents of the first window to match one of the other windows"
    );
    _firstWindowGraphicsSelection->setFixedWidth(150);
    connect(
        _showUiOnFirstWindow, &QCheckBox::clicked,
        this, &SettingsWidget::showUiOnFirstWindowClicked
    );

    _firstWindowSelectionLayout = new QHBoxLayout;
    _firstWindowSelectionLayout->addWidget(_showUiOnFirstWindow);
    _firstWindowSelectionLayout->addWidget(_firstWindowGraphicsSelection);
    _firstWindowSelectionLayout->addStretch();
    layout->addLayout(_firstWindowSelectionLayout);


    //
    // VSync settings
    _checkBoxVsync = new QCheckBox("Enable VSync");
    _checkBoxVsync->setToolTip(
        "If enabled the framerate will be locked to the refresh rate of the monitor"
    );
    layout->addWidget(_checkBoxVsync);
    

    //
    // Orientation specification
    QWidget* orientationContainer = new QWidget;
    layout->addWidget(orientationContainer);
    QGridLayout* layoutWindow = new QGridLayout(orientationContainer);

    glm::quat q = glm::quat(orientation.w, orientation.x, orientation.y, orientation.z);

    {
        const QString pitchTip = "Pitch or elevation: negative numbers tilt the camera "
            "downwards; positive numbers tilt upwards.\nThe allowed range is [-90, 90].";

        QLabel* labelPitch = new QLabel("Pitch");
        labelPitch->setToolTip(pitchTip);
        layoutWindow->addWidget(labelPitch, 0, 0);

        _linePitch = new QLineEdit;
        _linePitch->setText(QString::number(glm::degrees(glm::pitch(q))));
        _linePitch->setToolTip(pitchTip);
        QDoubleValidator* validatorPitch = new QDoubleValidator(-90.0, 90.0, 15);
        validatorPitch->setNotation(QDoubleValidator::StandardNotation);
        _linePitch->setValidator(validatorPitch);
        layoutWindow->addWidget(_linePitch, 0, 1);
    }
    {
        const QString rollTip = "Roll or bank: negative numbers rotate the camera "
            "counter-clockwise; positive numbers clockwise.\nThe allowed range is "
            "[-180, 180].";

        QLabel* labelRoll = new QLabel("Roll");
        labelRoll->setToolTip(rollTip);
        layoutWindow->addWidget(labelRoll, 1, 0);

        _lineRoll = new QLineEdit;
        _lineRoll->setText(QString::number(glm::degrees(glm::roll(q))));
        _lineRoll->setToolTip(rollTip);
        QDoubleValidator* validatorRoll = new QDoubleValidator(-360.0, 360.0, 15);
        validatorRoll->setNotation(QDoubleValidator::StandardNotation);
        _lineRoll->setValidator(validatorRoll);
        layoutWindow->addWidget(_lineRoll, 1, 1);
    }
    {
        const QString yawTip = "Yaw, heading, or azimuth: negative numbers pan the "
            "camera to the left; positive numbers pan to the\nright. The allowed range "
            "is [-360, 360].";

        QLabel* labelYaw = new QLabel;
        labelYaw->setText("Yaw");
        labelYaw->setToolTip(yawTip);
        layoutWindow->addWidget(labelYaw, 2, 0);

        _lineYaw = new QLineEdit;
        _lineYaw->setText(QString::number(glm::degrees(glm::yaw(q))));
        _lineYaw->setToolTip(yawTip);
        QDoubleValidator* validatorYaw = new QDoubleValidator(-180.0, 180.0, 15, this);
        validatorYaw->setNotation(QDoubleValidator::StandardNotation);
        _lineYaw->setValidator(validatorYaw);
        layoutWindow->addWidget(_lineYaw, 2, 1);
    }
}

sgct::quat SettingsWidget::orientation() const {
    // Reconstitute the quaternion
    const float pitch = glm::radians(_linePitch->text().toFloat());
    const float yaw = glm::radians(_lineYaw->text().toFloat());
    const float roll = glm::radians(_lineRoll->text().toFloat());
    glm::quat q = glm::quat(glm::vec3(pitch, yaw, roll));
    return sgct::quat(q.x, q.y, q.z, q.w);
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

    QList<QString> graphicsOptions = { "None (GUI only)" };
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
