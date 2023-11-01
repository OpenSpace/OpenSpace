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

#include "settingsdialog.h"

#include "profile/line.h"

#include <QCheckBox>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>

SettingsDialog::SettingsDialog(openspace::configuration::Settings settings,
                               QWidget* parent)
    : QDialog(parent)
    , _startEdit(settings)
    , _currentEdit(settings)
{
    setWindowTitle("Settings");
    createWidgets();
    loadFromSettings(settings);
}

void SettingsDialog::createWidgets() {
    // Layout of this dialog:
    //
    // -------------------------------------------------------
    // | Configuration                                       |
    // | Starting Configuration:    | [oooooooooooooooooooo] |
    // | [] Keep Last Configuration                          |
    // | Profile                                             |
    // | Starting Profile:          | [oooooooooooooooooooo] |
    // | [] Keep Last Profile                                |
    // | User Interface                                      |
    // | Property Visibility        | DDDDDDDDDDDDDDDDDDDDD> |
    // | [] Bypass Launcher                                  |
    // | Informational text about undoing the bypass setting |
    // | MRF Caching                                         |
    // | [] Enable caching                                   |
    // | Cache location             | [oooooooooooooooooooo] |
    // |                            | <Save>        <Cancel> |
    // -------------------------------------------------------
     
    QGridLayout* layout = new QGridLayout(this);

    {
        QLabel* label = new QLabel("Configuration");
        label->setObjectName("heading");
        layout->addWidget(label, 0, 0, 1, 2);

        QLabel* conf = new QLabel("Starting Configuration");
        layout->addWidget(conf, 1, 0);

        _configuration = new QLineEdit;
        connect(
            _configuration,
            &QLineEdit::editingFinished,
            [this]() {
                std::string v = _configuration->text().toStdString();
                if (v.empty()) {
                    _currentEdit.configuration = std::nullopt;
                }
                else {
                    _currentEdit.configuration = v;
                }

                updateSaveButton();
            }
        );
        layout->addWidget(_configuration, 1, 1);

        _rememberLastConfiguration = new QCheckBox("Keep Last Configuration");
        connect(
            _rememberLastConfiguration,
            &QCheckBox::stateChanged,
            [this]() {
                if (_rememberLastConfiguration->isChecked()) {
                    _currentEdit.rememberLastConfiguration = true;
                }
                else {
                    _currentEdit.rememberLastConfiguration = std::nullopt;
                }
                updateSaveButton();
            }
        );
        layout->addWidget(_rememberLastConfiguration, 2, 0, 1, 2);
    }

    layout->addWidget(new Line(), 3, 0, 1, 2);

    {
        QLabel* label = new QLabel("Profile");
        label->setObjectName("heading");
        layout->addWidget(label, 4, 0, 1, 2);

        QLabel* conf = new QLabel("Starting Profile");
        layout->addWidget(conf, 5, 0);

        _profile = new QLineEdit;
        connect(
            _profile,
            &QLineEdit::editingFinished,
            [this]() {
                std::string v = _profile->text().toStdString();
                if (v.empty()) {
                    _currentEdit.profile = std::nullopt;
                }
                else {
                    _currentEdit.profile = v;
                }

                updateSaveButton();
            }
        );
        layout->addWidget(_profile, 5, 1);

        _rememberLastProfile = new QCheckBox("Keep Last Profile");
        connect(
            _rememberLastProfile,
            &QCheckBox::stateChanged,
            [this]() {
                if (_rememberLastProfile->isChecked()) {
                    _currentEdit.rememberLastProfile = true;
                }
                else {
                    _currentEdit.rememberLastProfile = std::nullopt;
                }

                updateSaveButton();
            }
        );
        layout->addWidget(_rememberLastProfile, 6, 0, 1, 2);
    }

    layout->addWidget(new Line(), 7, 0, 1, 2);

    {
        QLabel* label = new QLabel("User Interface");
        label->setObjectName("heading");
        layout->addWidget(label, 8, 0, 1, 2);

        QLabel* conf = new QLabel("Property Visibility");
        layout->addWidget(conf, 9, 0);

        _propertyVisibility = new QComboBox;
        _propertyVisibility->addItems({
            "Novice User",
            "User",
            "Advanced User",
            "Developer"
        });
        connect(
            _propertyVisibility,
            &QComboBox::textActivated,
            [this](const QString& value) {
                using Visibility = openspace::properties::Property::Visibility;
                if (value == "Novice User") {
                    _currentEdit.visibility = Visibility::NoviceUser;
                }
                else if (value == "User") {
                    // This is the default value
                    _currentEdit.visibility = std::nullopt;
                }
                else if (value == "Advanced User") {
                    _currentEdit.visibility = Visibility::AdvancedUser;
                }
                else if (value == "Developer") {
                    _currentEdit.visibility = Visibility::Developer;
                }
                else {
                    throw ghoul::MissingCaseException();
                }

                updateSaveButton();
            }
        );
        layout->addWidget(_propertyVisibility, 9, 1);

        _bypassLauncher = new QCheckBox("Bypass Launcher");
        connect(
            _bypassLauncher,
            &QCheckBox::stateChanged,
            [this]() {
                if (_bypassLauncher->isChecked()) {
                    _currentEdit.bypassLauncher = _bypassLauncher->isChecked();
                }
                else {
                    _currentEdit.bypassLauncher = std::nullopt;
                }
                _bypassInformation->setVisible(_bypassLauncher->isChecked());
                updateSaveButton();
            }
        );
        layout->addWidget(_bypassLauncher, 10, 0, 1, 2);

        _bypassInformation = new QLabel(
            "Saving the settings with the bypass launcher enabled will cause this window "
            "to not show up again, making it harder to undo this change. In case you "
            "need to undo it, you need to open the settings.json and remove the line "
            "that says '\"bypass\": true,'"
        );
        _bypassInformation->setObjectName("information");
        _bypassInformation->setHidden(true);
        _bypassInformation->setWordWrap(true);
        layout->addWidget(_bypassInformation, 11, 0, 1, 2);
    }

    layout->addWidget(new Line(), 12, 0, 1, 2);

    {
        QLabel* label = new QLabel("MRF Caching");
        label->setObjectName("heading");
        layout->addWidget(label, 13, 0, 1, 2);

        _mrf.isEnabled = new QCheckBox("Enable caching");
        connect(
            _mrf.isEnabled,
            &QCheckBox::stateChanged,
            [this]() {
                if (_mrf.isEnabled->isChecked()) {
                    _currentEdit.mrf.isEnabled = _mrf.isEnabled->isChecked();
                }
                else {
                    _currentEdit.mrf.isEnabled = std::nullopt;
                }
                updateSaveButton();
            }
        );
        layout->addWidget(_mrf.isEnabled, 14, 0, 1, 2);

        QLabel* conf = new QLabel("Cache Location");
        layout->addWidget(conf, 15, 0);

        _mrf.location = new QLineEdit;
        connect(
            _mrf.location,
            &QLineEdit::editingFinished,
            [this]() {
                if (_mrf.location->text().isEmpty()) {
                    _currentEdit.mrf.location = std::nullopt;
                }
                else {
                    _currentEdit.mrf.location = _mrf.location->text().toStdString();

                }
                updateSaveButton();
            }
        );
        layout->addWidget(_mrf.location, 15, 1);
    }

    layout->addWidget(new Line(), 16, 0, 1, 2);

    _dialogButtons = new QDialogButtonBox;
    _dialogButtons->setStandardButtons(
        QDialogButtonBox::Save | QDialogButtonBox::Cancel
    );
    QObject::connect(
        _dialogButtons, &QDialogButtonBox::accepted,
        this, &SettingsDialog::save
    );
    QObject::connect(
        _dialogButtons, &QDialogButtonBox::rejected,
        this, &SettingsDialog::reject
    );
    _dialogButtons->button(QDialogButtonBox::Save)->setEnabled(false);
    layout->addWidget(_dialogButtons, 17, 1, 1, 1, Qt::AlignRight);
}

void SettingsDialog::loadFromSettings(const openspace::configuration::Settings& settings)
{
    using namespace openspace::configuration;

    if (settings.configuration.has_value()) {
        _configuration->setText(QString::fromStdString(*settings.configuration));
    }
    if (settings.rememberLastConfiguration.has_value()) {
        _rememberLastConfiguration->setChecked(*settings.rememberLastConfiguration);
    }

    if (settings.profile.has_value()) {
        _profile->setText(QString::fromStdString(*settings.profile));
    }
    if (settings.rememberLastProfile.has_value()) {
        _rememberLastProfile->setChecked(*settings.rememberLastProfile);
    }

    if (settings.visibility.has_value()) {
        using Visibility = openspace::properties::Property::Visibility;
        Visibility vis = *settings.visibility;
        switch (vis) {
            case Visibility::NoviceUser:
                _propertyVisibility->setCurrentText("Novice User");
                break;
            case Visibility::User:
                _propertyVisibility->setCurrentText("User");
                break;
            case Visibility::AdvancedUser:
                _propertyVisibility->setCurrentText("Advanced User");
                break;
            case Visibility::Developer:
                _propertyVisibility->setCurrentText("Developer");
                break;
        }
    }

    if (settings.bypassLauncher.has_value()) {
        _bypassLauncher->setChecked(*settings.bypassLauncher);
    }

    if (settings.mrf.isEnabled.has_value()) {
        _mrf.isEnabled->setChecked(*settings.mrf.isEnabled);
    }
    if (settings.mrf.location.has_value()) {
        _mrf.location->setText(QString::fromStdString(*settings.mrf.location));
    }
}

void SettingsDialog::updateSaveButton() {
    const bool hasChanges = _startEdit != _currentEdit;
    _dialogButtons->button(QDialogButtonBox::Save)->setEnabled(hasChanges);
}

void SettingsDialog::save() {
    ghoul_assert(
        _startEdit != _currentEdit,
        "The save button should only be available if we have changes"
    );

    emit saveSettings(_currentEdit);
    QDialog::accept();
}

void SettingsDialog::reject() {
    QDialog::reject();
}
