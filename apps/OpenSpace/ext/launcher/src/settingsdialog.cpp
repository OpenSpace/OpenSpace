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

SettingsDialog::SettingsDialog(openspace::Settings settings, QWidget* parent)
    : QDialog(parent)
    , _currentEdit(std::move(settings))
{
    setWindowTitle("Settings");
    createWidgets();
    loadFromSettings(_currentEdit);

    // Setting the startup values for the control will have caused the Save button to be
    // enabled, so we need to manually disable it again here
    _dialogButtons->button(QDialogButtonBox::Save)->setEnabled(false);
}

void SettingsDialog::createWidgets() {
    // Layout of this dialog:
    //
    // -------------------------------------------------------
    // | Profile                                             |
    // | Starting Profile:          | [oooooooooooooooooooo] |
    // | [] Keep Last Profile                                |
    // | Configuration                                       |
    // | Starting Configuration:    | [oooooooooooooooooooo] |
    // | [] Keep Last Configuration                          |
    // | User Interface                                      |
    // | Property Visibility        | DDDDDDDDDDDDDDDDDDDDD> |
    // | [] Bypass Launcher                                  |
    // | Informational text about undoing the bypass setting |
    // | Layer Settings                                      |
    // | Layer Server               | DDDDDDDDDDDDDDDDDDDDD> |
    // | [] Enable MRF caching                               |
    // | MRF Cache Location         | [oooooooooooooooooooo] |
    // |                            | <Save>        <Cancel> |
    // -------------------------------------------------------

    QGridLayout* layout = new QGridLayout(this);
    layout->setSizeConstraint(QLayout::SetFixedSize);

    {
        QLabel* label = new QLabel("Profile");
        label->setObjectName("heading");
        layout->addWidget(label, 0, 0, 1, 2);

        QLabel* conf = new QLabel("Starting Profile");
        conf->setToolTip(
            "With this setting, you can choose a profile that will be loaded the next "
            "time you start the application"
        );
        layout->addWidget(conf, 1, 0);

        _profile = new QLineEdit;
        _profile->setToolTip(conf->toolTip());
        connect(
            _profile,
            &QLineEdit::textChanged,
            [this]() {
                const std::string v = _profile->text().toStdString();
                if (v.empty()) {
                    _currentEdit.profile = std::nullopt;
                }
                else {
                    _currentEdit.profile = v;
                }

                updateSaveButton();
            }
        );
        layout->addWidget(_profile, 1, 1);

        _rememberLastProfile = new QCheckBox("Keep Last Profile");
        _rememberLastProfile->setToolTip(
            "If this setting is checked, the application will remember the profile that "
            "was loaded into OpenSpace and will use it at the next startup as well"
        );
        
        connect(
            _rememberLastProfile,
#if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
            &QCheckBox::checkStateChanged,
#else // ^^^^ >=6.7.0 // !WIN32 <6.7.0
            &QCheckBox::stateChanged,
#endif // (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
            [this]() {
                if (_rememberLastProfile->isChecked()) {
                    _currentEdit.rememberLastProfile = true;
                }
                else {
                    _currentEdit.rememberLastProfile = std::nullopt;
                }

                _profile->setDisabled(_rememberLastProfile->isChecked());
                updateSaveButton();
            }
        );
        layout->addWidget(_rememberLastProfile, 2, 0, 1, 2);
    }

    layout->addWidget(new Line(), 3, 0, 1, 2);

    {
        QLabel* label = new QLabel("Configuration");
        label->setObjectName("heading");
        layout->addWidget(label, 4, 0, 1, 2);

        QLabel* conf = new QLabel("Starting Configuration");
        conf->setToolTip(
            "With this setting, you can choose a window configuration that will be "
            "loaded the next time you start the application"
        );
        layout->addWidget(conf, 5, 0);

        _configuration = new QLineEdit;
        _configuration->setToolTip(conf->toolTip());
        connect(
            _configuration,
            &QLineEdit::textChanged,
            [this]() {
                const std::string v = _configuration->text().toStdString();
                if (v.empty()) {
                    _currentEdit.configuration = std::nullopt;
                }
                else {
                    _currentEdit.configuration = v;
                }

                updateSaveButton();
            }
        );
        layout->addWidget(_configuration, 5, 1);

        _rememberLastConfiguration = new QCheckBox("Keep Last Configuration");
        _rememberLastConfiguration->setToolTip(
            "If this setting is checked, the application will remember the window "
            "configuration and will use it at the next startup as well"
        );
        connect(
            _rememberLastConfiguration,
#if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
            &QCheckBox::checkStateChanged,
#else // ^^^^ >=6.7.0 // !WIN32 <6.7.0
            &QCheckBox::stateChanged,
#endif // (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
            [this]() {
                if (_rememberLastConfiguration->isChecked()) {
                    _currentEdit.rememberLastConfiguration = true;
                }
                else {
                    _currentEdit.rememberLastConfiguration = std::nullopt;
                }
                _configuration->setDisabled(_rememberLastConfiguration->isChecked());
                updateSaveButton();
            }
        );
        layout->addWidget(_rememberLastConfiguration, 6, 0, 1, 2);
    }

    layout->addWidget(new Line(), 7, 0, 1, 2);

    {
        QLabel* label = new QLabel("User Interface");
        label->setObjectName("heading");
        layout->addWidget(label, 8, 0, 1, 2);

        QLabel* conf = new QLabel("Property Visibility");
        conf->setToolTip(
            "This setting sets the default visibility for properties in the application. "
            "Note that these values are ordered, so all properties shown as a 'Novice "
            "User' are also visible when selecting 'User', etc."
        );
        layout->addWidget(conf, 9, 0);

        _propertyVisibility = new QComboBox;
        _propertyVisibility->setToolTip(conf->toolTip());
        _propertyVisibility->addItems({
            "Novice User",
            "User",
            "Advanced User",
            "Developer"
        });
        _propertyVisibility->setCurrentText("User");
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
        _propertyVisibility->setObjectName("dropdown");
        layout->addWidget(_propertyVisibility, 9, 1);

        _bypassLauncher = new QCheckBox("Bypass Launcher");
        _bypassLauncher->setToolTip(
            "If this value is selected, the Launcher will no longer be shown at startup. "
            "Note that this also means that it will not be easy to get back to this "
            "setting to reenable the Launcher either."
        );
        connect(
            _bypassLauncher,
#if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
            &QCheckBox::checkStateChanged,
#else // ^^^^ >=6.7.0 // !WIN32 <6.7.0
            &QCheckBox::stateChanged,
#endif // (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
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
        QLabel* label = new QLabel("Layer Settings");
        label->setObjectName("heading");
        layout->addWidget(label, 13, 0, 1, 2);

        QLabel* conf = new QLabel("Layer Server");
        conf->setToolTip(
            "This setting sets the default server to be used for the layers that "
            "are hosted by the OpenSpace team"
        );
        layout->addWidget(conf, 14, 0);

        _layerServer = new QComboBox;
        _layerServer->setToolTip(conf->toolTip());
        _layerServer->addItems({
            "All",
            "NewYork",
            "Sweden",
            "Utah",
            "None"
        });
        _layerServer->setCurrentText("All");
        connect(
            _layerServer,
            &QComboBox::textActivated,
            [this](const QString& value) {
                _currentEdit.layerServer =
                    openspace::stringToLayerServer(value.toStdString());
                updateSaveButton();
            }
        );
        _layerServer->setObjectName("dropdown");
        layout->addWidget(_layerServer, 14, 1);

        _mrf.isEnabled = new QCheckBox("Enable MRF Caching");
        _mrf.isEnabled->setToolTip(
            "If this setting is checked, the MRF caching for globe layers will be "
            "enabled. This means that all planetary images that are loaded over the "
            "internet will also be cached locally and stored between application runs. "
            "This will speedup the loading the second time at the expense of hard disk "
            "space."
        );
        connect(
            _mrf.isEnabled,
#if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
            &QCheckBox::checkStateChanged,
#else // ^^^^ >=6.7.0 // !WIN32 <6.7.0
            &QCheckBox::stateChanged,
#endif // (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
            [this]() {
                if (_mrf.isEnabled->isChecked()) {
                    _currentEdit.mrf.isEnabled = _mrf.isEnabled->isChecked();
                }
                else {
                    _currentEdit.mrf.isEnabled = std::nullopt;
                }

                _mrf.location->setDisabled(!_mrf.isEnabled->isChecked());
                updateSaveButton();
            }
        );
        layout->addWidget(_mrf.isEnabled, 16, 0, 1, 2);

        QLabel* mrfConf = new QLabel("MRF Cache Location");
        conf->setToolTip(
            "This is the place where the MRF cache files are located. Please note that "
            "these files can potentially become quite large when using OpenSpace for a "
            "long while and when visiting new places regularly. If this value is left "
            "blank, the cached files will be stored in the 'mrf_cache' folder in the "
            "OpenSpace base folder."
        );
        layout->addWidget(mrfConf, 17, 0);

        _mrf.location = new QLineEdit;
        _mrf.location->setToolTip(mrfConf->toolTip());
        _mrf.location->setDisabled(true);
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
        layout->addWidget(_mrf.location, 17, 1);
    }

    layout->addWidget(new Line(), 18, 0, 1, 2);

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
    layout->addWidget(_dialogButtons, 19, 1, 1, 1, Qt::AlignRight);
}

void SettingsDialog::loadFromSettings(const openspace::Settings& settings) {
    using namespace openspace;

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
        const Visibility vis = *settings.visibility;
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
            case Visibility::Always:
            case Visibility::Hidden:
                break;
        }
    }

    if (settings.bypassLauncher.has_value()) {
        _bypassLauncher->setChecked(*settings.bypassLauncher);
    }

    if (settings.layerServer.has_value()) {
        Configuration::LayerServer server = *settings.layerServer;
        _layerServer->setCurrentText(
            QString::fromStdString(openspace::layerServerToString(std::move(server)))
        );
    }

    if (settings.mrf.isEnabled.has_value()) {
        _mrf.isEnabled->setChecked(*settings.mrf.isEnabled);
    }
    if (settings.mrf.location.has_value()) {
        _mrf.location->setText(QString::fromStdString(*settings.mrf.location));
    }
}

void SettingsDialog::updateSaveButton() {
    _dialogButtons->button(QDialogButtonBox::Save)->setEnabled(true);
}

void SettingsDialog::save() {
    emit saveSettings(_currentEdit);
    _dialogButtons->button(QDialogButtonBox::Save)->setEnabled(false);
    QDialog::accept();
}

void SettingsDialog::reject() {
    QDialog::reject();
}
