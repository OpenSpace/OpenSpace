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

#ifndef __OPENSPACE_UI_LAUNCHER___SETTINGSDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___SETTINGSDIALOG___H__

#include <QDialog>

#include <openspace/engine/settings.h>

class QCheckBox;
class QComboBox;
class QDialogButtonBox;
class QLabel;
class QLineEdit;

class SettingsDialog : public QDialog {
Q_OBJECT
public:
    explicit SettingsDialog(openspace::Settings settings,
        QWidget* parent = nullptr);

signals:
    void saveSettings(openspace::Settings settings);

private:
    void createWidgets();
    void loadFromSettings(const openspace::Settings& settings);
    void updateSaveButton();

    void save();
    void reject() override;

    QLineEdit* _configuration = nullptr;
    QCheckBox* _rememberLastConfiguration = nullptr;
    QLineEdit* _profile = nullptr;
    QCheckBox* _rememberLastProfile = nullptr;
    QComboBox* _propertyVisibility = nullptr;
    QCheckBox* _bypassLauncher = nullptr;
    QLabel* _bypassInformation = nullptr;
    QComboBox* _layerServer = nullptr;

    struct {
        QCheckBox* isEnabled = nullptr;
        QLineEdit* location = nullptr;
    } _mrf;

    QDialogButtonBox* _dialogButtons = nullptr;

    // The set of settings that we have while editing
    openspace::Settings _currentEdit;
};

#endif // __OPENSPACE_UI_LAUNCHER___SETTINGSDIALOG___H__
