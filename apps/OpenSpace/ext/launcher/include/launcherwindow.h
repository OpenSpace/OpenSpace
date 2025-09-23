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

#ifndef __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__
#define __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__

#include <QMainWindow>

#include "sgctedit/sgctedit.h"
#include <openspace/scene/profile.h>
#include <sgct/config.h>
#include <sgct/error.h>
#include <QApplication>
#include <filesystem>
#include <optional>

class SplitComboBox;

namespace openspace { struct Configuration; }

class LauncherWindow final : public QMainWindow {
Q_OBJECT
public:
    /**
     * Constructor for LauncherWindow class.
     *
     * \param profileEnabled `true` if profile selection combo box will be enabled
     * \param globalConfig Reference to global configuration for OpenSpace settings
     * \param sgctConfigEnabled `true` if window selection combo box will be enabled
     * \param sgctConfigName The name of the sgct configuration function used to generate
     *        window config (blank if file is used)
     */
    LauncherWindow(bool profileEnabled, const openspace::Configuration& globalConfig,
        bool sgctConfigEnabled, std::string sgctConfigName);

    /**
      * Returns whether "Start OpenSpace" was chosen when this window closed.
      *
      * \return `true` if the "Start OpenSpace" button was clicked
      */
    bool wasLaunchSelected() const;

    /**
      * Returns the selected profile name when the launcher window closed.
      *
      * \return The path to the selected profile
      */
    std::string selectedProfile() const;

    /**
      * Returns the selected SGCT window configuration when the launcher window closed.
      *
      * \return The path to the selected profile
      */
    std::string selectedWindowConfig() const;

    /**
     * Handles keypresses while the Qt launcher window is open.
     *
     * \param evt QKeyEevent object of the key press event
     */
    void keyPressEvent(QKeyEvent* evt) override;

private slots:
    // Open the profile editor to the currently selected profile
    void editProfile();

    // Open the profile editor to a new empty profile
    void newProfile();

    // A new profile has been selected. In this case `selection` is guaranteed to have a
    // value
    void selectProfile(std::optional<std::string> selection);

    // Open the window configuration on the currently selected file
    void editConfiguration();

    // Open the window configuration on a new empty configuration
    void newConfiguration();

    // A new configuration has been selected. `selection` might be `std::nullopt` if the
    // newly selected configuration was the value provided in the openspace.cfg file
    void selectConfiguration(std::optional<std::string> selection);

    // The main start button has been pressed
    void start();

    // Open the settings dialog window
    void openSettings();

private:
    // Actually open the profile editor
    void openProfileEditor(const std::string& profile, bool isUserProfile);

    void handleReturnFromWindowEditor(std::filesystem::path savePath);

    // Enables or disables the start button depending on the validity of the selected
    // profile and configurations
    void updateStartButton() const;

    const std::filesystem::path _assetPath;
    const std::filesystem::path _userAssetPath;
    const std::filesystem::path _configPath;
    const std::filesystem::path _userConfigPath;
    const std::filesystem::path _profilePath;
    const std::filesystem::path _userProfilePath;
    bool _shouldLaunch = false;

    SplitComboBox* _profileBox = nullptr;
    SplitComboBox* _windowConfigBox = nullptr;
    QPushButton* _editProfileButton = nullptr;
    QPushButton* _editWindowButton = nullptr;
    QPushButton* _startButton = nullptr;
};
#endif // __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__
