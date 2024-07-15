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

#ifndef __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__
#define __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__

#include <QMainWindow>

#include "sgctedit/sgctedit.h"
#include <openspace/scene/profile.h>
#include <sgct/error.h>
#include <sgct/readconfig.h>
#include <QApplication>
#include <filesystem>
#include <optional>

namespace openspace { struct Configuration; }

class QComboBox;
class QLabel;

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
     * \param parentItem The parent that contains this (and possibly other) children
     *                   in the tree structure.
     */
    LauncherWindow(bool profileEnabled, const openspace::Configuration& globalConfig,
        bool sgctConfigEnabled, std::string sgctConfigName, QWidget* parent);

    /**
      * Returns whether "Start OpenSpace" was chosen when this window closed.
      *
      * \return `true` if the "Start OpenSpace" button was clicked
      */
    bool wasLaunchSelected() const;

    /**
      * Returns the selected profile name when launcher window closed.
      *
      * \return The name of selected profile (this is only the name without file extension
      *         and without path)
      */
    std::string selectedProfile() const;

    /**
      * Returns the selected sgct window configuration when launcher window closed.
      *
      * \return The name of selected profile (this is only the name without file extension
      *         and without path)
      */
    std::string selectedWindowConfig() const;

    /**
      * Returns `true` if the window configuration filename selected in the combo box
      * is a file in the user configurations section.
      *
      * \return `true` if window configuration is a user configuration file
      */
    bool isUserConfigSelected() const;

private:
    QWidget* createCentralWidget();
    void setBackgroundImage(const std::filesystem::path& syncPath);

    void openProfileEditor(const std::string& profile, bool isUserProfile);
    void openWindowEditor(const std::string& winCfg, bool isUserWinCfg);
    void editRefusalDialog(const std::string& title, const std::string& msg,
        const std::string& detailedText);

    void populateProfilesList(const std::string& preset);
    void populateWindowConfigsList(const std::string& preset);
    void handleReturnFromWindowEditor(const sgct::config::Cluster& cluster,
        std::filesystem::path savePath, const std::filesystem::path& saveWindowCfgPath);
    void onNewWindowConfigSelection(int newIndex);

    const std::filesystem::path _assetPath;
    const std::filesystem::path _userAssetPath;
    const std::filesystem::path _configPath;
    const std::filesystem::path _userConfigPath;
    const std::filesystem::path _profilePath;
    const std::filesystem::path _userProfilePath;
    bool _shouldLaunch = false;
    int _userAssetCount = 0;
    int _userConfigStartingIdx = 0;
    int _userConfigCount = 0;
    int _preDefinedConfigStartingIdx = 0;
    const std::string _sgctConfigName;
    int _windowConfigBoxIndexSgctCfgDefault = 0;

    QComboBox* _profileBox = nullptr;
    QComboBox* _windowConfigBox = nullptr;
    QLabel* _backgroundImage = nullptr;
    QPushButton* _editWindowButton = nullptr;
};
#endif // __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__
