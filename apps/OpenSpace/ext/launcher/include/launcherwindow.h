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

#ifndef __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__
#define __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__

#include <QMainWindow>

#include "sgctedit/sgctedit.h"
#include <openspace/scene/profile.h>
#include <sgct/error.h>
#include <sgct/readconfig.h>
#include <QApplication>
#include <optional>

namespace openspace::configuration { struct Configuration; }

class QComboBox;
class QLabel;

class LauncherWindow final : public QMainWindow {
Q_OBJECT
public:
    /**
     * Constructor for LauncherWindow class
     *
     * \param profileEnabled true if profile selection combo box will be enabled
     * \param globalConfig reference to global configuration for OpenSpace settings
     * \param sgctConfigEnabled true if window selection combo box will be enabled
     * \param sgctConfigName the name of the sgct configuration function used to
     *                       generate window config (blank if file is used)
     * \param parentItem The parent that contains this (and possibly other) children
     *                   in the tree structure.
     */
    LauncherWindow(bool profileEnabled,
        const openspace::configuration::Configuration& globalConfig,
        bool sgctConfigEnabled,  std::string sgctConfigName, QWidget* parent);

    /**
      * Returns bool for whether "start OpenSpace" was chosen when this window closed.
      * OpenSpace will not start if false
      *
      * \return true if the "start OpenSpace" button was clicked
      */
    bool wasLaunchSelected() const;

    /**
      * Returns the selected profile name when launcher window closed
      *
      * \return name of selected profile (this is only the name without file extension
      *         and without path)
      */
    std::string selectedProfile() const;

    /**
      * Returns the selected sgct window configuration when launcher window closed
      *
      * \return name of selected profile (this is only the name without file extension
      *         and without path)
      */
    std::string selectedWindowConfig() const;

    /**
      * Returns true if the window configuration filename selected in the combo box
      * is a file in the user configurations section
      *
      * \return true if window configuration is a user configuration file
      */
    bool isUserConfigSelected() const;

private:
    QWidget* createCentralWidget();
    void setBackgroundImage(const std::string& syncPath);

    void openProfileEditor(const std::string& profile, bool isUserProfile);
    void openWindowEditor(const std::string& winCfg, bool isUserWinCfg);
    void editRefusalDialog(const std::string& title, const std::string& msg,
        const std::string& detailedText);

    void populateProfilesList(std::string preset);
    void populateWindowConfigsList(std::string preset);
    void handleReturnFromWindowEditor(const sgct::config::Cluster& cluster,
        std::filesystem::path savePath, const std::string& saveWindowCfgPath);
    bool versionCheck(sgct::config::GeneratorVersion& v) const;

    const std::string _assetPath;
    const std::string _userAssetPath;
    const std::string _configPath;
    const std::string _userConfigPath;
    const std::string _profilePath;
    const std::string _userProfilePath;
    const std::vector<std::string>& _readOnlyWindowConfigs;
    const std::vector<std::string>& _readOnlyProfiles;
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
