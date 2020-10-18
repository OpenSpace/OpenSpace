/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/scene/profile.h>
#include <optional>

namespace openspace::configuration { struct Configuration; }

class QComboBox;
class QLabel;

class LauncherWindow : public QMainWindow {
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

private:
    QWidget* createCentralWidget();
    void setBackgroundImage(const std::string& syncPath);

    void openProfileEditor(const std::string& profile);

    void populateProfilesList(std::string preset);
    void populateWindowConfigsList(std::string preset);

    const std::string _assetPath;
    const std::string _configPath;
    const std::string _profilePath;
    const std::vector<std::string>& _readOnlyProfiles;
    bool _shouldLaunch = false;

    QComboBox* _profileBox = nullptr;
    QComboBox* _windowConfigBox = nullptr;
    QLabel* _backgroundImage = nullptr;
};
#endif // __OPENSPACE_UI_LAUNCHER___LAUNCHERWINDOW___H__
