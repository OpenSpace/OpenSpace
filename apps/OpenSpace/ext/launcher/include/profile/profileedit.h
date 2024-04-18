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

#ifndef __OPENSPACE_UI_LAUNCHER___PROFILEEDIT___H__
#define __OPENSPACE_UI_LAUNCHER___PROFILEEDIT___H__

#include <QDialog>
#include <string>
#include <vector>

namespace openspace { class Profile; }

class QWidget;
class QLabel;
class QLineEdit;
class QTextEdit;

class ProfileEdit final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for ProfileEdit class.
     *
     * \param profile The #openspace::Profile object containing all data of the new or
     *        imported profile
     * \param profileName The name of the profile to create
     * \param assetBasePath The path to the folder where the assets live
     * \param userAssetBasePath The path to the folder where the user assets live
     * \param builtInProfileBasePath The path to the folder in which the built-in profiles
     *        live
     * \param profileBasePath The path to the folder in which all profiles live
     * \param parent Pointer to parent Qt widget
     */
    ProfileEdit(openspace::Profile& profile, const std::string& profileName,
        std::filesystem::path assetBasePath, std::filesystem::path userAssetBasePath,
        std::filesystem::path builtInProfileBasePath,
        std::filesystem::path profileBasePath, QWidget* parent);

    /**
     * Gets the status of the save when the window is closed; was the file saved?
     *
     * \return `true` if the file was saved (false if cancel)
     */
    bool wasSaved() const;

    /**
     * Gets the profile name from the top save/edit window. This can be changed by user in
     * order to save to a different file.
     *
     * \return The profile name
     */
    std::string specifiedFilename() const;

    /**
     * Handles keypress while the Qt dialog window is open.
     *
     * \param evt The QKeyEvent object for the key press event
     */
    virtual void keyPressEvent(QKeyEvent* evt) override;

private slots:
    void duplicateProfile();
    void openMeta();
    void openProperties();
    void openModules();
    void openKeybindings();
    void openAssets();
    void openTime();
    void openAddedScripts();
    void openDeltaTimes();
    void openCamera();
    void openMarkNodes();
    void cancel();
    void approved();

private:
    void createWidgets(const std::string& profileName);
    void initSummaryTextForEachCategory();

    openspace::Profile& _profile;
    const std::filesystem::path _assetBasePath;
    const std::filesystem::path _userAssetBasePath;
    const std::filesystem::path _profileBasePath;
    const std::filesystem::path _builtInProfilesPath;
    bool _saveSelected = false;

    QLineEdit* _profileEdit = nullptr;
    QLabel* _modulesLabel = nullptr;
    QLabel* _assetsLabel = nullptr;
    QTextEdit* _assetsEdit = nullptr;
    QLabel* _propertiesLabel = nullptr;
    QTextEdit* _propertiesEdit = nullptr;
    QLabel* _keybindingsLabel = nullptr;
    QTextEdit* _keybindingsEdit = nullptr;
    QLabel* _deltaTimesLabel = nullptr;
    QLabel* _interestingNodesLabel = nullptr;
    QLabel* _cameraLabel = nullptr;
    QLabel* _timeLabel = nullptr;
    QLabel* _metaLabel = nullptr;
    QLabel* _additionalScriptsLabel = nullptr;

    QLabel* _errorMsg = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___PROFILEEDIT___H__
