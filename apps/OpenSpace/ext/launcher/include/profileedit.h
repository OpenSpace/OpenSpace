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

#ifndef __OPENSPACE_UI_LAUNCHER___PROFILEEDIT___H__
#define __OPENSPACE_UI_LAUNCHER___PROFILEEDIT___H__

#include <QDialog>
#include <QWidget>
#include "meta.h"
#include "properties.h"
#include "modules.h"
#include "keybindings.h"
#include "assets.h"
#include "timedialog.h"
#include "additionalscripts.h"
#include "deltatimes.h"
#include "camera.h"
#include "marknodes.h"
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class ProfileEdit;
}
QT_END_NAMESPACE

class ProfileEdit : public QDialog
{
    Q_OBJECT

public slots:
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

public:
    /**
     * Constructor for ProfileEdit class
     *
     * \param imported The #openspace::Profile object containing all data of the
     *                 new or imported profile.
     * \param reportedAssets string list of assets reported by filesystemAccess class
     * \param profilesReadOnly vector list of profile names that are read-only and must
     *                         not be overwritten
     * \param parent Pointer to parent Qt widget (optional)
     */
    explicit ProfileEdit(openspace::Profile* profile, const std::string reportedAssets,
        std::vector<std::string>& profilesReadOnly, QWidget *parent = nullptr);

    /**
     * Destructor for ProfileEdit class
     */
    ~ProfileEdit();

    /**
     * Sets the profile name in top save/edit window. This can be changed by user in
     * order to save to a different file.
     *
     * \param profileToSet name of the profile to set to
     */
    void setProfileName(QString profileToSet);


    /**
     * Gets the status of the save when the window is closed; was the file saved?
     *
     * \return true if the file was saved (false if cancel)
     */
    bool wasSaved();

    /**
     * Gets the profile name from the top save/edit window. This can be changed by user in
     * order to save to a different file.
     *
     * \return the profile name
     */
    std::string specifiedFilename();

    /**
     * Handles keypress while the Qt dialog window is open
     *
     * \param evt #QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent *evt);

private:
    void initSummaryTextForEachCategory();
    QString summarizeText_meta();
    QString summarizeText_modules();
    QString summarizeText_assets();
    QString summarizeText_properties();
    QString summarizeText_keybindings();
    QString summarizeText_time();
    QString summarizeText_camera();
    QString summarizeText_markNodes();
    QString summarizeText_addedScripts();
    void labelText(openspace::Profile* pData, int size, QString title, QLabel* pLabel);
    bool isReadOnly(std::string profileToSave);

    Ui::ProfileEdit *ui;
    QWidget* _parent;
    Meta* _meta;
    Properties* _properties;
    Modules* _modules;
    Keybindings* _keybindings;
    Assets* _assets;
    Time* _time;
    AdditionalScripts* _addedScripts;
    DeltaTimes* _deltaTimes;
    Camera* _camera;
    MarkNodes* _markNodes;
    openspace::Profile* _pData;
    const std::string _reportedAssets;
    bool _saveSelected = false;
    std::vector<std::string> _profilesReadOnly;
};

#endif // __OPENSPACE_UI_LAUNCHER___PROFILEEDIT___H__
