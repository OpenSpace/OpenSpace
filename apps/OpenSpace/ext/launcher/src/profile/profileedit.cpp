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

#include "profile/profileedit.h"

#include "profile/actiondialog.h"
#include "profile/additionalscriptsdialog.h"
#include "profile/assetsdialog.h"
#include "profile/cameradialog.h"
#include "profile/deltatimesdialog.h"
#include "profile/line.h"
#include "profile/marknodesdialog.h"
#include "profile/metadialog.h"
#include "profile/modulesdialog.h"
#include "profile/propertiesdialog.h"
#include "profile/timedialog.h"
#include "profile/uipanelsdialog.h"
#include <openspace/scene/profile.h>
#include <ghoul/format.h>
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QWidget>
#include <filesystem>
#include <fstream>
#include <iostream>

#ifdef WIN32
#include <Windows.h>
#endif // WIN32

using namespace openspace;

namespace {
    QString labelText(size_t size, const QString& title) {
        QString label;
        if (size > 0) {
            label = title + " (" + QString::number(size) + ")";
        }
        else {
            label = title;
        }
        return label;
    }

    std::string summarizeAssets(const std::vector<std::string>& assets) {
        std::string results;
        for (const std::string& a : assets) {
            results += std::format("{}<br>", a);
        }
        return results;
    }

    std::string summarizeKeybindings(const std::vector<Profile::Keybinding>& keybindings,
                                     const std::vector<Profile::Action>& actions)
    {
        std::string results;
        for (const Profile::Keybinding& k : keybindings) {
            const auto it = std::find_if(
                actions.cbegin(), actions.cend(),
                [id = k.action](const Profile::Action& a) { return a.identifier == id; }
            );

            std::string name = it != actions.end() ? it->name : "Unknown action";
            results += std::format("{} ({})<br>", name, ghoul::to_string(k.key));
        }
        return results;
    }

    std::string summarizeProperties(const std::vector<Profile::Property>& properties) {
        std::string results;
        for (openspace::Profile::Property p : properties) {
            results += std::format("{} = {}<br>", p.name, p.value);
        }
        return results;
    }
} // namespace

ProfileEdit::ProfileEdit(Profile& profile, std::string profileName,
                         std::filesystem::path assetBasePath,
                         std::filesystem::path userAssetBasePath,
                         std::filesystem::path profileBasePath,
                         QWidget* parent)
    : QDialog(parent)
    , _profile(profile)
    , _assetBasePath(std::move(assetBasePath))
    , _userAssetBasePath(std::move(userAssetBasePath))
    , _profileBasePath(std::move(profileBasePath))
    , _profileFilename(std::move(profileName))
{
    setWindowTitle("Profile Editor");
    createWidgets();

    initSummaryTextForEachCategory();
}

void ProfileEdit::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    QBoxLayout* topLayout = new QHBoxLayout;
    QBoxLayout* leftLayout = new QVBoxLayout;
    {
        QBoxLayout* container = new QHBoxLayout;
        QLabel* profileLabel = new QLabel("Profile Name:");
        profileLabel->setObjectName("profile");
        container->addWidget(profileLabel);

        _profileEdit = new QLineEdit(QString::fromStdString(_profileFilename));
        container->addWidget(_profileEdit);
        layout->addLayout(container);
    }
    layout->addWidget(new Line);

    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        _propertiesLabel = new QLabel("Properties");
        _propertiesLabel->setObjectName("heading");
        _propertiesLabel->setWordWrap(true);
        container->addWidget(_propertiesLabel, 0, 0);

        _propertiesEdit = new QTextEdit;
        _propertiesEdit->setReadOnly(true);
        _propertiesEdit->setAccessibleName("Property value settings");
        container->addWidget(_propertiesEdit, 1, 0, 1, 3);

        QPushButton* editProperties = new QPushButton("Edit");
        connect(
            editProperties, &QPushButton::clicked,
            this, &ProfileEdit::openProperties
        );
        editProperties->setAccessibleName("Edit properties");
        container->addWidget(editProperties, 0, 2);

        leftLayout->addLayout(container);
    }
    leftLayout->addWidget(new Line);
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        _assetsLabel = new QLabel("Assets");
        _assetsLabel->setObjectName("heading");
        _assetsLabel->setWordWrap(true);
        container->addWidget(_assetsLabel, 0, 0);

        _assetsEdit = new QTextEdit;
        _assetsEdit->setReadOnly(true);
        _assetsEdit->setAccessibleName("Loaded assets");
        container->addWidget(_assetsEdit, 1, 0, 1, 3);

        QPushButton* assetsProperties = new QPushButton("Edit");
        connect(assetsProperties, &QPushButton::clicked, this, &ProfileEdit::openAssets);
        assetsProperties->setAccessibleName("Edit assets");
        container->addWidget(assetsProperties, 0, 2);

        leftLayout->addLayout(container);
    }
    leftLayout->addWidget(new Line);
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        _keybindingsLabel = new QLabel("Actions & Keybindings");
        _keybindingsLabel->setObjectName("heading");
        container->addWidget(_keybindingsLabel, 0, 0);

        _keybindingsEdit = new QTextEdit;
        _keybindingsEdit->setReadOnly(true);
        _keybindingsEdit->setAccessibleName("Loaded action and keybindings");
        container->addWidget(_keybindingsEdit, 1, 0, 1, 3);

        QPushButton* keybindingsProperties = new QPushButton("Edit");
        connect(
            keybindingsProperties, &QPushButton::clicked,
            this, &ProfileEdit::openKeybindings
        );
        keybindingsProperties->setAccessibleName("Edit actions and keybindings");
        container->addWidget(keybindingsProperties, 0, 2);

        leftLayout->addLayout(container);
    }
    topLayout->addLayout(leftLayout, 3);

    QBoxLayout* rightLayout = new QVBoxLayout;
    {
        QBoxLayout* container = new QVBoxLayout;
        _metaLabel = new QLabel("Meta");
        _metaLabel->setObjectName("heading");
        _metaLabel->setWordWrap(true);
        container->addWidget(_metaLabel);

        QPushButton* metaEdit = new QPushButton("Edit");
        connect(metaEdit, &QPushButton::clicked, this, &ProfileEdit::openMeta);
        metaEdit->setLayoutDirection(Qt::RightToLeft);
        metaEdit->setAccessibleName("Edit metadata");
        container->addWidget(metaEdit);
        rightLayout->addLayout(container);
    }
    rightLayout->addWidget(new Line);
    {
        QBoxLayout* container = new QVBoxLayout;
        _interestingNodesLabel = new QLabel("Mark Interesting Nodes");
        _interestingNodesLabel->setObjectName("heading");
        _interestingNodesLabel->setWordWrap(true);
        container->addWidget(_interestingNodesLabel);

        QPushButton* interestingNodesEdit = new QPushButton("Edit");
        connect(
            interestingNodesEdit, &QPushButton::clicked,
            this, &ProfileEdit::openMarkNodes
        );
        interestingNodesEdit->setLayoutDirection(Qt::RightToLeft);
        interestingNodesEdit->setAccessibleName("Edit interesting nodes");
        container->addWidget(interestingNodesEdit);
        rightLayout->addLayout(container);
    }
    rightLayout->addWidget(new Line);
    {
        QBoxLayout* container = new QVBoxLayout;
        _deltaTimesLabel = new QLabel("Simulation Time Increments");
        _deltaTimesLabel->setObjectName("heading");
        _deltaTimesLabel->setWordWrap(true);
        container->addWidget(_deltaTimesLabel);

        QPushButton* deltaTimesEdit = new QPushButton("Edit");
        connect(
            deltaTimesEdit, &QPushButton::clicked,
            this, &ProfileEdit::openDeltaTimes
        );
        deltaTimesEdit->setLayoutDirection(Qt::RightToLeft);
        deltaTimesEdit->setAccessibleName("Edit simulation time increments");
        container->addWidget(deltaTimesEdit);
        rightLayout->addLayout(container);
    }
    rightLayout->addWidget(new Line);
    {
        QBoxLayout* container = new QVBoxLayout;
        _cameraLabel = new QLabel("Camera");
        _cameraLabel->setObjectName("heading");
        _cameraLabel->setWordWrap(true);
        container->addWidget(_cameraLabel);

        QPushButton* cameraEdit = new QPushButton("Edit");
        connect(cameraEdit, &QPushButton::clicked, this, &ProfileEdit::openCamera);
        cameraEdit->setLayoutDirection(Qt::RightToLeft);
        cameraEdit->setAccessibleName("Edit camera");
        container->addWidget(cameraEdit);
        rightLayout->addLayout(container);
    }
    rightLayout->addWidget(new Line);
    {
        QBoxLayout* container = new QVBoxLayout;
        _timeLabel = new QLabel("Time");
        _timeLabel->setObjectName("heading");
        _timeLabel->setWordWrap(true);
        container->addWidget(_timeLabel);

        QPushButton* timeEdit = new QPushButton("Edit");
        connect(timeEdit, &QPushButton::clicked, this, &ProfileEdit::openTime);
        timeEdit->setLayoutDirection(Qt::RightToLeft);
        timeEdit->setAccessibleName("Edit time");
        container->addWidget(timeEdit);
        rightLayout->addLayout(container);
    }
    rightLayout->addWidget(new Line);
    {
        QBoxLayout* container = new QVBoxLayout;
        _modulesLabel = new QLabel("Modules");
        _modulesLabel->setObjectName("heading");
        _modulesLabel->setWordWrap(true);
        container->addWidget(_modulesLabel);

        QPushButton* modulesEdit = new QPushButton("Edit");
        connect(modulesEdit, &QPushButton::clicked, this, &ProfileEdit::openModules);
        modulesEdit->setLayoutDirection(Qt::RightToLeft);
        modulesEdit->setAccessibleName("Edit modules");
        container->addWidget(modulesEdit);
        rightLayout->addLayout(container);
    }
    rightLayout->addWidget(new Line);
    {
        QBoxLayout* container = new QVBoxLayout;
        _uiPanelVisibilityLabel = new QLabel("User Interface Panels");
        _uiPanelVisibilityLabel->setObjectName("heading");
        _uiPanelVisibilityLabel->setWordWrap(true);
        container->addWidget(_uiPanelVisibilityLabel);

        QPushButton* uiPanelEdit = new QPushButton("Edit");
        connect(uiPanelEdit, &QPushButton::clicked, this, &ProfileEdit::openUiPanels);
        uiPanelEdit->setLayoutDirection(Qt::RightToLeft);
        uiPanelEdit->setAccessibleName("Edit user interface panels");
        container->addWidget(uiPanelEdit);
        rightLayout->addLayout(container);
    }
    rightLayout->addWidget(new Line);
    {
        QBoxLayout* container = new QVBoxLayout;
        _additionalScriptsLabel = new QLabel("Additional Scripts");
        _additionalScriptsLabel->setObjectName("heading");
        _additionalScriptsLabel->setWordWrap(true);
        container->addWidget(_additionalScriptsLabel);

        QPushButton* additionalScriptsEdit = new QPushButton("Edit");
        connect(
            additionalScriptsEdit, &QPushButton::clicked,
            this, &ProfileEdit::openAddedScripts
        );
        additionalScriptsEdit->setLayoutDirection(Qt::RightToLeft);
        additionalScriptsEdit->setAccessibleName("Edit additional scripts");
        container->addWidget(additionalScriptsEdit);
        rightLayout->addLayout(container);
    }
    rightLayout->addStretch();
    topLayout->addLayout(rightLayout);
    layout->addLayout(topLayout);

    layout->addWidget(new Line);

    {
        QBoxLayout* footer = new QHBoxLayout;
        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(buttons, &QDialogButtonBox::accepted, this, &ProfileEdit::approved);
        connect(buttons, &QDialogButtonBox::rejected, this, &ProfileEdit::reject);
        footer->addWidget(buttons);
        layout->addLayout(footer);
    }
}

void ProfileEdit::initSummaryTextForEachCategory() {
    _modulesLabel->setText(labelText(_profile.modules.size(), "Modules"));

    _assetsLabel->setText(labelText(_profile.assets.size(), "Assets"));
    _assetsEdit->setText(QString::fromStdString(summarizeAssets(_profile.assets)));

    _propertiesLabel->setText(labelText(_profile.properties.size(), "Properties"));
    _propertiesEdit->setText(
        QString::fromStdString(summarizeProperties(_profile.properties))
    );

    _keybindingsLabel->setText(
        labelText(_profile.keybindings.size(), "Actions & Keybindings")
    );
    _keybindingsEdit->setText(QString::fromStdString(
        summarizeKeybindings(_profile.keybindings, _profile.actions)
    ));

    _deltaTimesLabel->setText(
        labelText(_profile.deltaTimes.size(), "Simulation Time Increments")
    );
    _interestingNodesLabel->setText(
        labelText(_profile.markNodes.size(), "Mark Interesting Nodes")
    );
}


void ProfileEdit::openMeta() {
    MetaDialog(this, &_profile.meta).exec();
}

void ProfileEdit::openModules() {
    ModulesDialog(this, &_profile.modules).exec();
    _modulesLabel->setText(labelText(_profile.modules.size(), "Modules"));
}

void ProfileEdit::openUiPanels() {
    UiPanelsDialog(this, &_profile.uiPanelVisibility).exec();
}

void ProfileEdit::openProperties() {
    PropertiesDialog(this, &_profile.properties).exec();
    _propertiesLabel->setText(labelText(_profile.properties.size(), "Properties"));
    _propertiesEdit->setText(
        QString::fromStdString(summarizeProperties(_profile.properties))
    );
}

void ProfileEdit::openKeybindings() {
    ActionDialog(this, &_profile.actions, &_profile.keybindings).exec();
    _keybindingsLabel->setText(labelText(_profile.keybindings.size(), "Keybindings"));
    _keybindingsEdit->setText(QString::fromStdString(
        summarizeKeybindings(_profile.keybindings, _profile.actions)
    ));
}

void ProfileEdit::openAssets() {
    AssetsDialog(this, &_profile, _assetBasePath, _userAssetBasePath).exec();
    _assetsLabel->setText(labelText(_profile.assets.size(), "Assets"));
    _assetsEdit->setText(QString::fromStdString(summarizeAssets(_profile.assets)));
}

void ProfileEdit::openTime() {
    TimeDialog(this, &_profile.time).exec();
}

void ProfileEdit::openDeltaTimes() {
    DeltaTimesDialog(this, &_profile.deltaTimes).exec();
    _deltaTimesLabel->setText(
        labelText(_profile.deltaTimes.size(), "Simulation Time Increments")
    );
}

void ProfileEdit::openAddedScripts() {
    AdditionalScriptsDialog(this, &_profile.additionalScripts).exec();
}

void ProfileEdit::openCamera() {
    CameraDialog(this, &_profile.camera).exec();
}

void ProfileEdit::openMarkNodes() {
    MarkNodesDialog(this, &_profile.markNodes).exec();
    _interestingNodesLabel->setText(
        labelText(_profile.markNodes.size(), "Mark Interesting Nodes")
    );
}

bool ProfileEdit::wasSaved() const {
    return _saveSelected;
}

std::string ProfileEdit::specifiedFilename() const {
    return _profileFilename;
}

void ProfileEdit::approved() {
    // Show the save dialog
    std::filesystem::path fullPath =
        _profileBasePath / _profileEdit->text().toStdString();

    // We might need to create intermediate directories if the user specified them in the
    // name field. We do a "parent_path" here, since the last component of the path is the
    // actual desired filename
    std::filesystem::create_directories(std::filesystem::path(fullPath).parent_path());

    const QString path = QFileDialog::getSaveFileName(
        this,
        "Save Profile",
        QString::fromStdString(fullPath.string()),
        "Profile (*.profile)",
        nullptr
#ifdef __linux__
        // Linux in Qt5 and Qt6 crashes when trying to access the native dialog here
        , QFileDialog::DontUseNativeDialog
#endif // __linux__
    );

    // The user canceled the saving
    if (path.isEmpty()) {
        return;
    }

    //
    // If we got this far then we have a new or existing file that the user wanted to save
    // the profile into

    // Check if the user saved the profile in the correct folder
    std::string profileBase = std::filesystem::canonical(_profileBasePath).string();
    std::string file = std::filesystem::weakly_canonical(path.toStdString()).string();
    if (!file.starts_with(profileBase)) {
        QMessageBox::critical(
            this,
            "File Location",
            "Profiles can only be stored directly in the user profile folder or "
            "subfolders inside it"
        );
        return;
    }
    // +1 for the directory separator
    std::string filename = file.substr(profileBase.size() + 1);

    try {
        std::ofstream outFile;
        outFile.exceptions(std::ofstream::badbit | std::ofstream::failbit);
        outFile.open(path.toStdString(), std::ofstream::out);
        outFile << _profile.serialize();
    }
    catch (const std::ofstream::failure& e) {
#ifdef WIN32
        if (std::filesystem::exists(file)) {
            // Check if the file is hidden, since that causes ofstream to fail
            DWORD res = GetFileAttributesA(file.c_str());
            if (res & FILE_ATTRIBUTE_HIDDEN) {
                QMessageBox::critical(
                    this,
                    "Exception",
                    QString::fromStdString(std::format(
                        "Error writing data to file '{}' as file is marked hidden", file
                    ))
                );
                return;
            }
        }
#endif // WIN32
        QMessageBox::critical(
            this,
            "Exception",
            QString::fromStdString(std::format(
                "Error writing data to file '{}': {}", file, e.what()
            ))
        );
        return;
    }

    std::filesystem::path p = filename;
    p.replace_extension("");
    _profileFilename = p.string();
    _saveSelected = true;
    accept();
}

void ProfileEdit::keyPressEvent(QKeyEvent* evt) {
    if (evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return) {
        return;
    }
    QDialog::keyPressEvent(evt);
}


void ProfileEdit::reject()  {
    // We hijack the reject (i.e., exit window) and emit the signal. The actual shutdown
    // of the window comes at a later stage.
    emit raiseExitWindow();
}

void ProfileEdit::closeWithoutSaving() {
    _saveSelected = false;
    QDialog::reject();
}

void ProfileEdit::promptUserOfUnsavedChanges() {
    QMessageBox msgBox;
    msgBox.setText("There are unsaved changes");
    msgBox.setInformativeText("Do you want to save your changes");
    msgBox.setStandardButtons(QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
    msgBox.setDefaultButton(QMessageBox::Save);
    int ret = msgBox.exec();

    switch (ret) {
        case QMessageBox::Save:
            approved();
            break;
        case QMessageBox::Discard:
            closeWithoutSaving();
            break;
        case QMessageBox::Cancel:
            break;
        default:
            break;
    }
}
