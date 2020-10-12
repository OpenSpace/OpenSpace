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

#include <openspace/scene/profile.h>
#include "profile/profileedit.h"
#include "filesystemaccess.h"
#include <QKeyEvent>
#include <iostream>
#include <QVBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QTextEdit>
#include <QDialogButtonBox>

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

ProfileEdit::ProfileEdit(openspace::Profile* profile, const std::string reportedAssets,
                         std::vector<std::string>& profilesReadOnly, QWidget* parent)
    : QDialog(parent)
    , _reportedAssets(reportedAssets)
    , _pData(profile)
    , _profilesReadOnly(profilesReadOnly)
{
    setWindowTitle("Profile Editor");

    QBoxLayout* layout = new QVBoxLayout(this);
    QBoxLayout* topLayout = new QHBoxLayout;
    QBoxLayout* leftLayout = new QVBoxLayout;
    {
        QBoxLayout* container = new QHBoxLayout;
        QLabel* profileLabel = new QLabel("Profile Name:");
        profileLabel->setObjectName("profile");
        container->addWidget(profileLabel);

        _profileEdit = new QLineEdit;
        container->addWidget(_profileEdit);

        QPushButton* duplicateButton = new QPushButton("Duplicate Profile");
        connect(
            duplicateButton, &QPushButton::clicked,
            this, &ProfileEdit::duplicateProfile
        );
        container->addWidget(duplicateButton);

        layout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        layout->addWidget(line);
    }
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        _propertiesLabel = new QLabel("Properties");
        _propertiesLabel->setWordWrap(true);
        container->addWidget(_propertiesLabel, 0, 0);

        QPushButton* editProperties = new QPushButton("Edit");
        connect(
            editProperties, &QPushButton::clicked,
            this, &ProfileEdit::openProperties
        );
        container->addWidget(editProperties, 0, 2);

        _propertiesEdit = new QTextEdit;
        _propertiesEdit->setReadOnly(true);
        container->addWidget(_propertiesEdit, 1, 0, 1, 3);

        leftLayout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        leftLayout->addWidget(line);
    }
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        _assetsLabel = new QLabel("Assets");
        _assetsLabel->setWordWrap(true);
        container->addWidget(_assetsLabel, 0, 0);

        QPushButton* assetsProperties = new QPushButton("Edit");
        connect(assetsProperties, &QPushButton::clicked, this, &ProfileEdit::openAssets);
        container->addWidget(assetsProperties, 0, 2);

        _assetsEdit = new QTextEdit;
        _assetsEdit->setReadOnly(true);
        container->addWidget(_assetsEdit, 1, 0, 1, 3);

        leftLayout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        leftLayout->addWidget(line);
    }
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        _keybindingsLabel = new QLabel("Keybindings");
        _keybindingsLabel->setWordWrap(true);
        container->addWidget(_keybindingsLabel, 0, 0);

        QPushButton* keybindingsProperties = new QPushButton("Edit");
        connect(
            keybindingsProperties, &QPushButton::clicked,
            this, &ProfileEdit::openKeybindings
        );
        container->addWidget(keybindingsProperties, 0, 2);

        _keybindingsEdit = new QTextEdit;
        _keybindingsEdit->setReadOnly(true);
        container->addWidget(_keybindingsEdit, 1, 0, 1, 3);

        leftLayout->addLayout(container);
    }
    topLayout->addLayout(leftLayout, 3);

    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        topLayout->addWidget(line);
    }

    QBoxLayout* rightLayout = new QVBoxLayout;
    {
        QBoxLayout* container = new QVBoxLayout;
        _metaLabel = new QLabel("Meta");
        _metaLabel->setWordWrap(true);
        container->addWidget(_metaLabel);

        QPushButton* metaEdit = new QPushButton("Edit");
        connect(metaEdit, &QPushButton::clicked, this, &ProfileEdit::openMeta);
        metaEdit->setLayoutDirection(Qt::RightToLeft);
        container->addWidget(metaEdit);
        rightLayout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        rightLayout->addWidget(line);
    }
    {
        QBoxLayout* container = new QVBoxLayout;
        _interestingNodesLabel = new QLabel("Mark Interesting Nodes");
        _interestingNodesLabel->setWordWrap(true);
        container->addWidget(_interestingNodesLabel);

        QPushButton* interestingNodesEdit = new QPushButton("Edit");
        connect(
            interestingNodesEdit, &QPushButton::clicked,
            this, &ProfileEdit::openMarkNodes
        );
        interestingNodesEdit->setLayoutDirection(Qt::RightToLeft);
        container->addWidget(interestingNodesEdit);
        rightLayout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        rightLayout->addWidget(line);
    }
    {
        QBoxLayout* container = new QVBoxLayout;
        _deltaTimesLabel = new QLabel("Simulation Time Increments");
        _deltaTimesLabel->setWordWrap(true);
        container->addWidget(_deltaTimesLabel);

        QPushButton* deltaTimesEdit = new QPushButton("Edit");
        connect(
            deltaTimesEdit, &QPushButton::clicked,
            this, &ProfileEdit::openDeltaTimes
        );
        deltaTimesEdit->setLayoutDirection(Qt::RightToLeft);
        container->addWidget(deltaTimesEdit);
        rightLayout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        rightLayout->addWidget(line);
    }
    {
        QBoxLayout* container = new QVBoxLayout;
        _cameraLabel = new QLabel("Camera");
        _cameraLabel->setWordWrap(true);
        container->addWidget(_cameraLabel);

        QPushButton* cameraEdit = new QPushButton("Edit");
        connect(cameraEdit, &QPushButton::clicked, this, &ProfileEdit::openCamera);
        cameraEdit->setLayoutDirection(Qt::RightToLeft);
        container->addWidget(cameraEdit);
        rightLayout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        rightLayout->addWidget(line);
    }
    {
        QBoxLayout* container = new QVBoxLayout;
        _timeLabel = new QLabel("Time");
        _timeLabel->setWordWrap(true);
        container->addWidget(_timeLabel);

        QPushButton* timeEdit = new QPushButton("Edit");
        connect(timeEdit, &QPushButton::clicked, this, &ProfileEdit::openTime);
        timeEdit->setLayoutDirection(Qt::RightToLeft);
        container->addWidget(timeEdit);
        rightLayout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        rightLayout->addWidget(line);
    }
    {
        QBoxLayout* container = new QVBoxLayout;
        _modulesLabel = new QLabel("Modules");
        _modulesLabel->setWordWrap(true);
        container->addWidget(_modulesLabel);

        QPushButton* modulesEdit = new QPushButton("Edit");
        connect(modulesEdit, &QPushButton::clicked, this, &ProfileEdit::openModules);
        modulesEdit->setLayoutDirection(Qt::RightToLeft);
        container->addWidget(modulesEdit);
        rightLayout->addLayout(container);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        rightLayout->addWidget(line);
    }
    {
        QBoxLayout* container = new QVBoxLayout;
        _additionalScriptsLabel = new QLabel("Additional Scripts");
        _additionalScriptsLabel->setWordWrap(true);
        container->addWidget(_additionalScriptsLabel);

        QPushButton* additionalScriptsEdit = new QPushButton("Edit");
        connect(
            additionalScriptsEdit, &QPushButton::clicked,
            this, &ProfileEdit::openAddedScripts
        );
        additionalScriptsEdit->setLayoutDirection(Qt::RightToLeft);
        container->addWidget(additionalScriptsEdit);
        rightLayout->addLayout(container);
    }
    topLayout->addLayout(rightLayout);
    layout->addLayout(topLayout);

    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        layout->addWidget(line);
    }

    {
        QBoxLayout* footer = new QHBoxLayout;
        _errorMsg = new QLabel;
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footer->addWidget(_errorMsg);

        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(
            buttons, &QDialogButtonBox::accepted,
            this, &ProfileEdit::cancel
        );
        connect(
            buttons, &QDialogButtonBox::rejected,
            this, &ProfileEdit::reject
        );
        footer->addWidget(buttons);
        layout->addLayout(footer);
    }

    initSummaryTextForEachCategory();
}

void ProfileEdit::initSummaryTextForEachCategory() {
    labelText(_pData, _pData->modules().size(), "Modules", _modulesLabel);

    labelText(_pData, _pData->assets().size(), "Assets", _assetsLabel);
    _assetsEdit->setText(QString::fromStdString(summarizeAssets()));

    labelText(_pData, _pData->properties().size(), "Properties", _propertiesLabel);
    _propertiesEdit->setText(QString::fromStdString(summarizeProperties()));

    labelText(_pData, _pData->keybindings().size(), "Keybindings", _keybindingsLabel);
    _keybindingsEdit->setText(QString::fromStdString(summarizeKeybindings()));

    labelText(_pData, _pData->deltaTimes().size(), "Simulation Time Increments",
        _deltaTimesLabel);
    labelText(_pData, _pData->markNodes().size(), "Mark Interesting Nodes",
        _interestingNodesLabel);
    labelText(_pData, 0, "Camera", _cameraLabel);
    labelText(_pData, 0, "Time", _timeLabel);
    labelText(_pData, 0, "Meta", _metaLabel);
    labelText(_pData, 0, "Additional Scripts", _additionalScriptsLabel);
}

void ProfileEdit::setProfileName(QString profileToSet) {
    _profileEdit->setText(profileToSet);
}

void ProfileEdit::duplicateProfile() {
    QString currentProfile = _profileEdit->text();
    if (currentProfile != "") {
        QString duplicatedName = currentProfile + "_1";
        if ((currentProfile.length() > 2)
            && (currentProfile.midRef(currentProfile.length() - 2, 1) == "_"))
        {
            QStringRef num = currentProfile.midRef(currentProfile.length() - 1, 1);
            bool validConversion = false;
            int val = num.toInt(&validConversion, 10);
            if (validConversion && val < 9) {
                duplicatedName = currentProfile.left(currentProfile.length() - 2)
                    + "_" + QString::number(val + 1);
            }
        }
        _profileEdit->setText(duplicatedName);
    }
    _errorMsg->clear();
}

void ProfileEdit::openMeta() {
    _errorMsg->clear();
    if (_pData) {
        MetaDialog(_pData, this).exec();
    }
}

void ProfileEdit::openModules() {
    _errorMsg->clear();
    if (_pData) {
        ModulesDialog(_pData, this).exec();
        labelText(_pData, _pData->modules().size(), "Modules", _modulesLabel);
    }
}

void ProfileEdit::openProperties() {
    _errorMsg->clear();
    if (_pData) {
        PropertiesDialog(_pData, this).exec();
        labelText(_pData, _pData->properties().size(), "Properties", _propertiesLabel);
        _propertiesEdit->setText(QString::fromStdString(summarizeProperties()));
    }
}

void ProfileEdit::openKeybindings() {
    _errorMsg->clear();
    if (_pData) {
        KeybindingsDialog(_pData, this).exec();
        labelText(_pData, _pData->keybindings().size(), "Keybindings",
            _keybindingsLabel
        );
        _keybindingsEdit->setText(QString::fromStdString(summarizeKeybindings()));
    }
}

void ProfileEdit::openAssets() {
    _errorMsg->clear();
    if (_pData) {
        AssetsDialog(_pData, _reportedAssets, this).exec();
        labelText(_pData, _pData->assets().size(), "Assets", _assetsLabel);
        _assetsEdit->setText(_assets->createTextSummary());
        _assetsEdit->setText(QString::fromStdString(summarizeAssets()));
    }
}

void ProfileEdit::openTime() {
    _errorMsg->clear();
    if (_pData) {
        TimeDialog(_pData, this).exec();
    }
}

void ProfileEdit::openDeltaTimes() {
    _errorMsg->clear();
    if (_pData) {
        DeltaTimesDialog(_pData, this).exec();
        labelText(_pData, _pData->deltaTimes().size(), "Simulation Time Increments",
            _deltaTimesLabel
        );
    }
}

void ProfileEdit::openAddedScripts() {
    _errorMsg->clear();
    if (_pData) {
        AdditionalScriptsDialog(_pData, this).exec();
    }
}

void ProfileEdit::openCamera() {
    _errorMsg->clear();
    if (_pData) {
        CameraDialog(_pData, this).exec();
    }
}

void ProfileEdit::openMarkNodes() {
    _errorMsg->clear();
    if (_pData) {
        MarkNodesDialog(_pData, this).exec();
        labelText(_pData, _pData->markNodes().size(), "Mark Interesting Nodes",
            _interestingNodesLabel
        );
    }
}

void ProfileEdit::labelText(openspace::Profile* pData, int size, QString title,
                            QLabel* pLabel)
{
    if (pData == nullptr) {
        return;
    }
    QString label;
    if (size > 0) {
        label = title + " (" + QString::number(size) + ")";
    }
    else {
        label = title;
    }
    QByteArray qba = label.toLocal8Bit();
    pLabel->setText(label);
}

std::string ProfileEdit::summarizeProperties() {
    if (_pData == nullptr) {
        return "";
    }
    std::string results;
    for (openspace::Profile::Property p : _pData->properties()) {
        results += p.name + " = " + p.value + '\n';
    }
    return results;
}

std::string ProfileEdit::summarizeKeybindings() {
    if (_pData == nullptr) {
        return "";
    }
    std::string results;
    for (openspace::Profile::Keybinding k : _pData->keybindings()) {
        results += k.name + " (";
        int keymod = static_cast<int>(k.key.modifier);
        if (keymod != static_cast<int>(openspace::KeyModifier::NoModifier)) {
            results += openspace::KeyModifierNames.at(keymod) + "+";
        }
        results += openspace::KeyNames.at(static_cast<int>(k.key.key));
        results += ")\n";
    }
    return results;
}

std::string ProfileEdit::summarizeAssets() {
    std::string results;
    for (const std::string& a : _pData->assets()) {
        results += a + "\n";
    }
    return results;
}

bool ProfileEdit::wasSaved() {
    return _saveSelected;
}

std::string ProfileEdit::specifiedFilename() {
    return _profileEdit->text().toUtf8().constData();
}

void ProfileEdit::cancel() {
    _saveSelected = false;
    reject();
}

bool ProfileEdit::isReadOnly(std::string profileSave) {
    auto it = std::find(_profilesReadOnly.begin(), _profilesReadOnly.end(), profileSave);
    return !(it == _profilesReadOnly.end());
}

void ProfileEdit::approved() {
    QString profileName = _profileEdit->text();
    if ((profileName.length() > 0) && !isReadOnly(profileName.toUtf8().constData())) {
        _saveSelected = true;
        _errorMsg->setText("");
        accept();
    }
    else {
        //QString formatText = "<font color='red'>";
        //formatText += ui->label_profile->text();
        //formatText += "</font>";
        //ui->label_profile->setText(formatText);
        QString errorLabel = "<font color='red'>";
        errorLabel += "This is a read-only profile. Click 'duplicate' or rename & save.";
        errorLabel += "</font>";
        _errorMsg->setText(errorLabel);
    }
}

void ProfileEdit::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    QDialog::keyPressEvent(evt);
}

