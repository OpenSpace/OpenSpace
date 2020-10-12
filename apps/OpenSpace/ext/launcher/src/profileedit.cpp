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
#include "profileedit.h"
#include "filesystemaccess.h"
#include <QKeyEvent>
#include <iostream>
#include <QVBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QTextEdit>
#include <QDialogButtonBox>
#include <QCoreApplication>

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

ProfileEdit::ProfileEdit(openspace::Profile* profile, const std::string reportedAssets,
                         std::vector<std::string>& profilesReadOnly, QWidget *parent)
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
            this, &DeltaTimes::reject
        );
        footer->addWidget(buttons);
        layout->addLayout(footer);
    }

    initSummaryTextForEachCategory();
}

void ProfileEdit::initSummaryTextForEachCategory() {
    labelText(_pData, _pData->modules().size(), "Modules", _modulesLabel);

    labelText(_pData, _pData->assets().size(), "Assets", _assetsLabel);
    _assetsEdit->setText(summarizeText_assets());

    labelText(_pData, _pData->properties().size(), "Properties", _propertiesLabel);
    _propertiesEdit->setText(summarizeText_properties());

    labelText(_pData, _pData->keybindings().size(), "Keybindings", _keybindingsLabel);
    _keybindingsEdit->setText(summarizeText_keybindings());

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
    _errorMsg->setText("");
}

void ProfileEdit::openMeta() {
    _errorMsg->setText("");
    if (_pData) {
       _meta = new Meta(_pData, this);
       _meta->exec();
       delete _meta;
    }
}

void ProfileEdit::openModules() {
    _errorMsg->setText("");
    if (_pData) {
        _modules = new Modules(_pData, this);
        _modules->exec();
        labelText(_pData, _pData->modules().size(), "Modules", _modulesLabel);
        delete _modules;
    }
}

void ProfileEdit::openProperties() {
    _errorMsg->setText("");
    if (_pData) {
        _properties = new Properties(_pData, this);
        _properties->exec();
        labelText(_pData, _pData->properties().size(), "Properties", _propertiesLabel);
        _propertiesEdit->setText(summarizeText_properties());
        delete _properties;
    }
}

void ProfileEdit::openKeybindings() {
    _errorMsg->setText("");
    if (_pData) {
        _keybindings = new Keybindings(_pData, this);
        _keybindings->exec();
        labelText(_pData, _pData->keybindings().size(), "Keybindings",
            _keybindingsLabel
        );
        _keybindingsEdit->setText(summarizeText_keybindings());
        delete _keybindings;
    }
}

void ProfileEdit::openAssets() {
    _errorMsg->setText("");
    if (_pData) {
        _assets = new Assets(_pData, _reportedAssets, this);
        _assets->exec();
        labelText(_pData, _pData->assets().size(), "Assets", _assetsLabel);
        _assetsEdit->setText(_assets->createTextSummary());
        _assetsEdit->setText(summarizeText_assets());
        delete _assets;
    }
}

void ProfileEdit::openTime() {
    _errorMsg->setText("");
    if (_pData) {
        _time = new Time(_pData, this);
        _time->exec();
        delete _time;
    }
}

void ProfileEdit::openDeltaTimes() {
    _errorMsg->setText("");
    if (_pData) {
        _deltaTimes = new DeltaTimes(_pData, this);
        _deltaTimes->exec();
        labelText(_pData, _pData->deltaTimes().size(), "Simulation Time Increments",
            _deltaTimesLabel
        );
        delete _deltaTimes;
    }
}

void ProfileEdit::openAddedScripts() {
    _errorMsg->setText("");
    if (_pData) {
        _addedScripts = new AdditionalScripts(_pData, this);
        _addedScripts->exec();
        delete _addedScripts;
    }
}

void ProfileEdit::openCamera() {
    _errorMsg->setText("");
    if (_pData) {
        _camera = new Camera(_pData, this);
        _camera->exec();
        delete _camera;
    }
}

void ProfileEdit::openMarkNodes() {
    _errorMsg->setText("");
    if (_pData) {
        _markNodes = new MarkNodes(_pData, this);
        _markNodes->exec();
        labelText(_pData, _pData->markNodes().size(), "Mark Interesting Nodes",
            _interestingNodesLabel
        );
        delete _markNodes;
    }
}

QString ProfileEdit::summarizeText_meta() {
    if (_pData == nullptr) {
        return "";
    }
    QString s;
    if (_pData->meta().has_value()) {
        s += QString(_pData->meta().value().name->c_str());
        s += ", " + QString(_pData->meta().value().version->c_str());
        s += ", " + QString(_pData->meta().value().description->c_str());
        s += ", " + QString(_pData->meta().value().author->c_str());
        s += ", " + QString(_pData->meta().value().url->c_str());
        s += ", " + QString(_pData->meta().value().license->c_str());
    }

    return s;
}

void ProfileEdit::labelText(openspace::Profile* pData, int size, QString title,
                            QLabel* pLabel)
{
    if (pData == nullptr) {
        return;
    }
    QString label;
    if (size > 0) {
        label = "<html><head/><body><p><span style=\" font-weight:600;\">" + title + " ("
            + QString::number(size) + ")</span></p></body></html>";
    }
    else {
        label = "<html><head/><body><p><span style=\" font-weight:600;\">" + title
            + "</span></p></body></html>";
    }
    QByteArray qba = label.toLocal8Bit();
    pLabel->setText(QCoreApplication::translate("ProfileEdit", qba.data(), nullptr));
}

QString ProfileEdit::summarizeText_modules() {
    if (_pData == nullptr) {
        return "";
    }
    QString results;
    for (openspace::Profile::Module m : _pData->modules()) {
        results += QString(m.name.c_str());
        if (m.loadedInstruction->size() > 0 && m.notLoadedInstruction->size() > 0) {
            results += "(has commands for both loaded & non-loaded conditions)";
        }
        else if (m.loadedInstruction->size() > 0) {
            results += "(has command for loaded condition)";
        }
        else if (m.notLoadedInstruction->size() > 0) {
            results += "(has command for non-loaded condition)";
        }
        results += "\n";
    }
    return results;
}

QString ProfileEdit::summarizeText_properties() {
    if (_pData == nullptr) {
        return "";
    }
    QString results;
    for (openspace::Profile::Property p : _pData->properties()) {
        results += QString(p.name.c_str()) + " = ";
        results += QString(p.value.c_str()) + "\n";
    }
    return results;
}

QString ProfileEdit::summarizeText_keybindings() {
    if (_pData == nullptr) {
        return "";
    }
    QString results;
    for (openspace::Profile::Keybinding k : _pData->keybindings()) {
        results += QString(k.name.c_str()) + " (";
        int keymod = static_cast<int>(k.key.modifier);
        if (keymod != static_cast<int>(openspace::KeyModifier::NoModifier)) {
            results += QString(openspace::KeyModifierNames.at(keymod).c_str()) + "+";
        }
        results += QString(openspace::KeyNames.at(static_cast<int>(k.key.key)).c_str());
        results += ")\n";
    }
    return results;
}

QString ProfileEdit::summarizeText_assets() {
    QString results;
    for (std::string a : _pData->assets()) {
        results += QString(a.c_str()) + "\n"; //"    ";
    }
    return results;
}

QString ProfileEdit::summarizeText_time() {
    if (_pData == nullptr) {
        return "";
    }
    QString results;
    if (_pData->time().has_value()) {
        if (_pData->time().value().type == openspace::Profile::Time::Type::Absolute) {
            results = "Absolute time: ";
        }
        else if (_pData->time().value().type
                 == openspace::Profile::Time::Type::Relative)
        {
            results = "Relative time: ";
        }
        results += QString(_pData->time().value().value.c_str());
    }
    return results;
}

QString ProfileEdit::summarizeText_addedScripts() {
    if (_pData == nullptr) {
        return "";
    }
    QString result;
    for (auto s : _pData->additionalScripts()) {
        result += QString(s.c_str());
    result += "\n";
    }
    return result;
}

QString ProfileEdit::summarizeText_camera() {
    if (_pData == nullptr) {
        return "";
    }
    QString results;
    if (_pData->camera().has_value()) {
        std::visit(overloaded {
            [&] (const openspace::Profile::CameraNavState& nav) {
                results = "setNavigationState: ";
                results += QString(nav.anchor.c_str()) + " ";
                results += QString(nav.aim->c_str()) + " ";
                results += QString(nav.referenceFrame.c_str()) + " ";
                results += "Pos=" + QString::number(nav.position.x) + ",";
                results += QString::number(nav.position.y) + ",";
                results += QString::number(nav.position.z) + " ";
                if (nav.up.has_value()) {
                    results += "Up=" + QString::number(nav.up.value().x) + ",";
                    results += QString::number(nav.up.value().y) + ",";
                    results += QString::number(nav.up.value().z) + " ";
                }
                if (nav.yaw.has_value()) {
                    results += "Yaw=" + QString::number(nav.yaw.value()) + " ";
                }
                if (nav.pitch.has_value()) {
                    results += "Pitch=" + QString::number(nav.pitch.value());
                }
            },
            [&] (const openspace::Profile::CameraGoToGeo& geo) {
                results = "goToGeo: ";
                results += QString(geo.anchor.c_str()) + " ";
                results += "Lat=" + QString::number(geo.latitude) + " ";
                results += "Lon=" + QString::number(geo.longitude) + " ";
                if (geo.altitude.has_value()) {
                    results += "Alt=" + QString::number(geo.altitude.value());
                }
            },
        }, _pData->camera().value());
    }
    return results;
}

QString ProfileEdit::summarizeText_markNodes() {
    if (_pData == nullptr) {
        return "";
    }
    QString results;
    for (auto s : _pData->markNodes()) {
        results += QString(s.c_str()) + "  ";
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

