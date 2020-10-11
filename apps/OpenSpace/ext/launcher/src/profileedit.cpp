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
#include "./ui_profileedit.h"
#include "filesystemaccess.h"
#include <QKeyEvent>
#include <iostream>

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

ProfileEdit::ProfileEdit(openspace::Profile* profile, const std::string reportedAssets,
                         std::vector<std::string>& profilesReadOnly, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::ProfileEdit)
    , _reportedAssets(reportedAssets)
    , _pData(profile)
    , _profilesReadOnly(profilesReadOnly)
{
    Q_INIT_RESOURCE(resources);

    QFile file(":/qss/launcher.qss");
    file.open(QFile::ReadOnly);
    QString styleSheet = QLatin1String(file.readAll());
    setStyleSheet(styleSheet);


    ui->setupUi(this);
    if (_pData != nullptr) {
        initSummaryTextForEachCategory();
        connect(ui->duplicate_profile, SIGNAL(clicked()), this, SLOT(duplicateProfile()));
        connect(ui->edit_meta, SIGNAL(clicked()), this, SLOT(openMeta()));
        connect(ui->edit_properties, SIGNAL(clicked()), this, SLOT(openProperties()));
        connect(ui->edit_modules, SIGNAL(clicked()), this, SLOT(openModules()));
        connect(ui->edit_keybindings, SIGNAL(clicked()), this, SLOT(openKeybindings()));
        connect(ui->edit_assets, SIGNAL(clicked()), this, SLOT(openAssets()));
        connect(ui->edit_time, SIGNAL(clicked()), this, SLOT(openTime()));
        connect(ui->edit_additionalscripts, SIGNAL(clicked()), this,
            SLOT(openAddedScripts()));
        connect(ui->edit_deltatimes, SIGNAL(clicked()), this, SLOT(openDeltaTimes()));
        connect(ui->edit_camera, SIGNAL(clicked()), this, SLOT(openCamera()));
        connect(ui->edit_marknodes, SIGNAL(clicked()), this, SLOT(openMarkNodes()));
        connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(approved()));
        connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
    }
}

ProfileEdit::~ProfileEdit() {
    delete ui;
}

void ProfileEdit::initSummaryTextForEachCategory() {
    labelText(_pData, _pData->modules().size(), "Modules", ui->label_modules);

    labelText(_pData, _pData->assets().size(), "Assets", ui->label_assets);
    ui->text_assets->setText(summarizeText_assets());
    ui->text_assets->setReadOnly(true);

    labelText(_pData, _pData->properties().size(), "Properties", ui->label_properties);
    ui->text_properties->setText(summarizeText_properties());
    ui->text_properties->setReadOnly(true);

    labelText(_pData, _pData->keybindings().size(), "Keybindings", ui->label_keybindings);
    ui->text_keybindings->setText(summarizeText_keybindings());
    ui->text_keybindings->setReadOnly(true);

    labelText(_pData, _pData->deltaTimes().size(), "Simulation Time Increments",
        ui->label_deltatimes);
    labelText(_pData, _pData->markNodes().size(), "Mark Interesting Nodes",
        ui->label_marknodes);
    labelText(_pData, 0, "Camera", ui->label_camera);
    labelText(_pData, 0, "Time", ui->label_time);
    labelText(_pData, 0, "Meta", ui->label_meta);
    labelText(_pData, 0, "Additional Scripts", ui->label_additionalscripts);
}

void ProfileEdit::setProfileName(QString profileToSet) {
    ui->line_profile->setText(profileToSet);
}

void ProfileEdit::duplicateProfile() {
    QString currentProfile = ui->line_profile->text();
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
        ui->line_profile->setText(duplicatedName);
    }
    ui->label_error->setText("");
}

void ProfileEdit::openMeta() {
    ui->label_error->setText("");
    if (_pData) {
       _meta = new meta(_pData);
       _meta->exec();
       delete _meta;
    }
}

void ProfileEdit::openModules() {
    ui->label_error->setText("");
    if (_pData) {
        _modules = new osmodules(_pData);
        _modules->exec();
        labelText(_pData, _pData->modules().size(), "Modules", ui->label_modules);
        delete _modules;
    }
}

void ProfileEdit::openProperties() {
    ui->label_error->setText("");
    if (_pData) {
        _properties = new properties(_pData);
        _properties->exec();
        labelText(_pData, _pData->properties().size(), "Properties",
            ui->label_properties
        );
        ui->text_properties->setText(summarizeText_properties());
        delete _properties;
    }
}

void ProfileEdit::openKeybindings() {
    ui->label_error->setText("");
    if (_pData) {
        _keybindings = new keybindings(_pData);
        _keybindings->exec();
        labelText(_pData, _pData->keybindings().size(), "Keybindings",
            ui->label_keybindings
        );
        ui->text_keybindings->setText(summarizeText_keybindings());
        delete _keybindings;
    }
}

void ProfileEdit::openAssets() {
    ui->label_error->setText("");
    if (_pData) {
        _assets = new Assets(_pData, _reportedAssets, this);
        _assets->exec();
        labelText(_pData, _pData->assets().size(), "Assets", ui->label_assets);
        ui->text_assets->setText(_assets->createTextSummary());
        ui->text_assets->setText(summarizeText_assets());
        delete _assets;
    }
}

void ProfileEdit::openTime() {
    ui->label_error->setText("");
    if (_pData) {
        _time = new ostime(_pData);
        _time->exec();
        delete _time;
    }
}

void ProfileEdit::openDeltaTimes() {
    ui->label_error->setText("");
    if (_pData) {
        _deltaTimes = new DeltaTimes(_pData, this);
        _deltaTimes->exec();
        labelText(_pData, _pData->deltaTimes().size(), "Simulation Time Increments",
            ui->label_deltatimes
        );
        delete _deltaTimes;
    }
}

void ProfileEdit::openAddedScripts() {
    ui->label_error->setText("");
    if (_pData) {
        _addedScripts = new AdditionalScripts(_pData, this);
        _addedScripts->exec();
        delete _addedScripts;
    }
}

void ProfileEdit::openCamera() {
    ui->label_error->setText("");
    if (_pData) {
        _camera = new Camera(_pData, this);
        _camera->exec();
        delete _camera;
    }
}

void ProfileEdit::openMarkNodes() {
    ui->label_error->setText("");
    if (_pData) {
        _markNodes = new markNodes(_pData);
        _markNodes->exec();
        labelText(_pData, _pData->markNodes().size(), "Mark Interesting Nodes",
            ui->label_marknodes
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
    return ui->line_profile->text().toUtf8().constData();
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
    QString profileName = ui->line_profile->text();
    if ((profileName.length() > 0) && !isReadOnly(profileName.toUtf8().constData())) {
        _saveSelected = true;
        ui->label_error->setText("");
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
        ui->label_error->setText(errorLabel);
    }
}

void ProfileEdit::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    QDialog::keyPressEvent(evt);
}

