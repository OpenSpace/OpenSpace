#include <openspace/scene/profile.h>
#include "profileedit.h"
#include "./ui_profileedit.h"
#include "filesystemaccess.h"


ProfileEdit::ProfileEdit(ProfileBlock imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::ProfileEdit)
    , _pData(imported)
{
    ui->setupUi(this);

    ui->text_meta->setText(summarizeText_meta());
    ui->text_meta->setReadOnly(true);

    ui->text_modules->setText(summarizeText_modules());
    ui->text_modules->setReadOnly(true);

    ui->text_assets->setText(summarizeText_assets());
    ui->text_assets->setReadOnly(true);

    ui->text_properties->setText(summarizeText_properties());
    ui->text_properties->setReadOnly(true);

    ui->text_keybindings->setText(summarizeText_keybindings());
    ui->text_keybindings->setReadOnly(true);

    ui->text_time->setText(summarizeText_time());
    ui->text_time->setReadOnly(true);

    ui->text_deltatimes->setText(summarizeText_deltaTimes());
    ui->text_deltatimes->setReadOnly(true);

    ui->text_camera->setText(summarizeText_camera());
    ui->text_camera->setReadOnly(true);

    ui->text_marknodes->setText(summarizeText_markNodes());
    ui->text_marknodes->setReadOnly(true);

    ui->text_additionalscripts->setText(summarizeText_addedScripts());
    ui->text_additionalscripts->setReadOnly(true);

    connect(ui->edit_meta, SIGNAL(clicked()), this, SLOT(openMeta()));
    connect(ui->edit_properties, SIGNAL(clicked()), this, SLOT(openProperties()));
    connect(ui->edit_modules, SIGNAL(clicked()), this, SLOT(openModules()));
    connect(ui->edit_keybindings, SIGNAL(clicked()), this, SLOT(openKeybindings()));
    connect(ui->edit_assets, SIGNAL(clicked()), this, SLOT(openAssets()));
    connect(ui->edit_time, SIGNAL(clicked()), this, SLOT(openTime()));
    connect(ui->edit_additionalscripts, SIGNAL(clicked()), this, SLOT(openAddedScripts()));
    connect(ui->edit_deltatimes, SIGNAL(clicked()), this, SLOT(openDeltaTimes()));
    connect(ui->edit_camera, SIGNAL(clicked()), this, SLOT(openCamera()));
    connect(ui->edit_marknodes, SIGNAL(clicked()), this, SLOT(openMarkNodes()));
}

void ProfileEdit::setProfileName(QString profileToSet) {
    ui->line_profile->setText(profileToSet);
}

void ProfileEdit::openMeta() {
    _meta = new meta(_pData._metaData);
    _meta->exec();
    ui->text_meta->setText(summarizeText_meta());
}

QString ProfileEdit::summarizeText_meta() {
    QString s = QString(_pData._metaData.name.c_str());
    s += ", " + QString(_pData._metaData.version.c_str());
    s += ", " + QString(_pData._metaData.description.c_str());
    s += ", " + QString(_pData._metaData.author.c_str());
    s += ", " + QString(_pData._metaData.url.c_str());
    s += ", " + QString(_pData._metaData.license.c_str());

    return s;
}

void ProfileEdit::openModules() {
    _modules = new osmodules(_pData._moduleData);
    _modules->exec();
    ui->text_modules->setText(summarizeText_modules());
}

QString ProfileEdit::summarizeText_modules() {
    QString results = QString("<Configured with %1 modules>\n").arg(_pData._moduleData.size());
    for (openspace::Profile::Module m : _pData._moduleData) {
        results += "    " + QString(m.name.c_str());
        if (m.loadedInstruction.size() > 0 && m.notLoadedInstruction.size() > 0) {
            results += "    (has commands for both loaded & non-loaded conditions)";
        }
        else if (m.loadedInstruction.size() > 0) {
            results += "    (has command for loaded condition)";
        }
        else if (m.notLoadedInstruction.size() > 0) {
            results += "    (has command for non-loaded condition)";
        }
        results += "\n";
    }
    return results;
}

void ProfileEdit::openProperties() {
    _properties = new properties(_pData._propsData);
    _properties->exec();
    ui->text_properties->setText(summarizeText_properties());
}

QString ProfileEdit::summarizeText_properties() {
    QString results = QString("<Configured with %1 properties>\n").arg(_pData._propsData.size());
    for (openspace::Profile::Property p : _pData._propsData) {
        results += "    " + QString(p.name.c_str()) + " = ";
        results += QString(p.value.c_str()) + "\n";
    }
    return results;
}

void ProfileEdit::openKeybindings() {
    _keybindings = new keybindings(_pData._keybindingsData);
    _keybindings->exec();
    ui->text_keybindings->setText(summarizeText_keybindings());
}

QString ProfileEdit::summarizeText_keybindings() {
    QString results =
        QString("<Configured with %1 keybindings>\n").arg(_pData._keybindingsData.size());
    for (openspace::Profile::Keybinding k : _pData._keybindingsData) {
        results += "    " + QString(k.name.c_str()) + " (";
        int keymod = static_cast<int>(k.key.modifier);
        if (keymod != static_cast<int>(openspace::KeyModifier::NoModifier)) {
            results += QString(openspace::KeyModifierNames.at(keymod).c_str()) + "+";
        }
        results += QString(openspace::KeyNames.at(static_cast<int>(k.key.key)).c_str());
        results += ")\n";
    }
    return results;
}

void ProfileEdit::openAssets() {
    _assets = new assets(_pData._assetData, _pData._reportAssetsInFilesystem);
    _assets->exec();
    ui->text_assets->setText(summarizeText_assets());
}

QString ProfileEdit::summarizeText_assets() {
    QString results = QString("<Configured with %1 assets>\n").arg(_pData._assetData.size());
    for (openspace::Profile::Asset a : _pData._assetData) {
        results += "    " + QString(a.path.c_str()) + "/";
        results += QString(a.name.c_str()) + "\n";
    }
    return results;
}

void ProfileEdit::openTime() {
    _time = new ostime(_pData._timeData);
    _time->exec();
    ui->text_time->setText(summarizeText_time());
}

QString ProfileEdit::summarizeText_time() {
    QString results;
    if (_pData._timeData.type == openspace::Profile::Time::Type::Absolute) {
        results = "Absolute time: ";
    }
    else if (_pData._timeData.type == openspace::Profile::Time::Type::Relative) {
        results = "Relative time: ";
    }
    results += QString(_pData._timeData.time.c_str());
    return results;
}

void ProfileEdit::openDeltaTimes() {
    _deltaTimes = new deltaTimes(_pData._deltaTimesData);
    _deltaTimes->exec();
    ui->text_deltatimes->setText(summarizeText_deltaTimes());
}

QString ProfileEdit::summarizeText_deltaTimes() {
    QString results =
        QString("<Configured with %1 delta times>\n").arg(_pData._deltaTimesData.size());
    for (size_t i = 0; i < _pData._deltaTimesData.size(); ++i) {
        results += _deltaTimes->createSummaryForDeltaTime(i,
            _pData._deltaTimesData._times.at(i), false);
        results += "\t" + QString::number(_pData._deltaTimesData._times.at(i)) + "\n";
    }
    return results;
}

void ProfileEdit::openAddedScripts() {
    _addedScripts = new addedScripts(_pData._addedScriptsData);
    _addedScripts->exec();
    ui->text_additionalscripts->setText(summarizeText_addedScripts());
}

QString ProfileEdit::summarizeText_addedScripts() {
    return QString(_pData._addedScriptsData.c_str());
}

void ProfileEdit::openCamera() {
    _camera = new camera(_pData._cameraData);
    _camera->exec();
    ui->text_camera->setText(summarizeText_camera());
}

QString ProfileEdit::summarizeText_camera() {
    QString results;
    if (_pData._cameraData.type == Camera::Type::Nav) {
        results = "setNavigationState: ";
        results += QString(_pData._cameraData.nav.anchor.c_str()) + " ";
        results += QString(_pData._cameraData.nav.aim.c_str()) + " ";
        results += QString(_pData._cameraData.nav.referenceFrame.c_str()) + " ";
        results += "Pos=" + QString(_pData._cameraData.nav.position[0].c_str()) + ",";
        results += QString(_pData._cameraData.nav.position[1].c_str()) + ",";
        results += QString(_pData._cameraData.nav.position[2].c_str()) + " ";
        if (_pData._cameraData.nav.up[0].length() > 0) {
            results += "Up=" + QString(_pData._cameraData.nav.up[0].c_str()) + ",";
            results += QString(_pData._cameraData.nav.up[1].c_str()) + ",";
            results += QString(_pData._cameraData.nav.up[2].c_str()) + " ";
        }
        results += "Yaw=" + QString(_pData._cameraData.nav.yaw.c_str()) + " ";
        results += "Pitch=" + QString(_pData._cameraData.nav.pitch.c_str()) + " ";
    }
    else if (_pData._cameraData.type == Camera::Type::Geo) {
        results = "goToGeo: ";
        results += QString(_pData._cameraData.geo.anchor.c_str()) + " ";
        results += "Lat=" + QString(_pData._cameraData.geo.latitude.c_str()) + " ";
        results += "Lon=" + QString(_pData._cameraData.geo.longitude.c_str()) + " ";
        results += "Alt=" + QString(_pData._cameraData.geo.altitude.c_str()) + " ";
    }
    return results;
}

void ProfileEdit::openMarkNodes() {
    _markNodes = new markNodes(_pData._markNodesData);
    _markNodes->exec();
    ui->text_marknodes->setText(summarizeText_markNodes());
}

QString ProfileEdit::summarizeText_markNodes() {
    QString results;
    for (auto s : _pData._markNodesData) {
        results += QString(s.c_str()) + "  ";
    }
    return results;
}

ProfileEdit::~ProfileEdit() {
    delete ui;
}
