#include <openspace/scene/profile.h>
#include "profileedit.h"
#include "./ui_profileedit.h"
#include "filesystemaccess.h"

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

ProfileEdit::ProfileEdit(openspace::Profile* profile, const std::string reportedAssets, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::ProfileEdit)
    , _reportedAssets(reportedAssets)
    , _pData(profile)
{
    ui->setupUi(this);
    if (_pData != nullptr) {
        initSummaryTextForEachCategory();
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
        connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(approved()));
        connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
    }
}

ProfileEdit::~ProfileEdit() {
    delete ui;
}

void ProfileEdit::initSummaryTextForEachCategory() {
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
}

void ProfileEdit::setProfileName(QString profileToSet) {
    ui->line_profile->setText(profileToSet);
}

void ProfileEdit::openMeta() {
    if (_pData) {
       _meta = new meta(_pData);
       _meta->exec();
       ui->text_meta->setText(summarizeText_meta());
    }
}

void ProfileEdit::openModules() {
    if (_pData) {
        _modules = new osmodules(_pData);
        _modules->exec();
        ui->text_modules->setText(summarizeText_modules());
    }
}

void ProfileEdit::openProperties() {
    if (_pData) {
        _properties = new properties(_pData);
        _properties->exec();
        ui->text_properties->setText(summarizeText_properties());
    }
}

void ProfileEdit::openKeybindings() {
    if (_pData) {
        _keybindings = new keybindings(_pData);
        _keybindings->exec();
        ui->text_keybindings->setText(summarizeText_keybindings());
    }
}

void ProfileEdit::openAssets() {
    if (_pData) {
        _assets = new assets(_pData, _reportedAssets);
        _assets->exec();
        ui->text_assets->setText(summarizeText_assets());
    }
}

void ProfileEdit::openTime() {
    if (_pData) {
        _time = new ostime(_pData);
        _time->exec();
        ui->text_time->setText(summarizeText_time());
    }
}

void ProfileEdit::openDeltaTimes() {
    if (_pData) {
        _deltaTimes = new deltaTimes(_pData);
        _deltaTimes->exec();
        ui->text_deltatimes->setText(summarizeText_deltaTimes());
    }
}

void ProfileEdit::openAddedScripts() {
    if (_pData) {
        _addedScripts = new addedScripts(_pData);
        _addedScripts->exec();
        ui->text_additionalscripts->setText(summarizeText_addedScripts());
    }
}

void ProfileEdit::openCamera() {
    if (_pData) {
        _camera = new camera(_pData);
        _camera->exec();
        ui->text_camera->setText(summarizeText_camera());
    }
}

void ProfileEdit::openMarkNodes() {
    if (_pData) {
        _markNodes = new markNodes(_pData);
        _markNodes->exec();
        ui->text_marknodes->setText(summarizeText_markNodes());
    }
}

QString ProfileEdit::summarizeText_meta() {
    if (_pData == nullptr) {
        return "";
    }
    QString s;
    if (_pData->meta().has_value()) {
        s += QString(_pData->meta().value().name.c_str());
        s += ", " + QString(_pData->meta().value().version.c_str());
        s += ", " + QString(_pData->meta().value().description.c_str());
        s += ", " + QString(_pData->meta().value().author.c_str());
        s += ", " + QString(_pData->meta().value().url.c_str());
        s += ", " + QString(_pData->meta().value().license.c_str());
    }

    return s;
}

QString ProfileEdit::summarizeText_modules() {
    if (_pData == nullptr) {
        return "";
    }
    QString results = QString("<Configured with %1 modules>\n").arg(_pData->modules().size());
    for (openspace::Profile::Module m : _pData->modules()) {
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

QString ProfileEdit::summarizeText_properties() {
    if (_pData == nullptr) {
        return "";
    }
    QString results = QString("<Configured with %1 properties>\n").arg(_pData->properties().size());
    for (openspace::Profile::Property p : _pData->properties()) {
        results += "    " + QString(p.name.c_str()) + " = ";
        results += QString(p.value.c_str()) + "\n";
    }
    return results;
}

QString ProfileEdit::summarizeText_keybindings() {
    if (_pData == nullptr) {
        return "";
    }
    QString results =
        QString("<Configured with %1 keybindings>\n").arg(_pData->keybindings().size());
    for (openspace::Profile::Keybinding k : _pData->keybindings()) {
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

QString ProfileEdit::summarizeText_assets() {
    if (_pData == nullptr) {
        return "";
    }
    QString results = QString("<Configured with %1 assets>\n").arg(_pData->assets().size());
    for (openspace::Profile::Asset a : _pData->assets()) {
        results += "    " + QString(a.path.c_str()) + "    ";
        results += QString(a.name.c_str()) + "\n";
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
        else if (_pData->time().value().type == openspace::Profile::Time::Type::Relative) {
            results = "Relative time: ";
        }
        results += QString(_pData->time().value().time.c_str());
    }
    return results;
}

QString ProfileEdit::summarizeText_deltaTimes() {
    if (_pData == nullptr) {
        return "";
    }
    QString results =
        QString("<Configured with %1 delta times>\n").arg(_pData->deltaTimes().size());
    for (size_t i = 0; i < _pData->deltaTimes().size(); ++i) {
        results += _deltaTimes->createSummaryForDeltaTime(i,
            _pData->deltaTimes().at(i), false);
        results += "\t" + QString::number(_pData->deltaTimes().at(i)) + "\n";
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
                results += QString(nav.aim.c_str()) + " ";
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

void ProfileEdit::cancel() {
    reject();
}

void ProfileEdit::approved() {
}
