#ifndef PROFILEEDIT_H
#define PROFILEEDIT_H

#include <QDialog>
#include <QWidget>
#include "meta.h"
#include "properties.h"
#include "osmodules.h"
#include "keybindings.h"
#include "assets.h"
#include "ostime.h"
#include "addedscripts.h"
#include "deltatimes.h"
#include "camera.h"
#include "marknodes.h"
#include "ostime.h"

QT_BEGIN_NAMESPACE
namespace Ui {
class ProfileEdit;
}
QT_END_NAMESPACE

struct ProfileBlock {
    Meta& _metaData;
    std::vector<Module>& _moduleData;
    std::vector<Asset>& _assetData;
    std::string& _reportAssetsInFilesystem;
    std::vector<Property>& _propsData;
    std::vector<Keybinding>& _keybindingsData;
    DeltaTimes& _deltaTimesData;
    OSTime& _timeData;
    Camera& _cameraData;
    std::vector<std::string>& _markNodesData;
    std::string& _addedScriptsData;
};

class ProfileEdit : public QDialog
{
    Q_OBJECT

public slots:
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

public:
    explicit ProfileEdit(ProfileBlock imported, QWidget *parent = nullptr);
    ~ProfileEdit();
    void setProfileName(QString profileToSet);

private:
    QString summarizeText_meta();
    QString summarizeText_modules();
    QString summarizeText_assets();
    QString summarizeText_properties();
    QString summarizeText_keybindings();
    QString summarizeText_deltaTimes();
    QString summarizeText_time();
    QString summarizeText_camera();
    QString summarizeText_markNodes();
    QString summarizeText_addedScripts();

    Ui::ProfileEdit *ui;
    QWidget* _parent;
    meta* _meta;
    properties* _properties;
    osmodules* _modules;
    keybindings* _keybindings;
    assets* _assets;
    ostime* _time;
    addedScripts* _addedScripts;
    deltaTimes* _deltaTimes;
    camera* _camera;
    markNodes* _markNodes;

    ProfileBlock _pData;
};

#endif // PROFILEEDIT_H
