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
#include <openspace/scene/profile.h>
#include <errordialog.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class ProfileEdit;
}
QT_END_NAMESPACE

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
    void cancel();
    void approved();

public:
    explicit ProfileEdit(openspace::Profile* profile, const std::string reportedAssets,
        std::vector<std::string>& profilesReadOnly, QWidget *parent = nullptr);
    ~ProfileEdit();
    void setProfileName(QString profileToSet);
    bool wasSaved();
    std::string specifiedFilename();
    void keyPressEvent(QKeyEvent *evt);

private:
    void initSummaryTextForEachCategory();
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
    void labelText(openspace::Profile* pData, int size, QString title, QLabel* pLabel);
    bool isReadOnly(std::string profileToSave);

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
    openspace::Profile* _pData;
    const std::string _reportedAssets;
    bool _saveSelected = false;
    std::vector<std::string> _profilesReadOnly;
};

#endif // PROFILEEDIT_H
