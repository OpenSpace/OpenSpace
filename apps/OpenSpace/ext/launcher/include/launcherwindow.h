#ifndef LAUNCHERWINDOW_H
#define LAUNCHERWINDOW_H

#include <QMainWindow>
#include <QString>
#include "profileedit.h"
#include "filesystemaccess.h"

QT_BEGIN_NAMESPACE
namespace Ui { class LauncherWindow; }
QT_END_NAMESPACE

class LauncherWindow : public QMainWindow
{
    Q_OBJECT

public slots:
    void openWindow_edit();
    void openWindow_new();
    void simulateData();

public:
    LauncherWindow(std::string basePath, QWidget *parent = nullptr);
    ~LauncherWindow();
    void receiveAssets(std::vector<std::string> results);

private:
    void populateProfilesList();
    void populateWindowConfigsList();
    void clearData();
    void initialize_meta();
    void initialize_modules();
    void initialize_assets();
    void initialize_properties();
    void initialize_keybindings();
    void initialize_deltaTimes();
    void initialize_time();
    void initialize_camera();
    void initialize_markNodes();
    void initialize_addedScripts();

    Ui::LauncherWindow *ui;
    ProfileEdit* myEditorWindow;
    filesystemAccess _fileAccess_profiles;
    filesystemAccess _fileAccess_winConfigs;

    openspace::Profile::Meta _metaData;
    std::vector<openspace::Profile::Module> _moduleData;
    std::vector<openspace::Profile::Asset> _assetData;
    filesystemAccess _filesystemAccess;
    std::string _reportAssetsInFilesystem;
    std::vector<openspace::Profile::Property> _propsData;
    std::vector<openspace::Profile::Keybinding> _keybindingsData;
    DeltaTimes _deltaTimesData;
    openspace::Profile::Time _timeData;
    Camera _cameraData;
    std::vector<std::string> _markNodesData;
    std::string _addedScriptsData;
    ProfileBlock _pData;
    QString _basePath;
};
#endif // LAUNCHERWINDOW_H
