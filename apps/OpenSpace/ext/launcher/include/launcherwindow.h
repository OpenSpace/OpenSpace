#ifndef LAUNCHERWINDOW_H
#define LAUNCHERWINDOW_H

#include <QMainWindow>
#include <QString>
#include "profileedit.h"
#include "filesystemaccess.h"
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui { class LauncherWindow; }
QT_END_NAMESPACE

class LauncherWindow : public QMainWindow
{
    Q_OBJECT

public slots:
    void openWindow_edit();
    void openWindow_new();

public:
    LauncherWindow(std::string basePath, QWidget *parent = nullptr);
    ~LauncherWindow();

private:
    void populateProfilesList();
    void populateWindowConfigsList();
    bool loadProfileFromFile(openspace::Profile*& p, std::string filename);
    void saveProfileToFile(const std::string& path, openspace::Profile* p);
    void displayErrorDialog(std::string msg);

    Ui::LauncherWindow *ui;
    ProfileEdit* myEditorWindow;
    errordialog* _myDialog;
    filesystemAccess _fileAccess_profiles;
    filesystemAccess _fileAccess_winConfigs;
    filesystemAccess _filesystemAccess;
    std::string _reportAssetsInFilesystem;
    QString _basePath;
};
#endif // LAUNCHERWINDOW_H
