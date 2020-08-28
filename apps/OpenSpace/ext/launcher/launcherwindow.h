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

#ifndef __OPENSPACE_LAUNCHER___LAUNCHERWINDOW___H__
#define __OPENSPACE_LAUNCHER___LAUNCHERWINDOW___H__

#include <QApplication>
#include <QMainWindow>
#include "editorwindow.h"
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

public:
    LauncherWindow(std::string basePath, QWidget* parent = nullptr);
    ~LauncherWindow();
    void receiveAssets(std::vector<std::string> results);

private:
    void populateProfilesList();
    void populateWindowConfigsList();
    Ui::LauncherWindow *ui;
    editorwindow* myEditorWindow;
    filesystemAccess _fileAccess_profiles;
    filesystemAccess _fileAccess_winConfigs;
    QString _basePath;
};
#endif // __OPENSPACE_LAUNCHER___LAUNCHERWINDOW___H__
