/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_UI_LAUNCHER___SCRIPTLOGDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___SCRIPTLOGDIALOG___H__

#include <QDialog>

class QLineEdit;
class QListWidget;
class QPushButton;

class ScriptlogDialog final : public QDialog {
Q_OBJECT
public:
    ScriptlogDialog(QWidget* parent);

signals:
    void scriptsSelected(std::string script);

private:
    void createWidgets();
    
    void loadScriptFile();
    void saveChosenScripts();
    
    void updateScriptList();

    QListWidget* _scriptlogList = nullptr;
    QLineEdit* _filter = nullptr;
    QPushButton* _reloadFile = nullptr;
    std::string _scriptLogFile;
    std::vector<std::string> _scripts;
};

#endif // __OPENSPACE_UI_LAUNCHER___SCRIPTLOGDIALOG___H__
