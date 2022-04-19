/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include "profile/scriptlogdialog.h"

#include "profile/line.h"
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/scene/profile.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <QGridLayout>
#include <QDialogButtonBox>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QFile>
#include <QPushButton>
#include <QTextStream>

ScriptlogDialog::ScriptlogDialog(QWidget* parent)
    : QDialog(parent)
{
    setWindowTitle("Scriptlog");
    createWidgets();

    loadScriptFile();
}

void ScriptlogDialog::createWidgets() {
    //         Column 0              Column 1
    //  *-------------------------*------------*
    //  | Title                                |
    //  *--------------------------------------*
    //  | Filter scripts          * Reload     |
    //  *--------------------------------------*
    //  | Script list                          |
    //  *--------------------------------------*
    //  *                         Save Cancel  *
    //  *-------------------------*------------*

    QGridLayout* layout = new QGridLayout;
    {
        QLabel* heading = new QLabel(QString::fromStdString(
            fmt::format(
                "Choose commands from \"{}\"",
                openspace::global::configuration->scriptLog
            )
        ));
        heading->setObjectName("heading");
        layout->addWidget(heading, 0, 0, 1, 2);
    }

    _filter = new QLineEdit;
    _filter->setPlaceholderText("Filter the list of scripts");
    connect(
        _filter, &QLineEdit::textEdited,
        this, &ScriptlogDialog::updateScriptList
    );
    layout->addWidget(_filter, 1, 0);

    _reloadFile = new QPushButton("Reload");
    _reloadFile->setToolTip("Reload the script log file");
    connect(
        _reloadFile, &QPushButton::clicked,
        this, &ScriptlogDialog::loadScriptFile
    );
    layout->addWidget(_reloadFile, 1, 1);

    _scriptlogList = new QListWidget;
    _scriptlogList->setSelectionMode(QAbstractItemView::SelectionMode::MultiSelection);
    layout->addWidget(_scriptlogList, 2, 0, 1, 2);

    layout->addWidget(new Line);
    {
        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(
            buttons, &QDialogButtonBox::accepted,
            this, &ScriptlogDialog::saveChosenScripts
        );
        connect(
            buttons, &QDialogButtonBox::rejected,
            this, &ScriptlogDialog::reject
        );
        layout->addWidget(buttons, 3, 0, 1, 2);
    }

    setLayout(layout);
}

void ScriptlogDialog::loadScriptFile() {
    std::string log = absPath(openspace::global::configuration->scriptLog).string();
    QFile file(QString::fromStdString(log));
    if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        QTextStream in(&file);
        while (!in.atEnd()) {
            QString line = in.readLine();
            // removing return from a few statments
            // these are usually generated by gui panels
            line.remove(QRegularExpression("^return "));
            if (!line.isEmpty()) {
                _scripts.push_back(line.toStdString());
            }
        }
    }
    updateScriptList();
}

void ScriptlogDialog::updateScriptList() {
    std::string filter = _filter->text().toStdString();
    QListWidgetItem* curr = _scriptlogList->currentItem();
    std::string selection;
    if (curr) {
        selection = curr->text().toStdString();
    }
    int index = -1;
    _scriptlogList->clear();
    for (const std::string& script : _scripts) {
        if (script.find(filter) != std::string::npos) {
            if (script == selection && index == -1) {
                index = _scriptlogList->count();
            }
            _scriptlogList->addItem(QString::fromStdString(script));
        }
    }
    _scriptlogList->setCurrentRow(index != -1 ? index : 0);
}

void ScriptlogDialog::saveChosenScripts() {
    std::string chosenScripts;
    QList<QListWidgetItem*> itemList = _scriptlogList->selectedItems();
    for (int i = 0; i < itemList.size(); ++i) {
        chosenScripts += itemList.at(i)->text().toStdString();
        if (i < itemList.size()) {
            chosenScripts += "\n";
        }
    }
    emit scriptsSelected(chosenScripts);

    accept();
}
