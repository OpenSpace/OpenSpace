/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/format.h>
#include <QGridLayout>
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QFile>
#include <QPushButton>
#include <QTextStream>
#include <QTimer>

ScriptLogDialog::ScriptLogDialog(QWidget* parent, std::string filter)
    : QDialog(parent)
    , _scriptLogFile(openspace::global::configuration->scriptLog)
    , _fixedFilter(std::move(filter))
{
    setWindowTitle("Scriptlog");
    createWidgets();

    loadScriptFile();
}

void ScriptLogDialog::createWidgets() {
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

    QGridLayout* layout = new QGridLayout(this);
    {
        QLabel* heading = new QLabel(QString::fromStdString(std::format(
            "Choose commands from \"{}\"", _scriptLogFile
        )));
        heading->setObjectName("heading");
        layout->addWidget(heading, 0, 0, 1, 2);

        QPushButton* open = new QPushButton;
        open->setIcon(open->style()->standardIcon(QStyle::SP_FileIcon));
        connect(
            open, &QPushButton::clicked,
            [this, heading]() {
                const QString file = QFileDialog::getOpenFileName(
                    this,
                    "Select log file",
                    "",
                    "*.txt"
                );
                _scriptLogFile = file.toStdString();
        
                heading->setText(QString::fromStdString(std::format(
                    "Choose commands from \"{}\"", _scriptLogFile
                )));
                loadScriptFile();
            }
        );
        layout->addWidget(open, 0, 1, Qt::AlignRight);
    }

    _filter = new QLineEdit;
    _filter->setPlaceholderText("Filter the list of scripts");
    connect(_filter, &QLineEdit::textEdited, this, &ScriptLogDialog::updateScriptList);
    layout->addWidget(_filter, 1, 0);

    _reloadFile = new QPushButton("Reload");
    _reloadFile->setToolTip("Reload the script log file");
    connect(_reloadFile, &QPushButton::clicked, this, &ScriptLogDialog::loadScriptFile);
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
            this, &ScriptLogDialog::saveChosenScripts
        );
        connect(
            buttons, &QDialogButtonBox::rejected,
            this, &ScriptLogDialog::reject
        );
        layout->addWidget(buttons, 3, 0, 1, 2);
    }
}

void ScriptLogDialog::loadScriptFile() {
    _scripts.clear();

    const std::filesystem::path log = absPath(_scriptLogFile);
    QFile file = QFile(QString::fromStdString(log.string()));
    if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        QTextStream in(&file);
        while (!in.atEnd()) {
            QString line = in.readLine();
            // removing return from statements which are usually generated by gui panels
            line.remove(QRegularExpression("^return "));
            if (!line.isEmpty()) {
                _scripts.push_back(line.toStdString());
            }
        }
    }
    updateScriptList();

    // It's not possible to call the `scrollToBottom` function directly as it only will
    // scroll to the middle of the list. By doing it this way, we correctly end up at the
    // bottom of the list
    QTimer::singleShot(0, _scriptlogList, &QListWidget::scrollToBottom);
}

void ScriptLogDialog::updateScriptList() {
    const std::string filter = _filter->text().toStdString();
    QListWidgetItem* curr = _scriptlogList->currentItem();
    std::string selection;
    if (curr) {
        selection = curr->text().toStdString();
    }
    int index = -1;
    _scriptlogList->clear();
    for (const std::string& script : _scripts) {
        const bool foundDynamic = script.find(filter) != std::string::npos;
        const bool foundStatic =
            _fixedFilter.empty() ? true : script.find(_fixedFilter) != std::string::npos;
        if (foundDynamic && foundStatic) {
            if (script == selection && index == -1) {
                index = _scriptlogList->count();
            }
            _scriptlogList->addItem(QString::fromStdString(script));
        }
    }
}

void ScriptLogDialog::saveChosenScripts() {
    std::vector<std::string> chosenScripts;
    QList<QListWidgetItem*> itemList = _scriptlogList->selectedItems();

    // The selected items are returned in order of **selection** not in row-order, so we
    // need to sort them first
    std::sort(
        itemList.begin(),
        itemList.end(),
        [this](QListWidgetItem* lhs, QListWidgetItem* rhs) {
            return _scriptlogList->row(lhs) < _scriptlogList->row(rhs);
        }
    );

    for (QListWidgetItem* item : itemList) {
        chosenScripts.push_back(item->text().toStdString());
    }
    emit scriptsSelected(chosenScripts);
    accept();
}
