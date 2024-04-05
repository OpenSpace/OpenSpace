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

#include "profile/additionalscriptsdialog.h"

#include "profile/line.h"
#include "profile/scriptlogdialog.h"
#include <openspace/scene/profile.h>
#include <ghoul/misc/stringhelper.h>
#include <QDialogButtonBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QTextEdit>
#include <QVBoxLayout>
#include <sstream>

AdditionalScriptsDialog::AdditionalScriptsDialog(QWidget* parent,
                                                 std::vector<std::string>* scripts)
    : QDialog(parent)
    , _scripts(scripts)
    , _scriptsData(*_scripts)
{
    setWindowTitle("Additional Scripts");
    createWidgets();

    const std::string scriptText = std::accumulate(
        _scriptsData.begin(), _scriptsData.end(),
        std::string(),
        [](std::string lhs, const std::string& rhs) { return lhs + rhs + '\n'; }
    );
    _textScripts->setText(QString::fromStdString(scriptText));
    _textScripts->moveCursor(QTextCursor::MoveOperation::End);
}

void AdditionalScriptsDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QLabel* heading = new QLabel("Additional Lua Scripts for Configuration");
        heading->setObjectName("heading");
        layout->addWidget(heading);
    }

    _textScripts = new QTextEdit;
    _textScripts->setAcceptRichText(false);
    layout->addWidget(_textScripts, 1);

    _chooseScriptsButton = new QPushButton("Choose Scripts");
    connect(
        _chooseScriptsButton, &QPushButton::clicked,
        this, &AdditionalScriptsDialog::chooseScripts
    );
    layout->addWidget(_chooseScriptsButton);

    layout->addWidget(new Line);

    {
        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(
            buttons, &QDialogButtonBox::accepted,
            this, &AdditionalScriptsDialog::parseScript
        );
        connect(
            buttons, &QDialogButtonBox::rejected,
            this, &AdditionalScriptsDialog::reject
        );
        layout->addWidget(buttons);
    }
}

void AdditionalScriptsDialog::parseScript() {
    std::vector<std::string> additionalScripts;
    std::istringstream iss(_textScripts->toPlainText().toStdString());
    while (!iss.eof()) {
        std::string s;
        ghoul::getline(iss, s);
        additionalScripts.push_back(std::move(s));
    }
    *_scripts = std::move(additionalScripts);
    accept();
}

void AdditionalScriptsDialog::chooseScripts() {
    ScriptlogDialog d(this);
    connect(
        &d, &ScriptlogDialog::scriptsSelected,
        this, &AdditionalScriptsDialog::appendScriptsToTextfield
    );
    d.exec();
}

void AdditionalScriptsDialog::appendScriptsToTextfield(
                                                  const std::vector<std::string>& scripts)
{
    for (const std::string& script : scripts) {
        _textScripts->append(QString::fromStdString(script));
    }
}
