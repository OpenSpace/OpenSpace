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

#include "profile/additionalscriptsdialog.h"

#include <openspace/scene/profile.h>
#include <QDialogButtonBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QTextEdit>
#include <QVBoxLayout>

AdditionalScriptsDialog::AdditionalScriptsDialog(openspace::Profile* profile,
                                                 QWidget* parent)
    : QDialog(parent)
    , _profile(profile)
{
    setWindowTitle("Additional Scripts");

    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QLabel* heading = new QLabel("Addtional Lua Scripts for Configuration");
        heading->setObjectName("heading");
        layout->addWidget(heading);
    }

    _textScripts = new QTextEdit;
    layout->addWidget(_textScripts, 1);

    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        layout->addWidget(line);
    }

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

    std::vector<std::string> scripts = _profile->additionalScripts();
    std::string scpts = std::accumulate(
        scripts.begin(), scripts.end(),
        std::string(), [](std::string lhs, std::string rhs) { return lhs + rhs + '\n'; }
    );
    _textScripts->setText(QString::fromStdString(std::move(scpts)));
    _textScripts->moveCursor(QTextCursor::MoveOperation::End);
}

void AdditionalScriptsDialog::parseScript() {
    std::vector<std::string> tmpMultilineStringToVector;
    std::istringstream iss(_textScripts->toPlainText().toUtf8().constData());
    while (!iss.eof()) {
        std::string s;
        getline(iss, s);
        tmpMultilineStringToVector.push_back(s);
    }
    _profile->setAdditionalScripts(tmpMultilineStringToVector);
    accept();
}
