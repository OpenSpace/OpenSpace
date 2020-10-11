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

#include "addedscripts.h"
#include "ui_addedscripts.h"

#include <QEvent>
#include <QKeyEvent>
#include <iostream>
#include <sstream>

addedScripts::addedScripts(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::addedScripts)
    , _imported(imported)
{
    ui->setupUi(this);
    for (std::string s : imported->additionalScripts()) {
        _data += s + "\n";
    }
    ui->text_scripts->setText(QString(_data.c_str()));

    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseScript()));
}

void addedScripts::setScriptText(std::string s) {
    ui->text_scripts->setText(QString(s.c_str()));
}

void addedScripts::parseScript() {
    std::vector<std::string> tmpMultilineStringToVector;
    std::istringstream iss(ui->text_scripts->toPlainText().toUtf8().constData());
    while (!iss.eof())
    {
        std::string s;
        getline(iss, s);
        tmpMultilineStringToVector.push_back(s);
    }
    _imported->setAdditionalScripts(tmpMultilineStringToVector);
    accept();
}

void addedScripts::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    QDialog::keyPressEvent(evt);
}

addedScripts::~addedScripts() { }
