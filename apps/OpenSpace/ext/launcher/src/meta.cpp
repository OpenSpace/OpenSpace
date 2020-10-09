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

#include <openspace/scene/profile.h>
#include "meta.h"
#include "./ui_meta.h"
#include <algorithm>
#include <QKeyEvent>

meta::meta(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::meta)
    , _imported(imported)
{
    ui->setupUi(this);

    if (_imported->meta().has_value()) {
        ui->line_name->setText(QString(_imported->meta().value().name->c_str()));
        ui->line_version->setText(QString(_imported->meta().value().version->c_str()));
        ui->text_description->setText(
            QString(_imported->meta().value().description->c_str())
        );
        ui->line_author->setText(QString(_imported->meta().value().author->c_str()));
        ui->line_url->setText(QString(_imported->meta().value().url->c_str()));
        ui->line_license->setText(QString(_imported->meta().value().license->c_str()));
    }
    else {
        ui->line_name->setText("");
        ui->line_version->setText("");
        ui->text_description->setText("");
        ui->line_author->setText("");
        ui->line_url->setText("");
        ui->line_license->setText("");
    }

    ui->text_description->setTabChangesFocus(true);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(save()));
}

void meta::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    QDialog::keyPressEvent(evt);
}

void meta::save() {
    if (!areAllEntriesBlank()) {
        openspace::Profile::Meta m;
        m.name = ui->line_name->text().toUtf8().constData();
        m.version = ui->line_version->text().toUtf8().constData();
        std::string desc = ui->text_description->toPlainText().toUtf8().constData();
        std::replace(desc.begin(), desc.end(), '\n', ' ');
        m.description = desc;
        m.author = ui->line_author->text().toUtf8().constData();
        m.url = ui->line_url->text().toUtf8().constData();
        m.license = ui->line_license->text().toUtf8().constData();
        _imported->setMeta(m);
    }
    else {
        _imported->clearMeta();
    }
    accept();
}

bool meta::areAllEntriesBlank() {
    bool blank = true;
    if (ui->line_name->text().length() > 0) {
        blank = false;
    }
    if (ui->line_version->text().length() > 0) {
        blank = false;
    }
    if (ui->text_description->toPlainText().length() > 0) {
        blank = false;
    }
    if (ui->line_author->text().length() > 0) {
        blank = false;
    }
    if (ui->line_url->text().length() > 0) {
        blank = false;
    }
    if (ui->line_license->text().length() > 0) {
        blank = false;
    }
    return blank;
}

meta::~meta() {
    delete ui;
}
