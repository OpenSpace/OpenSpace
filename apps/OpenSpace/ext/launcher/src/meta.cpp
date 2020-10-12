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

#include "meta.h"

#include <openspace/scene/profile.h>
#include <QDialogButtonBox>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QTextEdit>
#include <QVBoxLayout>
#include <algorithm>

Meta::Meta(openspace::Profile* profile, QWidget *parent)
    : QDialog(parent)
    , _profile(profile)
{
    setWindowTitle("Meta");

    QBoxLayout* layout = new QVBoxLayout(this);
    layout->addWidget(new QLabel("Name"));
    _nameEdit = new QLineEdit;
    layout->addWidget(_nameEdit);

    layout->addWidget(new QLabel("Version"));
    _versionEdit = new QLineEdit;
    layout->addWidget(_versionEdit);

    layout->addWidget(new QLabel("Description"));
    _descriptionEdit = new QTextEdit;
    _descriptionEdit->setTabChangesFocus(true);
    layout->addWidget(_descriptionEdit);

    layout->addWidget(new QLabel("Author"));
    _authorEdit = new QLineEdit;
    layout->addWidget(_authorEdit);
    
    layout->addWidget(new QLabel("URL"));
    _urlEdit = new QLineEdit;
    layout->addWidget(_urlEdit);

    layout->addWidget(new QLabel("License"));
    _licenseEdit = new QLineEdit;
    layout->addWidget(_licenseEdit);

    QDialogButtonBox* buttons = new QDialogButtonBox;
    buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
    QObject::connect(buttons, &QDialogButtonBox::accepted, this, &Meta::save);
    QObject::connect(buttons, &QDialogButtonBox::rejected, this, &Meta::reject);
    layout->addWidget(buttons);


    if (_profile->meta().has_value()) {
        openspace::Profile::Meta meta = *_profile->meta();
        if (meta.name.has_value()) {
            _nameEdit->setText(QString::fromStdString(*meta.name));
        }
        if (meta.version.has_value()) {
            _versionEdit->setText(QString::fromStdString(*meta.version));
        }
        if (meta.description.has_value()) {
            _descriptionEdit->setText(QString::fromStdString(*meta.description));
        }
        if (meta.author.has_value()) {
            _authorEdit->setText(QString::fromStdString(*meta.author));
        }
        if (meta.url.has_value()) {
            _urlEdit->setText(QString::fromStdString(*meta.url));
        }
        if (meta.license.has_value()) {
            _licenseEdit->setText(QString::fromStdString(*meta.license));
        }
    }
}

void Meta::save() {
    const bool allEmpty =
        _nameEdit->text().isEmpty() && _versionEdit->text().isEmpty() &&
        _descriptionEdit->toPlainText().isEmpty() && _authorEdit->text().isEmpty() &&
        _urlEdit->text().isEmpty() && _licenseEdit->text().isEmpty();

    if (!allEmpty) {
        openspace::Profile::Meta m;
        if (!_nameEdit->text().isEmpty()) {
            m.name = _nameEdit->text().toStdString();
        }
        if (!_versionEdit->text().isEmpty()) {
            m.version = _versionEdit->text().toStdString();
        }
        if (!_descriptionEdit->toPlainText().isEmpty()) {
            m.description = _descriptionEdit->toPlainText().toStdString();
        }
        if (!_authorEdit->text().isEmpty()) {
            m.author = _authorEdit->text().toStdString();
        }
        if (!_urlEdit->text().isEmpty()) {
            m.url = _urlEdit->text().toStdString();
        }
        if (!_licenseEdit->text().isEmpty()) {
            m.license = _licenseEdit->text().toStdString();
        }
        _profile->setMeta(m);
    }
    else {
        _profile->clearMeta();
    }
    accept();
}
