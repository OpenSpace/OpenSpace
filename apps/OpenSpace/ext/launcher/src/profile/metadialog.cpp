/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "profile/metadialog.h"

#include "profile/line.h"
#include <openspace/scene/profile.h>
#include <QDialogButtonBox>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QTextEdit>
#include <QVBoxLayout>
#include <algorithm>

MetaDialog::MetaDialog(QWidget* parent, std::optional<openspace::Profile::Meta>* meta)
    : QDialog(parent)
    , _meta(meta)
{
    setWindowTitle("Meta");
    createWidgets();

    if (_meta->has_value()) {
        const openspace::Profile::Meta& m = **_meta;
        if (m.name.has_value()) {
            _nameEdit->setText(QString::fromStdString(*m.name));
        }
        if (m.version.has_value()) {
            _versionEdit->setText(QString::fromStdString(*m.version));
        }
        if (m.description.has_value()) {
            _descriptionEdit->setText(QString::fromStdString(*m.description));
        }
        if (m.author.has_value()) {
            _authorEdit->setText(QString::fromStdString(*m.author));
        }
        if (m.url.has_value()) {
            _urlEdit->setText(QString::fromStdString(*m.url));
        }
        if (m.license.has_value()) {
            _licenseEdit->setText(QString::fromStdString(*m.license));
        }
    }
}

void MetaDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->addWidget(new QLabel("Name"));
    _nameEdit = new QLineEdit;
    _nameEdit->setAccessibleName("Profile name");
    layout->addWidget(_nameEdit);

    layout->addWidget(new QLabel("Version"));
    _versionEdit = new QLineEdit;
    _versionEdit->setAccessibleName("Profile version number");
    layout->addWidget(_versionEdit);

    layout->addWidget(new QLabel("Description"));
    _descriptionEdit = new QTextEdit;
    _descriptionEdit->setAcceptRichText(false);
    _descriptionEdit->setTabChangesFocus(true);
    _descriptionEdit->setAccessibleName("Profile description");
    layout->addWidget(_descriptionEdit);

    layout->addWidget(new QLabel("Author"));
    _authorEdit = new QLineEdit;
    _authorEdit->setAccessibleName("Profile author name");
    layout->addWidget(_authorEdit);

    layout->addWidget(new QLabel("URL"));
    _urlEdit = new QLineEdit;
    _urlEdit->setAccessibleName("Profile url");
    layout->addWidget(_urlEdit);

    layout->addWidget(new QLabel("License"));
    _licenseEdit = new QLineEdit;
    _licenseEdit->setAccessibleName("Profile license");
    layout->addWidget(_licenseEdit);

    layout->addWidget(new Line);

    QDialogButtonBox* buttons = new QDialogButtonBox;
    buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
    QObject::connect(buttons, &QDialogButtonBox::accepted, this, &MetaDialog::save);
    QObject::connect(buttons, &QDialogButtonBox::rejected, this, &MetaDialog::reject);
    layout->addWidget(buttons);
}

void MetaDialog::save() {
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
        *_meta = std::move(m);
    }
    else {
        *_meta = std::nullopt;
    }
    accept();
}
