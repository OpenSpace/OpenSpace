/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include "metadatawidget.h"

#include <jasset.h>
#include <QComboBox>
#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QVBoxLayout>

namespace {
    constexpr int LabelColumnMinWidth = 80;
} // namespace

MetadataWidget::MetadataWidget(QWidget* parent, JAsset& asset)
    : QWidget(parent)
    , _asset(asset)
{
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    QLabel* header = new QLabel("METADATA", this);
    header->setObjectName("section-header");
    header->setContentsMargins(12, 8, 8, 4);
    layout->addWidget(header);

    QWidget* content = new QWidget(this);
    layout->addWidget(content);
    QGridLayout* grid = new QGridLayout(content);
    grid->setContentsMargins(12, 0, 8, 8);
    grid->setColumnMinimumWidth(0, LabelColumnMinWidth);
    grid->setColumnStretch(1, 1);
    grid->setHorizontalSpacing(8);
    grid->setVerticalSpacing(4);

    {
        // Left side label
        QLabel* fieldLabel = new QLabel("Name", content);
        fieldLabel->setObjectName("field-label");
        grid->addWidget(fieldLabel, 0, 0, Qt::AlignVCenter | Qt::AlignLeft);

        // Right side edit field
        _nameEdit = new QLineEdit(content);
        _nameEdit->setPlaceholderText("Asset name");
        connect(
            _nameEdit,
            &QLineEdit::textEdited,
            this,
            [this](const QString& text) {
                _asset.metadata.name = text.toStdString();
                emit assetModified();
            }
        );
        grid->addWidget(_nameEdit, 0, 1);
    }

    {
        // Left side label
        QLabel* fieldLabel = new QLabel("Version", content);
        fieldLabel->setObjectName("field-label");
        grid->addWidget(fieldLabel, 1, 0, Qt::AlignVCenter | Qt::AlignLeft);

        // Right side edit field
        _versionEdit = new QLineEdit(content);
        _versionEdit->setPlaceholderText("1.0.0");
        connect(
            _versionEdit,
            &QLineEdit::textEdited,
            this,
            [this](const QString& text) {
                _asset.metadata.version = text.toStdString();
                emit assetModified();
            }
        );
        grid->addWidget(_versionEdit, 1, 1);
    }

    {
        // Left side label
        QLabel* fieldLabel = new QLabel("Author", content);
        fieldLabel->setObjectName("field-label");
        grid->addWidget(fieldLabel, 2, 0, Qt::AlignVCenter | Qt::AlignLeft);

        // Right side edit field
        _authorEdit = new QLineEdit(content);
        _authorEdit->setPlaceholderText("Author name");
        connect(
            _authorEdit,
            &QLineEdit::textEdited,
            this,
            [this](const QString& text) {
                _asset.metadata.author = text.toStdString();
                emit assetModified();
            }
        );
        grid->addWidget(_authorEdit, 2, 1);
    }

    {
        // Description: multi-line plain text

        // Left side label
        QLabel* label = new QLabel("Description", content);
        label->setObjectName("field-label");
        grid->addWidget(label, 3, 0, Qt::AlignTop | Qt::AlignLeft);

        // Right side edit field
        _descriptionEdit = new QPlainTextEdit(content);
        _descriptionEdit->setPlaceholderText("Short summary");
        _descriptionEdit->setMinimumHeight(48);
        _descriptionEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        connect(
            _descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            [this]() {
                _asset.metadata.description =
                    _descriptionEdit->toPlainText().toStdString();
                emit assetModified();
            }
        );
        grid->addWidget(_descriptionEdit, 3, 1);
    }

    {
        // License: editable combo with preset items

        // Left side label
        QLabel* label = new QLabel("License", content);
        label->setObjectName("field-label");
        grid->addWidget(label, 4, 0, Qt::AlignVCenter | Qt::AlignLeft);

        // Right side combobox
        _licenseCombo = new QComboBox(content);
        _licenseCombo->setEditable(true);
        _licenseCombo->lineEdit()->setPlaceholderText("e.g. MIT");
        _licenseCombo->addItems({
            "", "MIT", "Apache 2.0", "GPL 3.0", "CC BY 4.0", "Proprietary"
        });
        _licenseCombo->setCurrentIndex(0);
        connect(
            _licenseCombo,
            &QComboBox::currentTextChanged,
            this,
            [this](const QString& text) {
                _asset.metadata.license = text.toStdString();
                emit assetModified();
            }
        );
        grid->addWidget(_licenseCombo, 4, 1);
    }

    layout->addStretch();
}

void MetadataWidget::refresh() {
    const AssetMetadata& metadata = _asset.metadata;
    _nameEdit->setText(QString::fromStdString(metadata.name));
    _versionEdit->setText(QString::fromStdString(metadata.version));
    _authorEdit->setText(QString::fromStdString(metadata.author));

    _descriptionEdit->blockSignals(true);
    _descriptionEdit->setPlainText(QString::fromStdString(metadata.description));
    _descriptionEdit->blockSignals(false);

    _licenseCombo->blockSignals(true);
    const QString license = QString::fromStdString(metadata.license);
    const int index = _licenseCombo->findText(license);
    if (index >= 0) {
        _licenseCombo->setCurrentIndex(index);
    }
    else {
        _licenseCombo->setCurrentText(license);
    }
    _licenseCombo->blockSignals(false);
}
