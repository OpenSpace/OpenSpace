/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include "profile/assetedit.h"

#include "profile/horizonsdialog.h"
#include "profile/line.h"
#include <openspace/scene/asset.h>
#include <QDialogButtonBox>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QVBoxLayout>
#include <QWidget>

AssetEdit::AssetEdit(const std::string& assetName, QWidget* parent)
    : QDialog(parent)
{
    setWindowTitle("Asset Editor");
    createWidgets(assetName);
}

void AssetEdit::createWidgets(const std::string& assetName) {
    QBoxLayout* layout = new QVBoxLayout(this);
    QBoxLayout* leftLayout = new QVBoxLayout;
    {
        QBoxLayout* container = new QHBoxLayout;
        QLabel* assetLabel = new QLabel("Asset Name:");
        assetLabel->setObjectName("profile");
        container->addWidget(assetLabel);

        _assetEdit = new QLineEdit(QString::fromStdString(assetName));
        container->addWidget(_assetEdit);

        layout->addLayout(container);
    }
    layout->addWidget(new Line);
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        _horizonsLabel = new QLabel("Horizons");
        _horizonsLabel->setObjectName("heading");
        container->addWidget(_horizonsLabel, 0, 0);

        QPushButton* editHorizons = new QPushButton("Edit");
        connect(
            editHorizons, &QPushButton::clicked,
            this, &AssetEdit::openHorizons
        );
        container->addWidget(editHorizons, 0, 2);

        leftLayout->addLayout(container);
    }
    layout->addLayout(leftLayout, 3);
    layout->addWidget(new Line);
    {
        QBoxLayout* footer = new QHBoxLayout;
        _errorMsg = new QLabel;
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footer->addWidget(_errorMsg);

        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(buttons, &QDialogButtonBox::accepted, this, &AssetEdit::approved);
        connect(buttons, &QDialogButtonBox::rejected, this, &AssetEdit::reject);
        footer->addWidget(buttons);
        layout->addLayout(footer);
    }
}

void AssetEdit::openHorizons() {
    _errorMsg->clear();
    HorizonsDialog(this).exec();
}

void AssetEdit::approved() {
    std::string assetName = _assetEdit->text().toStdString();
    if (assetName.empty()) {
        _errorMsg->setText("Asset name must be specified");
        return;
    }
    accept();
}
