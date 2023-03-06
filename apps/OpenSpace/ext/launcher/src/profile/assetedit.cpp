/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
#include <QComboBox>
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QVBoxLayout>
#include <QWidget>
#include <filesystem>
#include <string>

AssetEdit::AssetEdit(QWidget* parent)
    : QDialog(parent)
{
    setWindowTitle("Asset Editor");
    createWidgets();
}

void AssetEdit::createWidgets() {
    _layout = new QVBoxLayout(this);
    {
        QLabel* heading = new QLabel("The Asset Editor is coming in a future release");
        heading->setObjectName("heading");
        _layout->addWidget(heading);
    }
    {
        QPushButton* generateButton = new QPushButton("Generate Horizons File");
        connect(
            generateButton, &QPushButton::released,
            this, &AssetEdit::openHorizons
        );
        generateButton->setDefault(false);

        // In order to generate a Horizons File the Space module is required
#ifndef OPENSPACE_MODULE_SPACE_ENABLED
        generateButton->setEnabled(false);
        generateButton->setToolTip(
            "Cannot generate Horizons file without the space module enabled"
        );
#else
        generateButton->setCursor(Qt::PointingHandCursor);
#endif // OPENSPACE_MODULE_SPACE_ENABLED

        _layout->addWidget(generateButton);
    }
    //{
    //    QBoxLayout* container = new QHBoxLayout(this);
    //    QLabel* assetLabel = new QLabel("Asset Name:");
    //    assetLabel->setObjectName("profile");
    //    container->addWidget(assetLabel);

    //    _nameEdit = new QLineEdit();
    //    container->addWidget(_nameEdit);

    //    _layout->addLayout(container);
    //}
    //_layout->addWidget(new Line);
    //{
    //    QBoxLayout* container = new QHBoxLayout(this);
    //    _components = new QComboBox(this);
    //    _components->addItems(_supportedComponents);
    //    _components->setCurrentIndex(0);
    //    container->addWidget(_components);

    //    QPushButton* addButton = new QPushButton("Add", this);
    //    connect(addButton, &QPushButton::clicked, this, &AssetEdit::openComponent);
    //    addButton->setCursor(Qt::PointingHandCursor);
    //    container->addWidget(addButton);

    //    _layout->addLayout(container);
    //}
    _layout->addWidget(new Line);
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
        _layout->addLayout(footer);
    }
}

//void AssetEdit::openComponent() {
//    switch (_components->currentIndex()) {
//        case 0:
//            _errorMsg->setText("Choose a component to add to the asset");
//            break;
//        case 1: {
//            QBoxLayout* horizonsLayout = new QVBoxLayout(this);
//            {
//                QLabel* label = new QLabel("Horizons Translation:", this);
//                label->setObjectName("heading");
//                horizonsLayout->addWidget(label);
//            }
//            {
//                QBoxLayout* container = new QHBoxLayout(this);
//                QLabel* fileLabel = new QLabel("File path:", this);
//                container->addWidget(fileLabel);
//
//                _horizonsFileEdit = new QLineEdit(this);
//                container->addWidget(_horizonsFileEdit);
//
//                QPushButton* fileButton = new QPushButton("Browse", this);
//                connect(
//                    fileButton,
//                    &QPushButton::released,
//                    this,
//                    &AssetEdit::openHorizonsFile
//                );
//                fileButton->setCursor(Qt::PointingHandCursor);
//                container->addWidget(fileButton);
//
//                QPushButton* generateButton = new QPushButton("Generate", this);
//                connect(
//                    generateButton,
//                    &QPushButton::released,
//                    this,
//                    &AssetEdit::openHorizons
//                );
//
//                // In order to generate a Horizons File the Space module is required
//                #ifndef OPENSPACE_MODULE_SPACE_ENABLED
//                    generateButton->setEnabled(false);
//                    generateButton->setToolTip(
//                        "Connot generate Horizons file without the space module enabled"
//                    );
//                #else
//                    generateButton->setCursor(Qt::PointingHandCursor);
//                #endif // OPENSPACE_MODULE_SPACE_ENABLED
//
//                container->addWidget(generateButton);
//                horizonsLayout->addLayout(container);
//            }
//            horizonsLayout->addWidget(new Line);
//            _layout->insertLayout(_layout->count() - 3, horizonsLayout);
//            break;
//        }
//        default:
//            _errorMsg->setText("Unkown component");
//            break;
//    }
//}
//
//void AssetEdit::openHorizonsFile() {
//    std::string filePath = QFileDialog::getOpenFileName(
//        this,
//        tr("Open Horizons file"),
//        "",
//        tr("Horiozons file (*.dat)")
//    ).toStdString();
//    _horizonsFile = std::filesystem::absolute(filePath);
//    _horizonsFileEdit->setText(QString(_horizonsFile.string().c_str()));
//}

void AssetEdit::openHorizons() {
    _errorMsg->clear();
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsDialog* horizonsDialog = new HorizonsDialog(this);
    horizonsDialog->exec();
    //_horizonsFile = horizonsDialog->file();
    //_horizonsFileEdit->setText(QString(_horizonsFile.string().c_str()));
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

void AssetEdit::approved() {
//    std::string assetName = _nameEdit->text().toStdString();
//    if (assetName.empty()) {
//        _errorMsg->setText("Asset name must be specified");
//        return;
//    }
    accept();
}
