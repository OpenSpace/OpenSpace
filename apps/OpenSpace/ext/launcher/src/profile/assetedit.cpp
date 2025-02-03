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

    _layout->addWidget(new Line);
    {
        QBoxLayout* footer = new QHBoxLayout;
        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(buttons, &QDialogButtonBox::accepted, this, &AssetEdit::approved);
        connect(buttons, &QDialogButtonBox::rejected, this, &AssetEdit::reject);
        footer->addWidget(buttons);
        _layout->addLayout(footer);
    }
}


void AssetEdit::openHorizons() {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    HorizonsDialog* horizonsDialog = new HorizonsDialog(this);
    horizonsDialog->exec();
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

void AssetEdit::approved() {
    accept();
}
