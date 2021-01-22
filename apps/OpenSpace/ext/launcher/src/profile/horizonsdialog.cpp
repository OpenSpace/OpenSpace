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

#include "profile/horizonsdialog.h"

#include "profile/line.h"
#include <QDialogButtonBox>
#include <QFileDialog>
#include <QHeaderView>
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>

HorizonsDialog::HorizonsDialog(QWidget* parent)
    : QDialog(parent)
{
    setWindowTitle("Horizons");
    createWidgets();
}

void HorizonsDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        QLabel* localLabel = new QLabel("Select a local Horizons file");
        localLabel->setObjectName("heading");
        container->addWidget(localLabel, 0, 0);

        QPushButton* horizonsFileButton = new QPushButton("Browse", this);
        connect(
            horizonsFileButton, &QPushButton::released,
            [this]() {
                openHorizonsFile();
            }
        );
        horizonsFileButton->setCursor(Qt::PointingHandCursor);
        container->addWidget(horizonsFileButton, 0, 2);

        layout->addLayout(container);
    }
    layout->addWidget(new Line);
    {
        QBoxLayout* footer = new QHBoxLayout;
        _errorMsg = new QLabel;
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footer->addWidget(_errorMsg);

        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(buttons, &QDialogButtonBox::accepted, this, &HorizonsDialog::approved);
        connect(buttons, &QDialogButtonBox::rejected, this, &HorizonsDialog::reject);
        footer->addWidget(buttons);
        layout->addLayout(footer);
    }
}

void HorizonsDialog::openHorizonsFile() {
    _horizonsFile = QFileDialog::getOpenFileName(
        this,
        tr("Open Horizons file"),
        "",
        tr("Horiozons file (*.dat)")
    ).toStdString();
}

void HorizonsDialog::approved() {
    accept();
}
