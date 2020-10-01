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

#ifndef UI_ASSETS_H
#define UI_ASSETS_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QLabel>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QTreeView>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_assets
{
public:
    QVBoxLayout *verticalLayout;
    QTreeView *treeView;
    QTextEdit *textEdit;
    QFrame *line;
    QDialogButtonBox *buttonBox;
    QLabel *label_selection;
    QLabel *label_summary;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;

    void setupUi(QDialog *assets)
    {
        if (assets->objectName().isEmpty())
            assets->setObjectName(QString::fromUtf8("assets"));
        assets->resize(610, 479);
        assets->setMinimumSize(QSize(400, 400));
        assets->setMaximumSize(QSize(1600, 950));
        label_selection= new QLabel(assets);
        label_selection->setObjectName(QString::fromUtf8("label_selection"));
        label_summary = new QLabel(assets);
        label_summary->setObjectName(QString::fromUtf8("label_summary"));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        label_selection->setFont(font1);
        label_summary->setFont(font1);
        verticalLayout = new QVBoxLayout(assets);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        treeView = new QTreeView(assets);
        treeView->setObjectName(QString::fromUtf8("treeView"));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        treeView->setFont(font);
        //treeView->setStyleSheet(QString::fromUtf8("background-color: rgb(85, 87, 83);"));
        treeView->setAlternatingRowColors(false);
        treeView->setAnimated(false);
        verticalLayout->addWidget(label_selection);
        verticalLayout->addWidget(treeView);

        textEdit = new QTextEdit(assets);
        textEdit->setObjectName(QString::fromUtf8("textEdit"));

        verticalLayout->addWidget(label_summary);
        verticalLayout->addWidget(textEdit);

        line = new QFrame(assets);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(assets);
        label_error->setObjectName(QString::fromUtf8("label_error"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(label_error->sizePolicy().hasHeightForWidth());
        label_error->setSizePolicy(sizePolicy1);
        label_error->setMinimumSize(QSize(200, 40));
        label_error->setMaximumSize(QSize(800, 40));
        label_error->setFont(fontE);
        label_error->setWordWrap(true);
        hLay_bottom_buttonBox->addWidget(label_error);

        buttonBox = new QDialogButtonBox(assets);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);

        retranslateUi(assets);
        //QObject::connect(buttonBox, SIGNAL(accepted()), assets, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), assets, SLOT(reject()));

        QMetaObject::connectSlotsByName(assets);
    } // setupUi

    void retranslateUi(QDialog *assets)
    {
        assets->setWindowTitle(QCoreApplication::translate("assets", "Dialog", nullptr));
        label_selection->setText(QCoreApplication::translate("assets", "Select Assets from /data/assets:", nullptr));
        label_summary->setText(QCoreApplication::translate("assets", "Selection summary:", nullptr));
        label_error->setText(QCoreApplication::translate("assets", "", nullptr));
#if QT_CONFIG(tooltip)
        treeView->setToolTip(QCoreApplication::translate("assets", "<html><head/><body><p>Expand arrow entries to browse assets in this OpenSpace installation. Enable checkbox to include an asset. Those assets highlighted in red are present in the profile but do not exist in this OpenSpace installation.</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
    } // retranslateUi

};

namespace Ui {
    class assets: public Ui_assets {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ASSETS_H
