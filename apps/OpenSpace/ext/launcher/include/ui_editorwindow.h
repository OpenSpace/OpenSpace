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

#ifndef __OPENSPACE_LAUNCHER_UI__EDITORWINDOW___H__
#define __OPENSPACE_LAUNCHER_UI__EDITORWINDOW___H__

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QTreeView>

QT_BEGIN_NAMESPACE

class Ui_editorwindow
{
public:
    QDialogButtonBox *buttonBox;
    QLineEdit *lineEditProfile;
    QLabel *labelProfileName;
    QLineEdit *lineEditStartTime;
    QTreeView *treeView;
    QLabel *labelSelectAssets;
    QLabel *labelStartTime;
    QLabel *labelFeaturedNodes;
    QTextEdit *textEditFeaturedNodes;
    QLabel *labelInitialAnchorNode;
    QLineEdit *lineEditInitialAnchorNode;
    QLabel *labelCustomizations;
    QTextEdit *textEditCustomizations;

    void setupUi(QDialog *editorwindow)
    {
        if (editorwindow->objectName().isEmpty()) {
            editorwindow->setObjectName(QString::fromUtf8("editorwindow"));
        }
        editorwindow->resize(509, 678);
        editorwindow->setStyleSheet(QString::fromUtf8(
            "background: rgb(46, 52, 54);\n"
            "color: white;\n"
            "\n"
            "QPushButton {\n"
            "  background: rgb(186, 189, 182);\n"
            "};\n"
            "\n"
            "QTreeView {\n"
            "    color: white;\n"
            "};\n"
            "")
        );
        buttonBox = new QDialogButtonBox(editorwindow);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(30, 630, 341, 32));
        buttonBox->setStyleSheet(QString::fromUtf8(
            "background: rgb(186, 189, 182);\n"
            "color: rgb(0, 0, 0);")
        );
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Save);
        lineEditProfile = new QLineEdit(editorwindow);
        lineEditProfile->setObjectName(QString::fromUtf8("lineEditProfile"));
        lineEditProfile->setGeometry(QRect(60, 40, 251, 25));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        lineEditProfile->setFont(font);
        lineEditProfile->setCursor(QCursor(Qt::IBeamCursor));
        lineEditProfile->setStyleSheet(QString::fromUtf8("color: rgb(255, 255, 255)"));
        labelProfileName = new QLabel(editorwindow);
        labelProfileName->setObjectName(QString::fromUtf8("labelProfileName"));
        labelProfileName->setGeometry(QRect(60, 20, 111, 17));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        labelProfileName->setFont(font1);
        lineEditStartTime = new QLineEdit(editorwindow);
        lineEditStartTime->setObjectName(QString::fromUtf8("lineEditStartTime"));
        lineEditStartTime->setGeometry(QRect(60, 340, 371, 25));
        lineEditStartTime->setFont(font);
        lineEditStartTime->setStyleSheet(QString::fromUtf8("color: rgb(255, 255, 255)"));
        treeView = new QTreeView(editorwindow);
        treeView->setObjectName(QString::fromUtf8("treeView"));
        treeView->setGeometry(QRect(60, 100, 400, 200));
        treeView->setFont(font1);
        treeView->setStyleSheet(QString::fromUtf8("background-color: rgb(85, 87, 83);"));
        treeView->setAlternatingRowColors(false);
        treeView->setAnimated(false);
        labelSelectAssets = new QLabel(editorwindow);
        labelSelectAssets->setObjectName(QString::fromUtf8("labelSelectAssets"));
        labelSelectAssets->setGeometry(QRect(60, 80, 111, 17));
        labelSelectAssets->setFont(font1);
        labelStartTime = new QLabel(editorwindow);
        labelStartTime->setObjectName(QString::fromUtf8("labelStartTime"));
        labelStartTime->setGeometry(QRect(60, 320, 111, 17));
        labelStartTime->setFont(font1);
        labelFeaturedNodes = new QLabel(editorwindow);
        labelFeaturedNodes->setObjectName(QString::fromUtf8("labelFeaturedNodes"));
        labelFeaturedNodes->setGeometry(QRect(60, 370, 131, 17));
        labelFeaturedNodes->setFont(font1);
        textEditFeaturedNodes = new QTextEdit(editorwindow);
        textEditFeaturedNodes->setObjectName(QString::fromUtf8("textEditFeaturedNodes"));
        textEditFeaturedNodes->setGeometry(QRect(60, 390, 321, 51));
        labelInitialAnchorNode = new QLabel(editorwindow);
        labelInitialAnchorNode->setObjectName(QString::fromUtf8(
            "labelInitialAnchorNode")
        );
        labelInitialAnchorNode->setGeometry(QRect(60, 450, 171, 17));
        labelInitialAnchorNode->setFont(font1);
        lineEditInitialAnchorNode = new QLineEdit(editorwindow);
        lineEditInitialAnchorNode->setObjectName(QString::fromUtf8(
            "lineEditInitialAnchorNode")
        );
        lineEditInitialAnchorNode->setGeometry(QRect(60, 480, 371, 25));
        lineEditInitialAnchorNode->setFont(font);
        labelCustomizations = new QLabel(editorwindow);
        labelCustomizations->setObjectName(QString::fromUtf8("labelCustomizations"));
        labelCustomizations->setGeometry(QRect(60, 520, 141, 17));
        labelCustomizations->setFont(font1);
        textEditCustomizations = new QTextEdit(editorwindow);
        textEditCustomizations->setObjectName(QString::fromUtf8(
            "textEditCustomizations")
        );
        textEditCustomizations->setGeometry(QRect(60, 550, 321, 51));

        retranslateUi(editorwindow);
        QObject::connect(buttonBox, SIGNAL(accepted()), editorwindow, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), editorwindow, SLOT(reject()));

        QMetaObject::connectSlotsByName(editorwindow);
    } // setupUi

    void retranslateUi(QDialog *editorwindow)
    {
        editorwindow->setWindowTitle(QApplication::translate(
            "editorwindow",
            "Dialog",
            nullptr)
        );
#ifndef QT_NO_TOOLTIP
        lineEditProfile->setToolTip(QApplication::translate(
            "editorwindow", 
            "<html><head/><body><p>Enter name for profile</p></body></html>",
            nullptr)
        );
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        lineEditProfile->setWhatsThis(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p><span style=\" color:#ffffff;\">"
            "Enter name</span></p></body></html>",
            nullptr)
        );
#endif // QT_NO_WHATSTHIS
        lineEditProfile->setText(QString());
        labelProfileName->setText(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Profile Name</span></p></body></html>",
            nullptr)
        );
#ifndef QT_NO_TOOLTIP
        lineEditStartTime->setToolTip(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p>Tip here...</p></body></html>",
            nullptr)
        );
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        lineEditStartTime->setWhatsThis(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p>Sample text</p></body></html>",
            nullptr)
        );
#endif // QT_NO_WHATSTHIS
        labelSelectAssets->setText(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Select Assets</span></p></body></html>",
            nullptr)
        );
        labelStartTime->setText(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Start Time</span></p></body></html>",
            nullptr)
        );
        labelFeaturedNodes->setText(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Featured Nodes</span></p></body></html>",
            nullptr)
        );
#ifndef QT_NO_TOOLTIP
        textEditFeaturedNodes->setToolTip(QString());
#endif // QT_NO_TOOLTIP
        labelInitialAnchorNode->setText(QApplication::translate("editorwindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Initial Anchor Node</span></p></body></html>",
            nullptr)
        );
#ifndef QT_NO_TOOLTIP
        lineEditInitialAnchorNode->setToolTip(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p>Tip here...</p></body></html>",
            nullptr)
        );
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        lineEditInitialAnchorNode->setWhatsThis(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p>Sample text</p></body></html>",
            nullptr)
        );
#endif // QT_NO_WHATSTHIS
        labelCustomizations->setText(QApplication::translate(
            "editorwindow",
            "<html><head/><body><p><span style=\" font-size:12pt; font-weight:600; "
            "color:#ffffff;\">Customizations</span></p></body></html>",
            nullptr)
        );
    } // retranslateUi

};

namespace Ui {
    class editorwindow: public Ui_editorwindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // __OPENSPACE_LAUNCHER_UI__EDITORWINDOW___H__
