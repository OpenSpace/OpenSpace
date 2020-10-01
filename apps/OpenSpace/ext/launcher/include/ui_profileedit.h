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

#ifndef UI_PROFILEEDIT_H
#define UI_PROFILEEDIT_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_ProfileEdit
{
public:
    QGridLayout *gridLayout;
    QVBoxLayout *vLay_main;
    QHBoxLayout *hLay_top_profileName;
    QLabel *label_profile;
    QLineEdit *line_profile;
    QPushButton *duplicate_profile;
    QFrame *line_6;
    QHBoxLayout *hLay_middle_info;
    QVBoxLayout *vLay_left;
    QVBoxLayout *vLay_properties;
    QHBoxLayout *horizontalLayout_2;
    QLabel *label_properties;
    QPushButton *edit_properties;
    QSpacerItem *horizontalSpacer_3;
    QTextEdit *text_properties;
    QFrame *line_3;
    QVBoxLayout *vLay_assets;
    QHBoxLayout *hLay_assets_edit;
    QLabel *label_assets;
    QPushButton *edit_assets;
    QSpacerItem *horizontalSpacer;
    QTextEdit *text_assets;
    QFrame *line_4;
    QVBoxLayout *vLay_keybindings;
    QHBoxLayout *hLay_keybindings_edit;
    QLabel *label_keybindings;
    QPushButton *edit_keybindings;
    QSpacerItem *horizontalSpacer_2;
    QTextEdit *text_keybindings;
    QFrame *line;
    QVBoxLayout *vLay_right;
    QVBoxLayout *vLay_meta;
    QLabel *label_meta;
    QPushButton *edit_meta;
    QFrame *line_2;
    QVBoxLayout *vLay_marknodes;
    QLabel *label_marknodes;
    QPushButton *edit_marknodes;
    QFrame *line_7;
    QVBoxLayout *vLay_deltatimes;
    QLabel *label_deltatimes;
    QPushButton *edit_deltatimes;
    QFrame *line_8;
    QVBoxLayout *vLay_camera;
    QLabel *label_camera;
    QPushButton *edit_camera;
    QFrame *line_9;
    QVBoxLayout *vLay_time;
    QLabel *label_time;
    QPushButton *edit_time;
    QFrame *line_10;
    QVBoxLayout *vLay_modules;
    QLabel *label_modules;
    QPushButton *edit_modules;
    QFrame *line_11;
    QVBoxLayout *vLay_additionalscripts;
    QLabel *label_additionalscripts;
    QPushButton *edit_additionalscripts;
    QFrame *line_5;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *ProfileEdit)
    {
        if (ProfileEdit->objectName().isEmpty())
            ProfileEdit->setObjectName(QString::fromUtf8("ProfileEdit"));
        ProfileEdit->resize(707, 674);
        gridLayout = new QGridLayout(ProfileEdit);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        vLay_main = new QVBoxLayout();
        vLay_main->setObjectName(QString::fromUtf8("vLay_main"));
        hLay_top_profileName = new QHBoxLayout();
        hLay_top_profileName->setObjectName(QString::fromUtf8("hLay_top_profileName"));
        label_profile = new QLabel(ProfileEdit);
        label_profile->setObjectName(QString::fromUtf8("label_profile"));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(13);
        label_profile->setFont(font);

        hLay_top_profileName->addWidget(label_profile);

        line_profile = new QLineEdit(ProfileEdit);
        line_profile->setObjectName(QString::fromUtf8("line_profile"));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(12);
        line_profile->setFont(font1);

        hLay_top_profileName->addWidget(line_profile);

        duplicate_profile = new QPushButton(ProfileEdit);
        duplicate_profile->setObjectName(QString::fromUtf8("duplicate_profile"));
        QFont font2;
        font2.setFamily(QString::fromUtf8("Arial"));
        duplicate_profile->setFont(font2);
        duplicate_profile->setMinimumSize(QSize(110, 0));
        duplicate_profile->setMaximumSize(QSize(130, 16777215));

        hLay_top_profileName->addWidget(duplicate_profile);


        vLay_main->addLayout(hLay_top_profileName);

        line_6 = new QFrame(ProfileEdit);
        line_6->setObjectName(QString::fromUtf8("line_6"));
        line_6->setFrameShape(QFrame::HLine);
        line_6->setFrameShadow(QFrame::Sunken);

        vLay_main->addWidget(line_6);

        hLay_middle_info = new QHBoxLayout();
        hLay_middle_info->setObjectName(QString::fromUtf8("hLay_middle_info"));
        vLay_left = new QVBoxLayout();
        vLay_left->setObjectName(QString::fromUtf8("vLay_left"));
        vLay_properties = new QVBoxLayout();
        vLay_properties->setObjectName(QString::fromUtf8("vLay_properties"));
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        label_properties = new QLabel(ProfileEdit);
        label_properties->setObjectName(QString::fromUtf8("label_properties"));
        label_properties->setMinimumSize(QSize(120, 0));
        label_properties->setMaximumSize(QSize(200, 16777215));
        label_properties->setFont(font1);
        label_properties->setTextFormat(Qt::AutoText);

        horizontalLayout_2->addWidget(label_properties);

        edit_properties = new QPushButton(ProfileEdit);
        edit_properties->setObjectName(QString::fromUtf8("edit_properties"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(edit_properties->sizePolicy().hasHeightForWidth());
        edit_properties->setSizePolicy(sizePolicy);
        edit_properties->setMinimumSize(QSize(60, 25));
        edit_properties->setMaximumSize(QSize(80, 40));

        horizontalLayout_2->addWidget(edit_properties);

        horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_3);


        vLay_properties->addLayout(horizontalLayout_2);

        text_properties = new QTextEdit(ProfileEdit);
        text_properties->setObjectName(QString::fromUtf8("text_properties"));
        text_properties->setMinimumSize(QSize(200, 0));

        vLay_properties->addWidget(text_properties);


        vLay_left->addLayout(vLay_properties);

        line_3 = new QFrame(ProfileEdit);
        line_3->setObjectName(QString::fromUtf8("line_3"));
        line_3->setFrameShape(QFrame::HLine);
        line_3->setFrameShadow(QFrame::Sunken);

        vLay_left->addWidget(line_3);

        vLay_assets = new QVBoxLayout();
        vLay_assets->setObjectName(QString::fromUtf8("vLay_assets"));
        hLay_assets_edit = new QHBoxLayout();
        hLay_assets_edit->setObjectName(QString::fromUtf8("hLay_assets_edit"));
        label_assets = new QLabel(ProfileEdit);
        label_assets->setObjectName(QString::fromUtf8("label_assets"));
        label_assets->setMinimumSize(QSize(120, 0));
        label_assets->setMaximumSize(QSize(200, 16777215));
        label_assets->setFont(font1);

        hLay_assets_edit->addWidget(label_assets);

        edit_assets = new QPushButton(ProfileEdit);
        edit_assets->setObjectName(QString::fromUtf8("edit_assets"));
        sizePolicy.setHeightForWidth(edit_assets->sizePolicy().hasHeightForWidth());
        edit_assets->setSizePolicy(sizePolicy);
        edit_assets->setMinimumSize(QSize(60, 25));
        edit_assets->setMaximumSize(QSize(80, 40));

        hLay_assets_edit->addWidget(edit_assets);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hLay_assets_edit->addItem(horizontalSpacer);


        vLay_assets->addLayout(hLay_assets_edit);

        text_assets = new QTextEdit(ProfileEdit);
        text_assets->setObjectName(QString::fromUtf8("text_assets"));
        text_assets->setMinimumSize(QSize(200, 0));

        vLay_assets->addWidget(text_assets);


        vLay_left->addLayout(vLay_assets);

        line_4 = new QFrame(ProfileEdit);
        line_4->setObjectName(QString::fromUtf8("line_4"));
        line_4->setFrameShape(QFrame::HLine);
        line_4->setFrameShadow(QFrame::Sunken);

        vLay_left->addWidget(line_4);

        vLay_keybindings = new QVBoxLayout();
        vLay_keybindings->setObjectName(QString::fromUtf8("vLay_keybindings"));
        hLay_keybindings_edit = new QHBoxLayout();
        hLay_keybindings_edit->setObjectName(QString::fromUtf8("hLay_keybindings_edit"));
        label_keybindings = new QLabel(ProfileEdit);
        label_keybindings->setObjectName(QString::fromUtf8("label_keybindings"));
        label_keybindings->setMinimumSize(QSize(120, 0));
        label_keybindings->setMaximumSize(QSize(200, 16777215));
        label_keybindings->setFont(font1);

        hLay_keybindings_edit->addWidget(label_keybindings);

        edit_keybindings = new QPushButton(ProfileEdit);
        edit_keybindings->setObjectName(QString::fromUtf8("edit_keybindings"));
        sizePolicy.setHeightForWidth(edit_keybindings->sizePolicy().hasHeightForWidth());
        edit_keybindings->setSizePolicy(sizePolicy);
        edit_keybindings->setMinimumSize(QSize(60, 0));
        edit_keybindings->setMaximumSize(QSize(80, 16777215));

        hLay_keybindings_edit->addWidget(edit_keybindings);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hLay_keybindings_edit->addItem(horizontalSpacer_2);


        vLay_keybindings->addLayout(hLay_keybindings_edit);

        text_keybindings = new QTextEdit(ProfileEdit);
        text_keybindings->setObjectName(QString::fromUtf8("text_keybindings"));
        text_keybindings->setMinimumSize(QSize(200, 0));

        vLay_keybindings->addWidget(text_keybindings);


        vLay_left->addLayout(vLay_keybindings);


        hLay_middle_info->addLayout(vLay_left);

        line = new QFrame(ProfileEdit);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::VLine);
        line->setFrameShadow(QFrame::Sunken);

        hLay_middle_info->addWidget(line);

        vLay_right = new QVBoxLayout();
        vLay_right->setObjectName(QString::fromUtf8("vLay_right"));
        vLay_meta = new QVBoxLayout();
        vLay_meta->setObjectName(QString::fromUtf8("vLay_meta"));
        label_meta = new QLabel(ProfileEdit);
        label_meta->setObjectName(QString::fromUtf8("label_meta"));
        label_meta->setMinimumSize(QSize(160, 25));
        label_meta->setFont(font1);
        label_meta->setWordWrap(true);

        vLay_meta->addWidget(label_meta);

        edit_meta = new QPushButton(ProfileEdit);
        edit_meta->setObjectName(QString::fromUtf8("edit_meta"));
        sizePolicy.setHeightForWidth(edit_meta->sizePolicy().hasHeightForWidth());
        edit_meta->setSizePolicy(sizePolicy);
        edit_meta->setMinimumSize(QSize(50, 0));
        edit_meta->setMaximumSize(QSize(70, 16777215));
        QFont font3;
        font3.setFamily(QString::fromUtf8("Arial"));
        font3.setPointSize(11);
        edit_meta->setFont(font3);
        edit_meta->setLayoutDirection(Qt::RightToLeft);

        vLay_meta->addWidget(edit_meta);


        vLay_right->addLayout(vLay_meta);

        line_2 = new QFrame(ProfileEdit);
        line_2->setObjectName(QString::fromUtf8("line_2"));
        line_2->setFrameShape(QFrame::HLine);
        line_2->setFrameShadow(QFrame::Sunken);

        vLay_right->addWidget(line_2);

        vLay_marknodes = new QVBoxLayout();
        vLay_marknodes->setObjectName(QString::fromUtf8("vLay_marknodes"));
        label_marknodes = new QLabel(ProfileEdit);
        label_marknodes->setObjectName(QString::fromUtf8("label_marknodes"));
        label_marknodes->setMinimumSize(QSize(160, 40));
        label_marknodes->setFont(font1);
        label_marknodes->setWordWrap(true);

        vLay_marknodes->addWidget(label_marknodes);

        edit_marknodes = new QPushButton(ProfileEdit);
        edit_marknodes->setObjectName(QString::fromUtf8("edit_marknodes"));
        sizePolicy.setHeightForWidth(edit_marknodes->sizePolicy().hasHeightForWidth());
        edit_marknodes->setSizePolicy(sizePolicy);
        edit_marknodes->setMinimumSize(QSize(50, 0));
        edit_marknodes->setMaximumSize(QSize(70, 16777215));
        edit_marknodes->setFont(font2);
        edit_marknodes->setLayoutDirection(Qt::RightToLeft);

        vLay_marknodes->addWidget(edit_marknodes);


        vLay_right->addLayout(vLay_marknodes);

        line_7 = new QFrame(ProfileEdit);
        line_7->setObjectName(QString::fromUtf8("line_7"));
        line_7->setFrameShape(QFrame::HLine);
        line_7->setFrameShadow(QFrame::Sunken);

        vLay_right->addWidget(line_7);

        vLay_deltatimes = new QVBoxLayout();
        vLay_deltatimes->setObjectName(QString::fromUtf8("vLay_deltatimes"));
        label_deltatimes = new QLabel(ProfileEdit);
        label_deltatimes->setObjectName(QString::fromUtf8("label_deltatimes"));
        label_deltatimes->setMinimumSize(QSize(160, 40));
        label_deltatimes->setFont(font1);
        label_deltatimes->setAlignment(Qt::AlignBottom|Qt::AlignLeading|Qt::AlignLeft);
        label_deltatimes->setWordWrap(true);

        vLay_deltatimes->addWidget(label_deltatimes);

        edit_deltatimes = new QPushButton(ProfileEdit);
        edit_deltatimes->setObjectName(QString::fromUtf8("edit_deltatimes"));
        sizePolicy.setHeightForWidth(edit_deltatimes->sizePolicy().hasHeightForWidth());
        edit_deltatimes->setSizePolicy(sizePolicy);
        edit_deltatimes->setMinimumSize(QSize(50, 0));
        edit_deltatimes->setMaximumSize(QSize(70, 16777215));
        edit_deltatimes->setFont(font2);
        edit_deltatimes->setLayoutDirection(Qt::RightToLeft);

        vLay_deltatimes->addWidget(edit_deltatimes);


        vLay_right->addLayout(vLay_deltatimes);

        line_8 = new QFrame(ProfileEdit);
        line_8->setObjectName(QString::fromUtf8("line_8"));
        line_8->setFrameShape(QFrame::HLine);
        line_8->setFrameShadow(QFrame::Sunken);

        vLay_right->addWidget(line_8);

        vLay_camera = new QVBoxLayout();
        vLay_camera->setObjectName(QString::fromUtf8("vLay_camera"));
        label_camera = new QLabel(ProfileEdit);
        label_camera->setObjectName(QString::fromUtf8("label_camera"));
        label_camera->setMinimumSize(QSize(160, 25));
        label_camera->setFont(font1);
        label_camera->setWordWrap(true);

        vLay_camera->addWidget(label_camera);

        edit_camera = new QPushButton(ProfileEdit);
        edit_camera->setObjectName(QString::fromUtf8("edit_camera"));
        sizePolicy.setHeightForWidth(edit_camera->sizePolicy().hasHeightForWidth());
        edit_camera->setSizePolicy(sizePolicy);
        edit_camera->setMinimumSize(QSize(50, 0));
        edit_camera->setMaximumSize(QSize(70, 16777215));
        edit_camera->setFont(font2);
        edit_camera->setLayoutDirection(Qt::RightToLeft);

        vLay_camera->addWidget(edit_camera);


        vLay_right->addLayout(vLay_camera);

        line_9 = new QFrame(ProfileEdit);
        line_9->setObjectName(QString::fromUtf8("line_9"));
        line_9->setFrameShape(QFrame::HLine);
        line_9->setFrameShadow(QFrame::Sunken);

        vLay_right->addWidget(line_9);

        vLay_time = new QVBoxLayout();
        vLay_time->setObjectName(QString::fromUtf8("vLay_time"));
        label_time = new QLabel(ProfileEdit);
        label_time->setObjectName(QString::fromUtf8("label_time"));
        label_time->setMinimumSize(QSize(160, 25));
        label_time->setFont(font1);
        label_time->setWordWrap(true);

        vLay_time->addWidget(label_time);

        edit_time = new QPushButton(ProfileEdit);
        edit_time->setObjectName(QString::fromUtf8("edit_time"));
        sizePolicy.setHeightForWidth(edit_time->sizePolicy().hasHeightForWidth());
        edit_time->setSizePolicy(sizePolicy);
        edit_time->setMinimumSize(QSize(50, 0));
        edit_time->setMaximumSize(QSize(70, 16777215));
        edit_time->setFont(font2);
        edit_time->setLayoutDirection(Qt::RightToLeft);

        vLay_time->addWidget(edit_time);


        vLay_right->addLayout(vLay_time);

        line_10 = new QFrame(ProfileEdit);
        line_10->setObjectName(QString::fromUtf8("line_10"));
        line_10->setFrameShape(QFrame::HLine);
        line_10->setFrameShadow(QFrame::Sunken);

        vLay_right->addWidget(line_10);

        vLay_modules = new QVBoxLayout();
        vLay_modules->setObjectName(QString::fromUtf8("vLay_modules"));
        label_modules = new QLabel(ProfileEdit);
        label_modules->setObjectName(QString::fromUtf8("label_modules"));
        label_modules->setMinimumSize(QSize(160, 25));
        label_modules->setFont(font1);
        label_modules->setWordWrap(true);

        vLay_modules->addWidget(label_modules);

        edit_modules = new QPushButton(ProfileEdit);
        edit_modules->setObjectName(QString::fromUtf8("edit_modules"));
        sizePolicy.setHeightForWidth(edit_modules->sizePolicy().hasHeightForWidth());
        edit_modules->setSizePolicy(sizePolicy);
        edit_modules->setMinimumSize(QSize(50, 0));
        edit_modules->setMaximumSize(QSize(70, 16777215));
        edit_modules->setFont(font2);
        edit_modules->setLayoutDirection(Qt::RightToLeft);

        vLay_modules->addWidget(edit_modules);


        vLay_right->addLayout(vLay_modules);

        line_11 = new QFrame(ProfileEdit);
        line_11->setObjectName(QString::fromUtf8("line_11"));
        line_11->setFrameShape(QFrame::HLine);
        line_11->setFrameShadow(QFrame::Sunken);

        vLay_right->addWidget(line_11);

        vLay_additionalscripts = new QVBoxLayout();
        vLay_additionalscripts->setObjectName(QString::fromUtf8("vLay_additionalscripts"));
        label_additionalscripts = new QLabel(ProfileEdit);
        label_additionalscripts->setObjectName(QString::fromUtf8("label_additionalscripts"));
        label_additionalscripts->setMinimumSize(QSize(160, 25));
        label_additionalscripts->setFont(font1);
        label_additionalscripts->setWordWrap(true);

        vLay_additionalscripts->addWidget(label_additionalscripts);

        edit_additionalscripts = new QPushButton(ProfileEdit);
        edit_additionalscripts->setObjectName(QString::fromUtf8("edit_additionalscripts"));
        sizePolicy.setHeightForWidth(edit_additionalscripts->sizePolicy().hasHeightForWidth());
        edit_additionalscripts->setSizePolicy(sizePolicy);
        edit_additionalscripts->setMinimumSize(QSize(50, 0));
        edit_additionalscripts->setMaximumSize(QSize(70, 16777215));
        edit_additionalscripts->setFont(font2);
        edit_additionalscripts->setLayoutDirection(Qt::RightToLeft);

        vLay_additionalscripts->addWidget(edit_additionalscripts);


        vLay_right->addLayout(vLay_additionalscripts);


        hLay_middle_info->addLayout(vLay_right);


        vLay_main->addLayout(hLay_middle_info);

        line_5 = new QFrame(ProfileEdit);
        line_5->setObjectName(QString::fromUtf8("line_5"));
        line_5->setFrameShape(QFrame::HLine);
        line_5->setFrameShadow(QFrame::Sunken);

        vLay_main->addWidget(line_5);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));
        label_error = new QLabel(ProfileEdit);
        label_error->setObjectName(QString::fromUtf8("label_error"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(label_error->sizePolicy().hasHeightForWidth());
        label_error->setSizePolicy(sizePolicy1);
        label_error->setMinimumSize(QSize(200, 40));
        label_error->setMaximumSize(QSize(800, 40));
        label_error->setFont(font2);
        label_error->setWordWrap(true);

        hLay_bottom_buttonBox->addWidget(label_error);

        buttonBox = new QDialogButtonBox(ProfileEdit);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setFont(font2);
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);


        vLay_main->addLayout(hLay_bottom_buttonBox);


        gridLayout->addLayout(vLay_main, 2, 0, 1, 1);


        retranslateUi(ProfileEdit);
        //QObject::connect(buttonBox, SIGNAL(accepted()), ProfileEdit, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), ProfileEdit, SLOT(reject()));

        QMetaObject::connectSlotsByName(ProfileEdit);
    } // setupUi

    void retranslateUi(QDialog *ProfileEdit)
    {
        ProfileEdit->setWindowTitle(QCoreApplication::translate("ProfileEdit", "Profile Editor", nullptr));
        label_profile->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Profile Name:</span></p></body></html>", nullptr));
#if QT_CONFIG(tooltip)
        line_profile->setToolTip(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p>Name of profile filename</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        duplicate_profile->setText(QCoreApplication::translate("ProfileEdit", "Duplicate Profile", nullptr));
        label_properties->setText(QCoreApplication::translate("ProfileEdit", "Properties", nullptr));
        edit_properties->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_assets->setText(QCoreApplication::translate("ProfileEdit", "Assets", nullptr));
        edit_assets->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_keybindings->setText(QCoreApplication::translate("ProfileEdit", "Keybindings", nullptr));
        edit_keybindings->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_meta->setText(QCoreApplication::translate("ProfileEdit", "Meta", nullptr));
        edit_meta->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_marknodes->setText(QCoreApplication::translate("ProfileEdit", "Mark Interesting Nodes", nullptr));
        edit_marknodes->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_deltatimes->setText(QCoreApplication::translate("ProfileEdit", "Simulation Time Increments", nullptr));
        edit_deltatimes->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_camera->setText(QCoreApplication::translate("ProfileEdit", "Camera", nullptr));
        edit_camera->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_time->setText(QCoreApplication::translate("ProfileEdit", "Time", nullptr));
        edit_time->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_modules->setText(QCoreApplication::translate("ProfileEdit", "Modules", nullptr));
        edit_modules->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_additionalscripts->setText(QCoreApplication::translate("ProfileEdit", "Additional Scripts", nullptr));
        edit_additionalscripts->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_error->setText(QCoreApplication::translate("ProfileEdit", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class ProfileEdit: public Ui_ProfileEdit {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PROFILEEDIT_H
