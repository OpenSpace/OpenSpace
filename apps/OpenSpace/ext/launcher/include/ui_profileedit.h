/********************************************************************************
** Form generated from reading UI file 'profileedit.ui'
**
** Created by: Qt User Interface Compiler version 5.15.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PROFILEEDIT_H
#define UI_PROFILEEDIT_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_ProfileEdit
{
public:
    QDialogButtonBox *buttonBox;
    QWidget *verticalLayoutWidget;
    QVBoxLayout *verticalLayout;
    QFrame *frame_meta;
    QLabel *label_meta;
    QPushButton *edit_meta;
    QTextEdit *text_meta;
    QFrame *frame_modules;
    QLabel *label_modules;
    QPushButton *edit_modules;
    QTextEdit *text_modules;
    QFrame *frame_assets;
    QLabel *label_assets;
    QPushButton *edit_assets;
    QTextEdit *text_assets;
    QFrame *frame_properties;
    QLabel *label_properties;
    QPushButton *edit_properties;
    QTextEdit *text_properties;
    QFrame *frame_keybindings;
    QLabel *label_keybindings;
    QPushButton *edit_keybindings;
    QTextEdit *text_keybindings;
    QFrame *frame_time;
    QLabel *label_time;
    QPushButton *edit_time;
    QTextEdit *text_time;
    QFrame *frame_deltatimes;
    QLabel *label_deltatimes;
    QPushButton *edit_deltatimes;
    QTextEdit *text_deltatimes;
    QFrame *frame_camera;
    QLabel *label_camera;
    QPushButton *edit_camera;
    QTextEdit *text_camera;
    QFrame *frame_marknodes;
    QLabel *label_marknodes;
    QPushButton *edit_marknodes;
    QTextEdit *text_marknodes;
    QFrame *frame_additionalscripts;
    QLabel *label_additionalscripts;
    QPushButton *edit_additionalscripts;
    QTextEdit *text_additionalscripts;
    QLabel *label_profile;
    QLineEdit *line_profile;

    void setupUi(QDialog *ProfileEdit)
    {
        if (ProfileEdit->objectName().isEmpty())
            ProfileEdit->setObjectName(QString::fromUtf8("ProfileEdit"));
        ProfileEdit->resize(400, 790);
        buttonBox = new QDialogButtonBox(ProfileEdit);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setGeometry(QRect(50, 750, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Save);
        verticalLayoutWidget = new QWidget(ProfileEdit);
        verticalLayoutWidget->setObjectName(QString::fromUtf8("verticalLayoutWidget"));
        verticalLayoutWidget->setGeometry(QRect(10, 60, 381, 671));
        verticalLayout = new QVBoxLayout(verticalLayoutWidget);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 0);
        frame_meta = new QFrame(verticalLayoutWidget);
        frame_meta->setObjectName(QString::fromUtf8("frame_meta"));
        frame_meta->setEnabled(true);
        frame_meta->setMaximumSize(QSize(444, 56));
        frame_meta->setFrameShape(QFrame::StyledPanel);
        frame_meta->setFrameShadow(QFrame::Plain);
        label_meta = new QLabel(frame_meta);
        label_meta->setObjectName(QString::fromUtf8("label_meta"));
        label_meta->setGeometry(QRect(4, 0, 111, 17));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        label_meta->setFont(font);
        edit_meta = new QPushButton(frame_meta);
        edit_meta->setObjectName(QString::fromUtf8("edit_meta"));
        edit_meta->setGeometry(QRect(328, 20, 41, 25));
        text_meta = new QTextEdit(frame_meta);
        text_meta->setObjectName(QString::fromUtf8("text_meta"));
        text_meta->setGeometry(QRect(10, 18, 311, 35));
        QFont font1;
        font1.setFamily(QString::fromUtf8("Arial"));
        font1.setPointSize(10);
        text_meta->setFont(font1);
        text_meta->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_meta->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_meta);

        frame_modules = new QFrame(verticalLayoutWidget);
        frame_modules->setObjectName(QString::fromUtf8("frame_modules"));
        frame_modules->setEnabled(true);
        frame_modules->setMaximumSize(QSize(444, 56));
        frame_modules->setFrameShape(QFrame::StyledPanel);
        frame_modules->setFrameShadow(QFrame::Plain);
        label_modules = new QLabel(frame_modules);
        label_modules->setObjectName(QString::fromUtf8("label_modules"));
        label_modules->setGeometry(QRect(4, 0, 111, 17));
        label_modules->setFont(font);
        edit_modules = new QPushButton(frame_modules);
        edit_modules->setObjectName(QString::fromUtf8("edit_modules"));
        edit_modules->setGeometry(QRect(328, 20, 41, 25));
        text_modules = new QTextEdit(frame_modules);
        text_modules->setObjectName(QString::fromUtf8("text_modules"));
        text_modules->setGeometry(QRect(10, 18, 311, 35));
        text_modules->setFont(font1);
        text_modules->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_modules->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_modules);

        frame_assets = new QFrame(verticalLayoutWidget);
        frame_assets->setObjectName(QString::fromUtf8("frame_assets"));
        frame_assets->setEnabled(true);
        frame_assets->setMaximumSize(QSize(444, 56));
        frame_assets->setFrameShape(QFrame::StyledPanel);
        frame_assets->setFrameShadow(QFrame::Plain);
        label_assets = new QLabel(frame_assets);
        label_assets->setObjectName(QString::fromUtf8("label_assets"));
        label_assets->setGeometry(QRect(4, 0, 111, 17));
        label_assets->setFont(font);
        edit_assets = new QPushButton(frame_assets);
        edit_assets->setObjectName(QString::fromUtf8("edit_assets"));
        edit_assets->setGeometry(QRect(328, 20, 41, 25));
        text_assets = new QTextEdit(frame_assets);
        text_assets->setObjectName(QString::fromUtf8("text_assets"));
        text_assets->setGeometry(QRect(10, 18, 311, 35));
        text_assets->setFont(font1);
        text_assets->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_assets->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_assets);

        frame_properties = new QFrame(verticalLayoutWidget);
        frame_properties->setObjectName(QString::fromUtf8("frame_properties"));
        frame_properties->setEnabled(true);
        frame_properties->setMaximumSize(QSize(444, 56));
        frame_properties->setFrameShape(QFrame::StyledPanel);
        frame_properties->setFrameShadow(QFrame::Plain);
        label_properties = new QLabel(frame_properties);
        label_properties->setObjectName(QString::fromUtf8("label_properties"));
        label_properties->setGeometry(QRect(4, 0, 111, 17));
        label_properties->setFont(font);
        edit_properties = new QPushButton(frame_properties);
        edit_properties->setObjectName(QString::fromUtf8("edit_properties"));
        edit_properties->setGeometry(QRect(328, 20, 41, 25));
        text_properties = new QTextEdit(frame_properties);
        text_properties->setObjectName(QString::fromUtf8("text_properties"));
        text_properties->setGeometry(QRect(10, 18, 311, 35));
        text_properties->setFont(font1);
        text_properties->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_properties->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_properties);

        frame_keybindings = new QFrame(verticalLayoutWidget);
        frame_keybindings->setObjectName(QString::fromUtf8("frame_keybindings"));
        frame_keybindings->setEnabled(true);
        frame_keybindings->setMaximumSize(QSize(444, 56));
        frame_keybindings->setFrameShape(QFrame::StyledPanel);
        frame_keybindings->setFrameShadow(QFrame::Plain);
        label_keybindings = new QLabel(frame_keybindings);
        label_keybindings->setObjectName(QString::fromUtf8("label_keybindings"));
        label_keybindings->setGeometry(QRect(4, 0, 111, 17));
        label_keybindings->setFont(font);
        edit_keybindings = new QPushButton(frame_keybindings);
        edit_keybindings->setObjectName(QString::fromUtf8("edit_keybindings"));
        edit_keybindings->setGeometry(QRect(328, 20, 41, 25));
        text_keybindings = new QTextEdit(frame_keybindings);
        text_keybindings->setObjectName(QString::fromUtf8("text_keybindings"));
        text_keybindings->setGeometry(QRect(10, 18, 311, 35));
        text_keybindings->setFont(font1);
        text_keybindings->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_keybindings->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_keybindings);

        frame_time = new QFrame(verticalLayoutWidget);
        frame_time->setObjectName(QString::fromUtf8("frame_time"));
        frame_time->setEnabled(true);
        frame_time->setMaximumSize(QSize(444, 56));
        frame_time->setFrameShape(QFrame::StyledPanel);
        frame_time->setFrameShadow(QFrame::Plain);
        label_time = new QLabel(frame_time);
        label_time->setObjectName(QString::fromUtf8("label_time"));
        label_time->setGeometry(QRect(4, 0, 111, 17));
        label_time->setFont(font);
        edit_time = new QPushButton(frame_time);
        edit_time->setObjectName(QString::fromUtf8("edit_time"));
        edit_time->setGeometry(QRect(328, 20, 41, 25));
        text_time = new QTextEdit(frame_time);
        text_time->setObjectName(QString::fromUtf8("text_time"));
        text_time->setGeometry(QRect(10, 18, 311, 35));
        text_time->setFont(font1);
        text_time->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_time->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_time);

        frame_deltatimes = new QFrame(verticalLayoutWidget);
        frame_deltatimes->setObjectName(QString::fromUtf8("frame_deltatimes"));
        frame_deltatimes->setEnabled(true);
        frame_deltatimes->setMaximumSize(QSize(444, 56));
        frame_deltatimes->setFrameShape(QFrame::StyledPanel);
        frame_deltatimes->setFrameShadow(QFrame::Plain);
        label_deltatimes = new QLabel(frame_deltatimes);
        label_deltatimes->setObjectName(QString::fromUtf8("label_deltatimes"));
        label_deltatimes->setGeometry(QRect(4, 0, 111, 17));
        label_deltatimes->setFont(font);
        edit_deltatimes = new QPushButton(frame_deltatimes);
        edit_deltatimes->setObjectName(QString::fromUtf8("edit_deltatimes"));
        edit_deltatimes->setGeometry(QRect(328, 20, 41, 25));
        text_deltatimes = new QTextEdit(frame_deltatimes);
        text_deltatimes->setObjectName(QString::fromUtf8("text_deltatimes"));
        text_deltatimes->setGeometry(QRect(10, 18, 311, 35));
        text_deltatimes->setFont(font1);
        text_deltatimes->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_deltatimes->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_deltatimes);

        frame_camera = new QFrame(verticalLayoutWidget);
        frame_camera->setObjectName(QString::fromUtf8("frame_camera"));
        frame_camera->setEnabled(true);
        frame_camera->setMaximumSize(QSize(444, 56));
        frame_camera->setFrameShape(QFrame::StyledPanel);
        frame_camera->setFrameShadow(QFrame::Plain);
        label_camera = new QLabel(frame_camera);
        label_camera->setObjectName(QString::fromUtf8("label_camera"));
        label_camera->setGeometry(QRect(4, 0, 111, 17));
        label_camera->setFont(font);
        edit_camera = new QPushButton(frame_camera);
        edit_camera->setObjectName(QString::fromUtf8("edit_camera"));
        edit_camera->setGeometry(QRect(328, 20, 41, 25));
        text_camera = new QTextEdit(frame_camera);
        text_camera->setObjectName(QString::fromUtf8("text_camera"));
        text_camera->setGeometry(QRect(10, 18, 311, 35));
        text_camera->setFont(font1);
        text_camera->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_camera->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_camera);

        frame_marknodes = new QFrame(verticalLayoutWidget);
        frame_marknodes->setObjectName(QString::fromUtf8("frame_marknodes"));
        frame_marknodes->setEnabled(true);
        frame_marknodes->setMaximumSize(QSize(444, 56));
        frame_marknodes->setFrameShape(QFrame::StyledPanel);
        frame_marknodes->setFrameShadow(QFrame::Plain);
        label_marknodes = new QLabel(frame_marknodes);
        label_marknodes->setObjectName(QString::fromUtf8("label_marknodes"));
        label_marknodes->setGeometry(QRect(4, 0, 181, 17));
        label_marknodes->setFont(font);
        edit_marknodes = new QPushButton(frame_marknodes);
        edit_marknodes->setObjectName(QString::fromUtf8("edit_marknodes"));
        edit_marknodes->setGeometry(QRect(328, 20, 41, 25));
        text_marknodes = new QTextEdit(frame_marknodes);
        text_marknodes->setObjectName(QString::fromUtf8("text_marknodes"));
        text_marknodes->setGeometry(QRect(10, 18, 311, 35));
        text_marknodes->setFont(font1);
        text_marknodes->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_marknodes->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_marknodes);

        frame_additionalscripts = new QFrame(verticalLayoutWidget);
        frame_additionalscripts->setObjectName(QString::fromUtf8("frame_additionalscripts"));
        frame_additionalscripts->setEnabled(true);
        frame_additionalscripts->setMaximumSize(QSize(444, 56));
        frame_additionalscripts->setFrameShape(QFrame::StyledPanel);
        frame_additionalscripts->setFrameShadow(QFrame::Plain);
        label_additionalscripts = new QLabel(frame_additionalscripts);
        label_additionalscripts->setObjectName(QString::fromUtf8("label_additionalscripts"));
        label_additionalscripts->setGeometry(QRect(4, 0, 171, 17));
        label_additionalscripts->setFont(font);
        edit_additionalscripts = new QPushButton(frame_additionalscripts);
        edit_additionalscripts->setObjectName(QString::fromUtf8("edit_additionalscripts"));
        edit_additionalscripts->setGeometry(QRect(328, 20, 41, 25));
        text_additionalscripts = new QTextEdit(frame_additionalscripts);
        text_additionalscripts->setObjectName(QString::fromUtf8("text_additionalscripts"));
        text_additionalscripts->setGeometry(QRect(10, 18, 311, 35));
        text_additionalscripts->setFont(font1);
        text_additionalscripts->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        text_additionalscripts->setLineWrapMode(QTextEdit::NoWrap);

        verticalLayout->addWidget(frame_additionalscripts);

        label_profile = new QLabel(ProfileEdit);
        label_profile->setObjectName(QString::fromUtf8("label_profile"));
        label_profile->setGeometry(QRect(10, 22, 111, 21));
        QFont font2;
        font2.setFamily(QString::fromUtf8("Arial"));
        font2.setPointSize(13);
        label_profile->setFont(font2);
        line_profile = new QLineEdit(ProfileEdit);
        line_profile->setObjectName(QString::fromUtf8("line_profile"));
        line_profile->setGeometry(QRect(130, 20, 201, 25));
        QFont font3;
        font3.setFamily(QString::fromUtf8("Arial"));
        font3.setPointSize(12);
        line_profile->setFont(font3);

        retranslateUi(ProfileEdit);
        //QObject::connect(buttonBox, SIGNAL(accepted()), ProfileEdit, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), ProfileEdit, SLOT(reject()));

        QMetaObject::connectSlotsByName(ProfileEdit);
    } // setupUi

    void retranslateUi(QDialog *ProfileEdit)
    {
        ProfileEdit->setWindowTitle(QCoreApplication::translate("ProfileEdit", "Profile Editor", nullptr));
        label_meta->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Meta</span></p></body></html>", nullptr));
        edit_meta->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_modules->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Modules</span></p></body></html>", nullptr));
        edit_modules->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_assets->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Assets</span></p></body></html>", nullptr));
        edit_assets->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_properties->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Properties</span></p></body></html>", nullptr));
        edit_properties->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_keybindings->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Keybindings</span></p></body></html>", nullptr));
        edit_keybindings->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_time->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Time</span></p></body></html>", nullptr));
        edit_time->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_deltatimes->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Delta Times</span></p></body></html>", nullptr));
        edit_deltatimes->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_camera->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Camera</span></p></body></html>", nullptr));
        edit_camera->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_marknodes->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Mark Interesting Nodes</span></p></body></html>", nullptr));
        edit_marknodes->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_additionalscripts->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Additional Scripts</span></p></body></html>", nullptr));
        edit_additionalscripts->setText(QCoreApplication::translate("ProfileEdit", "Edit", nullptr));
        label_profile->setText(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p><span style=\" font-weight:600;\">Profile Name:</span></p></body></html>", nullptr));
#if QT_CONFIG(tooltip)
        line_profile->setToolTip(QCoreApplication::translate("ProfileEdit", "<html><head/><body><p>Name of profile filename</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
    } // retranslateUi

};

namespace Ui {
    class ProfileEdit: public Ui_ProfileEdit {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PROFILEEDIT_H
