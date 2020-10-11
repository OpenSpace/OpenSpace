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

#ifndef __OPENSPACE_UI_LAUNCHER___UI_CAMERA___H__
#define __OPENSPACE_UI_LAUNCHER___UI_CAMERA___H__

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFrame>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_camera
{
public:
    QVBoxLayout *verticalLayout;
    QFrame *line;
    QTabWidget *tabWidget;
    QWidget *tab;
    QLabel *label_anchorNav;
    QLineEdit *line_anchorNav;
    QLabel *label_aim;
    QLineEdit *line_aim;
    QLabel *label_referenceFrame;
    QLineEdit *line_referenceFrame;
    QLabel *label_pos;
    QLabel *label_posX;
    QLineEdit *line_posX;
    QLabel *label_posY;
    QLineEdit *line_posY;
    QLabel *label_posZ;
    QLineEdit *line_posZ;
    QLabel *label_up;
    QLabel *label_upX;
    QLineEdit *line_upX;
    QLabel *label_upY;
    QLineEdit *line_upY;
    QLabel *label_upZ;
    QLineEdit *line_upZ;
    QLabel *label_yaw;
    QLineEdit *line_yaw;
    QLabel *label_pitch;
    QLineEdit *line_pitch;
    QWidget *tab_2;
    QLabel *label_lat;
    QLineEdit *line_lat;
    QLabel *label_anchorGeo;
    QLineEdit *line_anchorGeo;
    QLabel *label_long;
    QLineEdit *line_long;
    QLabel *label_altitude;
    QLineEdit *line_altitude;
    QFrame *line_2;
    QHBoxLayout *hLay_bottom_buttonBox;
    QLabel *label_error;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *camera)
    {
        if (camera->objectName().isEmpty())
            camera->setObjectName(QString::fromUtf8("camera"));
        camera->resize(600, 420);
        camera->setMinimumSize(QSize(600, 420));
        camera->setMaximumSize(QSize(600, 420));
        verticalLayout = new QVBoxLayout(camera);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        line = new QFrame(camera);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        tabWidget = new QTabWidget(camera);
        tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
        tabWidget->setMinimumSize(QSize(400, 300));
        tabWidget->setMaximumSize(QSize(800, 400));
        tab = new QWidget();
        tab->setObjectName(QString::fromUtf8("tab"));
        label_anchorNav = new QLabel(tab);
        label_anchorNav->setObjectName(QString::fromUtf8("label_anchorNav"));
        label_anchorNav->setGeometry(QRect(20, 20, 71, 17));
        QFont font;
        font.setFamily(QString::fromUtf8("Arial"));
        font.setPointSize(12);
        label_anchorNav->setFont(font);
        line_anchorNav = new QLineEdit(tab);
        line_anchorNav->setObjectName(QString::fromUtf8("line_anchorNav"));
        line_anchorNav->setGeometry(QRect(90, 18, 411, 25));
        line_anchorNav->setFont(font);
        label_aim = new QLabel(tab);
        label_aim->setObjectName(QString::fromUtf8("label_aim"));
        label_aim->setGeometry(QRect(20, 60, 71, 17));
        label_aim->setFont(font);
        line_aim = new QLineEdit(tab);
        line_aim->setObjectName(QString::fromUtf8("line_aim"));
        line_aim->setGeometry(QRect(90, 60, 411, 25));
        line_aim->setFont(font);
        label_referenceFrame = new QLabel(tab);
        label_referenceFrame->setObjectName(QString::fromUtf8("label_referenceFrame"));
        label_referenceFrame->setGeometry(QRect(20, 100, 141, 17));
        label_referenceFrame->setFont(font);
        line_referenceFrame = new QLineEdit(tab);
        line_referenceFrame->setObjectName(QString::fromUtf8("line_referenceFrame"));
        line_referenceFrame->setGeometry(QRect(160, 100, 341, 25));
        line_referenceFrame->setFont(font);
        label_pos = new QLabel(tab);
        label_pos->setObjectName(QString::fromUtf8("label_pos"));
        label_pos->setGeometry(QRect(20, 152, 71, 17));
        label_pos->setFont(font);
        label_posX = new QLabel(tab);
        label_posX->setObjectName(QString::fromUtf8("label_posX"));
        label_posX->setGeometry(QRect(100, 152, 16, 17));
        label_posX->setFont(font);
        line_posX = new QLineEdit(tab);
        line_posX->setObjectName(QString::fromUtf8("line_posX"));
        line_posX->setGeometry(QRect(120, 152, 100, 25));
        line_posX->setFont(font);
        label_posY = new QLabel(tab);
        label_posY->setObjectName(QString::fromUtf8("label_posY"));
        label_posY->setGeometry(QRect(240, 152, 16, 17));
        label_posY->setFont(font);
        line_posY = new QLineEdit(tab);
        line_posY->setObjectName(QString::fromUtf8("line_posY"));
        line_posY->setGeometry(QRect(260, 152, 100, 25));
        line_posY->setFont(font);
        label_posZ = new QLabel(tab);
        label_posZ->setObjectName(QString::fromUtf8("label_posZ"));
        label_posZ->setGeometry(QRect(380, 152, 16, 17));
        label_posZ->setFont(font);
        line_posZ = new QLineEdit(tab);
        line_posZ->setObjectName(QString::fromUtf8("line_posZ"));
        line_posZ->setGeometry(QRect(400, 152, 100, 25));
        line_posZ->setFont(font);
        label_up = new QLabel(tab);
        label_up->setObjectName(QString::fromUtf8("label_up"));
        label_up->setGeometry(QRect(20, 192, 71, 17));
        label_up->setFont(font);
        label_upX = new QLabel(tab);
        label_upX->setObjectName(QString::fromUtf8("label_upX"));
        label_upX->setGeometry(QRect(100, 192, 16, 17));
        label_upX->setFont(font);
        line_upX = new QLineEdit(tab);
        line_upX->setObjectName(QString::fromUtf8("line_upX"));
        line_upX->setGeometry(QRect(120, 192, 100, 25));
        line_upX->setFont(font);
        label_upY = new QLabel(tab);
        label_upY->setObjectName(QString::fromUtf8("label_upY"));
        label_upY->setGeometry(QRect(240, 192, 16, 17));
        label_upY->setFont(font);
        line_upY = new QLineEdit(tab);
        line_upY->setObjectName(QString::fromUtf8("line_upY"));
        line_upY->setGeometry(QRect(260, 192, 100, 25));
        line_upY->setFont(font);
        label_upZ = new QLabel(tab);
        label_upZ->setObjectName(QString::fromUtf8("label_upZ"));
        label_upZ->setGeometry(QRect(380, 192, 16, 17));
        label_upZ->setFont(font);
        line_upZ = new QLineEdit(tab);
        line_upZ->setObjectName(QString::fromUtf8("line_upZ"));
        line_upZ->setGeometry(QRect(400, 192, 100, 25));
        line_upZ->setFont(font);
        label_yaw = new QLabel(tab);
        label_yaw->setObjectName(QString::fromUtf8("label_yaw"));
        label_yaw->setGeometry(QRect(20, 232, 91, 17));
        label_yaw->setFont(font);
        line_yaw = new QLineEdit(tab);
        line_yaw->setObjectName(QString::fromUtf8("line_yaw"));
        line_yaw->setGeometry(QRect(120, 232, 121, 25));
        line_yaw->setFont(font);
        label_pitch = new QLabel(tab);
        label_pitch->setObjectName(QString::fromUtf8("label_pitch"));
        label_pitch->setGeometry(QRect(20, 272, 91, 17));
        label_pitch->setFont(font);
        line_pitch = new QLineEdit(tab);
        line_pitch->setObjectName(QString::fromUtf8("line_pitch"));
        line_pitch->setGeometry(QRect(120, 272, 121, 25));
        line_pitch->setFont(font);
        tabWidget->addTab(tab, QString());
        tab_2 = new QWidget();
        tab_2->setObjectName(QString::fromUtf8("tab_2"));
        tab_2->setMinimumSize(QSize(250, 0));
        tab_2->setMaximumSize(QSize(600, 16777215));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        label_anchorGeo = new QLabel(tab_2);
        label_anchorGeo->setObjectName(QString::fromUtf8("label_anchorGeo"));
        label_anchorGeo->setGeometry(QRect(20, 20, 100, 19));
        sizePolicy.setHeightForWidth(label_anchorGeo->sizePolicy().hasHeightForWidth());
        label_anchorGeo->setSizePolicy(sizePolicy);
        label_anchorGeo->setMinimumSize(QSize(100, 0));
        label_anchorGeo->setMaximumSize(QSize(100, 16777215));
        label_anchorGeo->setFont(font);
        line_anchorGeo = new QLineEdit(tab_2);
        line_anchorGeo->setObjectName(QString::fromUtf8("line_anchorGeo"));
        line_anchorGeo->setGeometry(QRect(120, 20, 300, 27));
        line_anchorGeo->setMinimumSize(QSize(200, 0));
        line_anchorGeo->setMaximumSize(QSize(600, 16777215));
        line_anchorGeo->setFont(font);
        label_lat = new QLabel(tab_2);
        label_lat->setObjectName(QString::fromUtf8("label_lat"));
        label_lat->setGeometry(QRect(20, 60, 110, 19));
        sizePolicy.setHeightForWidth(label_lat->sizePolicy().hasHeightForWidth());
        label_lat->setSizePolicy(sizePolicy);
        label_lat->setMinimumSize(QSize(110, 0));
        label_lat->setMaximumSize(QSize(120, 16777215));
        label_lat->setFont(font);
        line_lat = new QLineEdit(tab_2);
        line_lat->setObjectName(QString::fromUtf8("line_lat"));
        line_lat->setGeometry(QRect(120, 60, 200, 27));
        line_lat->setMinimumSize(QSize(200, 0));
        line_lat->setMaximumSize(QSize(600, 16777215));
        line_lat->setFont(font);
        label_long = new QLabel(tab_2);
        label_long->setObjectName(QString::fromUtf8("label_long"));
        label_long->setGeometry(QRect(20, 100, 110, 19));
        sizePolicy.setHeightForWidth(label_long->sizePolicy().hasHeightForWidth());
        label_long->setSizePolicy(sizePolicy);
        label_long->setMinimumSize(QSize(110, 0));
        label_long->setMaximumSize(QSize(120, 16777215));
        label_long->setFont(font);
        line_long = new QLineEdit(tab_2);
        line_long->setObjectName(QString::fromUtf8("line_long"));
        line_long->setGeometry(QRect(120, 100, 200, 27));
        line_long->setFont(font);
        label_altitude = new QLabel(tab_2);
        label_altitude->setObjectName(QString::fromUtf8("label_altitude"));
        label_altitude->setGeometry(QRect(20, 140, 110, 19));
        sizePolicy.setHeightForWidth(label_altitude->sizePolicy().hasHeightForWidth());
        label_altitude->setSizePolicy(sizePolicy);
        label_altitude->setMinimumSize(QSize(110, 0));
        label_altitude->setMaximumSize(QSize(120, 16777215));
        label_altitude->setFont(font);
        line_altitude = new QLineEdit(tab_2);
        line_altitude->setObjectName(QString::fromUtf8("line_altitude"));
        line_altitude->setGeometry(QRect(120, 140, 200, 27));
        line_altitude->setFont(font);
        tabWidget->addTab(tab_2, QString());

        verticalLayout->addWidget(tabWidget);

        line_2 = new QFrame(camera);
        line_2->setObjectName(QString::fromUtf8("line_2"));
        line_2->setFrameShape(QFrame::HLine);
        line_2->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line_2);

        hLay_bottom_buttonBox = new QHBoxLayout();
        hLay_bottom_buttonBox->setObjectName(QString::fromUtf8("hLay_bottom_buttonBox"));

        QFont fontE;
        fontE.setFamily(QString::fromUtf8("Arial"));
        label_error = new QLabel(camera);
        label_error->setObjectName(QString::fromUtf8("label_error"));
        QSizePolicy sizePolicy3(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(label_error->sizePolicy().hasHeightForWidth());
        label_error->setSizePolicy(sizePolicy3);
        label_error->setMinimumSize(QSize(400, 40));
        label_error->setMaximumSize(QSize(800, 40));
        label_error->setFont(fontE);
        label_error->setWordWrap(true);
        hLay_bottom_buttonBox->addWidget(label_error);

        buttonBox = new QDialogButtonBox(camera);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        hLay_bottom_buttonBox->addWidget(buttonBox);
        verticalLayout->addLayout(hLay_bottom_buttonBox);

        retranslateUi(camera);
        //QObject::connect(buttonBox, SIGNAL(accepted()), camera, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), camera, SLOT(reject()));

        tabWidget->setCurrentIndex(1);

        QMetaObject::connectSlotsByName(camera);
    } // setupUi

    void retranslateUi(QDialog *camera)
    {
        camera->setWindowTitle(QCoreApplication::translate("camera", "Set Camera Position", nullptr));
        label_anchorNav->setText(QCoreApplication::translate("camera", "Anchor:", nullptr));
#if QT_CONFIG(tooltip)
        line_anchorNav->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>Anchor camera to this node</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(whatsthis)
        line_anchorNav->setWhatsThis(QString());
#endif // QT_CONFIG(whatsthis)
        label_aim->setText(QCoreApplication::translate("camera", "Aim:", nullptr));
#if QT_CONFIG(tooltip)
        line_aim->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>[OPTIONAL] If specified, camera will be aimed at this node while keeping the anchor node in the same view location</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_referenceFrame->setText(QCoreApplication::translate("camera", "Reference Frame:", nullptr));
#if QT_CONFIG(tooltip)
        line_referenceFrame->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>[OPTIONAL] Camera location in reference to this frame</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_pos->setText(QCoreApplication::translate("camera", "Position:", nullptr));
        label_posX->setText(QCoreApplication::translate("camera", "X", nullptr));
#if QT_CONFIG(tooltip)
        line_posX->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>Camera position vector (x)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_posY->setText(QCoreApplication::translate("camera", "Y", nullptr));
#if QT_CONFIG(tooltip)
        line_posY->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>Camera position vector (y)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_posZ->setText(QCoreApplication::translate("camera", "Z", nullptr));
#if QT_CONFIG(tooltip)
        line_posZ->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>Camera position vector (z)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_up->setText(QCoreApplication::translate("camera", "Up:", nullptr));
        label_upX->setText(QCoreApplication::translate("camera", "X", nullptr));
#if QT_CONFIG(tooltip)
        line_upX->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>[OPTIONAL] Camera up vector (x)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_upY->setText(QCoreApplication::translate("camera", "Y", nullptr));
#if QT_CONFIG(tooltip)
        line_upY->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>[OPTIONAL] Camera up vector (y)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_upZ->setText(QCoreApplication::translate("camera", "Z", nullptr));
#if QT_CONFIG(tooltip)
        line_upZ->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>[OPTIONAL] Camera up vector (z)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_yaw->setText(QCoreApplication::translate("camera", "Yaw angle:", nullptr));
#if QT_CONFIG(tooltip)
        line_yaw->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>[OPTIONAL] yaw angle +/- 360 degrees</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_pitch->setText(QCoreApplication::translate("camera", "Pitch angle:", nullptr));
#if QT_CONFIG(tooltip)
        line_pitch->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>[OPTIONAL] pitch angle +/- 360 degrees</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        tabWidget->setTabText(tabWidget->indexOf(tab), QCoreApplication::translate("camera", "setNavigationState", nullptr));
        label_lat->setText(QCoreApplication::translate("camera", "Latitude:", nullptr));
#if QT_CONFIG(tooltip)
        line_lat->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>Latitude of camera focus point (+/- 90 degrees)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_anchorGeo->setText(QCoreApplication::translate("camera", "Anchor:", nullptr));
#if QT_CONFIG(tooltip)
        line_anchorGeo->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>Anchor camera to this globe (planet/moon)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_long->setText(QCoreApplication::translate("camera", "Longitude:", nullptr));
#if QT_CONFIG(tooltip)
        line_long->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>Longitude of camera focus point (+/- 180 degrees)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        label_altitude->setText(QCoreApplication::translate("camera", "Altitude:", nullptr));
#if QT_CONFIG(tooltip)
        line_altitude->setToolTip(QCoreApplication::translate("camera", "<html><head/><body><p>[OPTIONAL] Altitude of camera (meters)</p></body></html>", nullptr));
#endif // QT_CONFIG(tooltip)
        tabWidget->setTabText(tabWidget->indexOf(tab_2), QCoreApplication::translate("camera", "goToGeo", nullptr));
        label_error->setText(QCoreApplication::translate("camera", "", nullptr));
    } // retranslateUi

};

namespace Ui {
    class camera: public Ui_camera {};
} // namespace Ui

QT_END_NAMESPACE

#endif // __OPENSPACE_UI_LAUNCHER___UI_CAMERA___H__
