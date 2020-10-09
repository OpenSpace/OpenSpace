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

#include <openspace/scene/profile.h>
#include "camera.h"
#include "./ui_camera.h"
#include <QKeyEvent>

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

camera::camera(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::camera)
    , _imported(imported)
{
    ui->setupUi(this);

    if (_imported->camera().has_value()) {
        _data = imported->camera().value();
        std::visit(overloaded {
            [&] (const openspace::Profile::CameraNavState& nav) {
                ui->tabWidget->setCurrentIndex(static_cast<int>(cameraTypeTab::Nav));
                ui->line_anchorNav->setText(QString(nav.anchor.c_str()));
                ui->line_aim->setText(QString(nav.aim->c_str()));
                ui->line_referenceFrame->setText(QString(nav.referenceFrame.c_str()));
                ui->line_posX->setText(QString::number(nav.position.x));
                ui->line_posY->setText(QString::number(nav.position.y));
                ui->line_posZ->setText(QString::number(nav.position.z));
                if (nav.up.has_value()) {
                    ui->line_upX->setText(QString::number(nav.up.value().x));
                    ui->line_upY->setText(QString::number(nav.up.value().y));
                    ui->line_upZ->setText(QString::number(nav.up.value().z));
                }
                else {
                    ui->line_upX->setText("");
                    ui->line_upY->setText("");
                    ui->line_upZ->setText("");
                }
                if (nav.yaw.has_value()) {
                    ui->line_yaw->setText(QString::number(nav.yaw.value()));
                }
                else {
                    ui->line_yaw->setText("");
                }
                if (nav.pitch.has_value()) {
                    ui->line_pitch->setText(QString::number(nav.pitch.value()));
                }
                else {
                    ui->line_pitch->setText("");
                }
                tabSelect(0);
            },
            [&] (const openspace::Profile::CameraGoToGeo& geo) {
                ui->tabWidget->setCurrentIndex(static_cast<int>(cameraTypeTab::Geo));
                ui->line_anchorGeo->setText(QString(geo.anchor.c_str()));
                ui->line_lat->setText(QString::number(geo.latitude));
                ui->line_long->setText(QString::number(geo.longitude));
                if (geo.altitude.has_value()) {
                    ui->line_altitude->setText(QString::number(geo.altitude.value()));
                }
                else {
                    ui->line_altitude->setText("");
                }
                tabSelect(1);
            }
        }, _data);
    }
    else {
        ui->tabWidget->setCurrentIndex(static_cast<int>(cameraTypeTab::Nav));
        ui->line_anchorNav->setText("");
        ui->line_aim->setText("");
        ui->line_referenceFrame->setText("");
        ui->line_posX->setText("");
        ui->line_posY->setText("");
        ui->line_posZ->setText("");
        ui->line_upX->setText("");
        ui->line_upY->setText("");
        ui->line_upZ->setText("");
        ui->line_yaw->setText("");
        ui->line_pitch->setText("");
        ui->line_anchorGeo->setText("");
        ui->line_lat->setText("");
        ui->line_long->setText("");
        ui->line_altitude->setText("");
    }

    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(approved()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
    connect(ui->tabWidget, SIGNAL(tabBarClicked(int)), this, SLOT(tabSelect(int)));
}

bool camera::isNumericalValue(QLineEdit* le) {
    QString s = le->text();
    bool validConversion = false;
    s.toDouble(&validConversion);
    return validConversion;
}

bool camera::inNumericalRange(QLineEdit* le, float min, float max) {
    QString s = le->text();
    bool validConversion = false;
    float value = s.toFloat(&validConversion);
    if (!validConversion) {
        return false;
    }
    if (value < min || value > max) {
        return false;
    }
    return true;
}

bool camera::isEmpty(QLineEdit* textLine) {
    return (textLine->text().length() == 0);
}

camera::~camera() {
    delete ui;
}

void camera::cancel() {

}

bool camera::areRequiredFormsFilledAndValid() {
    bool allFormsOk = true;
    ui->label_error->setText("");

    if (ui->tabWidget->currentIndex() == static_cast<int>(cameraTypeTab::Nav)) {
        if (isEmpty(ui->line_anchorNav)) {
            allFormsOk = false;
            addErrorMsg("Anchor is empty");
        }
        if (isEmpty(ui->line_posX)) {
            allFormsOk = false;
            addErrorMsg("Position X is empty");
        }
        else if (!isNumericalValue(ui->line_posX)) {
            allFormsOk = false;
            addErrorMsg("Position X is not a number");
        }
        if (isEmpty(ui->line_posY)) {
            allFormsOk = false;
            addErrorMsg("Position Y is empty");
        }
        else if (!isNumericalValue(ui->line_posY)) {
            allFormsOk = false;
            addErrorMsg("Position Y is not a number");
        }
        if (isEmpty(ui->line_posZ)) {
            allFormsOk = false;
            addErrorMsg("Position Z is empty");
        }
        else if (!isNumericalValue(ui->line_posZ)) {
            allFormsOk = false;
            addErrorMsg("Position Z is not a number");
        }
        int upVectorCount = 0;
        if (!isEmpty(ui->line_upX)) {
            upVectorCount++;
            if (!isNumericalValue(ui->line_upX)) {
                allFormsOk = false;
                addErrorMsg("Up X is not a number");
            }
        }
        if (!isEmpty(ui->line_upY)) {
            upVectorCount++;
            if (!isNumericalValue(ui->line_upY)) {
                allFormsOk = false;
                addErrorMsg("Up Y is not a number");
            }
        }
        if (!isEmpty(ui->line_upZ)) {
            upVectorCount++;
            if (!isNumericalValue(ui->line_upZ)) {
                allFormsOk = false;
                addErrorMsg("Up Z is not a number");
            }
        }
        if (!(upVectorCount == 0 || upVectorCount == 3)) {
            allFormsOk = false;
            addErrorMsg("Up vector is incomplete");
        }
        if (!isEmpty(ui->line_yaw)) {
            if (!isNumericalValue(ui->line_yaw)) {
                allFormsOk = false;
                addErrorMsg("Yaw value is not a number");
            }
            else if (!inNumericalRange(ui->line_yaw, -360.0, 360.0)) {
                allFormsOk = false;
                addErrorMsg("Yaw value is not in +/- 360.0 range");
            }
        }
        if (!isEmpty(ui->line_pitch)) {
            if (!isNumericalValue(ui->line_pitch)) {
                allFormsOk = false;
                addErrorMsg("Pitch value is not a number");
            }
            else if (!inNumericalRange(ui->line_pitch, -360.0, 360.0)) {
                allFormsOk = false;
                addErrorMsg("Pitch value is not in +/- 360.0 range");
            }
        }
    }

    if (ui->tabWidget->currentIndex() == static_cast<int>(cameraTypeTab::Geo)) {
        if (isEmpty(ui->line_anchorGeo)) {
            allFormsOk = false;
            addErrorMsg("Anchor is empty");
        }
        if (!isNumericalValue(ui->line_lat)) {
            allFormsOk = false;
            addErrorMsg("Latitude value is not a number");
        }
        else if (!inNumericalRange(ui->line_lat, -90.0, 90.0)) {
            allFormsOk = false;
            addErrorMsg("Latitude value is not in +/- 90.0 range");
        }
        if (!isNumericalValue(ui->line_long)) {
            allFormsOk = false;
            addErrorMsg("Longitude value is not a number");
        }
        else if (!inNumericalRange(ui->line_long, -180.0, 180.0)) {
            allFormsOk = false;
            addErrorMsg("Longitude value is not in +/- 180.0 range");
        }
        if (!isEmpty(ui->line_altitude)) {
            if (!isNumericalValue(ui->line_altitude)) {
                allFormsOk = false;
                addErrorMsg("Altitude value is not a number");
            }
        }
    }
    return allFormsOk;
}

void camera::addErrorMsg(const QString& errorDescription) {
    QString contents = ui->label_error->text();
    if (contents.length() > 0) {
        contents += ", ";
    }
    contents += errorDescription;
    ui->label_error->setText("<font color='red'>" + contents + "</font>");
}

void camera::setErrorTextFormat(QLabel* label, const QString& labelTxt,
                                bool setErrorFormat)
{
    QString formatText = "<font color='";
    formatText += (setErrorFormat) ? "red" : "black";
    formatText += "'>";
    formatText += labelTxt;
    formatText += "</font>";
    label->setText(formatText);
}

void camera::approved() {
    if (areRequiredFormsFilledAndValid()) {
        if (ui->tabWidget->currentIndex() == static_cast<int>(cameraTypeTab::Nav)) {
            openspace::Profile::CameraNavState nav;
            nav.anchor = ui->line_anchorNav->text().toUtf8().constData();
            nav.aim = ui->line_aim->text().toUtf8().constData();
            nav.referenceFrame = ui->line_referenceFrame->text().toUtf8().constData();
            nav.position.x = ui->line_posX->text().toDouble();
            nav.position.y = ui->line_posY->text().toDouble();
            nav.position.z = ui->line_posZ->text().toDouble();
            if (isUpVectorValid()) {
                glm::dvec3 u = {
                    ui->line_upX->text().toDouble(),
                    ui->line_upY->text().toDouble(),
                    ui->line_upZ->text().toDouble()
                };
                nav.up = u;
            }
            else {
                nav.up = std::nullopt;
            }
            if (ui->line_yaw->text().length() > 0) {
                nav.yaw = ui->line_yaw->text().toDouble();
            }
            else {
                nav.yaw = std::nullopt;
            }
            if (ui->line_pitch->text().length() > 0) {
                nav.pitch = ui->line_pitch->text().toDouble();
            }
            else {
                nav.pitch = std::nullopt;
            }
            _data = nav;
        }
        else if (ui->tabWidget->currentIndex() == static_cast<int>(cameraTypeTab::Geo)) {
            openspace::Profile::CameraGoToGeo geo;
            geo.anchor = ui->line_anchorGeo->text().toUtf8().constData();
            geo.latitude = ui->line_lat->text().toDouble();
            geo.longitude = ui->line_long->text().toDouble();
            if (ui->line_altitude->text().length() > 0) {
                geo.altitude = ui->line_altitude->text().toDouble();
            }
            _data = geo;
        }

        _imported->setCamera(_data);
        accept();
    }
}

bool camera::isUpVectorValid() {
    return (isNumericalValue(ui->line_upX)
            && isNumericalValue(ui->line_upY)
            && isNumericalValue(ui->line_upZ));
}

void camera::tabSelect(int tabIndex) {
    ui->label_error->setText("");
    if (tabIndex == 0) {
        ui->line_anchorNav->setFocus(Qt::OtherFocusReason);
    }
    else if (tabIndex == 1) {
        ui->line_anchorGeo->setFocus(Qt::OtherFocusReason);
    }
}

void camera::keyPressEvent(QKeyEvent *evt)
{
    QDialog::keyPressEvent(evt);
}

