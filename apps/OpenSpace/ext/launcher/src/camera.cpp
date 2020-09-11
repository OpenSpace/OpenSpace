#include <openspace/scene/profile.h>
#include "camera.h"
#include "./ui_camera.h"

camera::camera(Camera& imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::camera)
    , _imported(imported)
    , _data(imported)
{
    ui->setupUi(this);
    ui->tabWidget->setCurrentIndex(static_cast<int>(_data.type));
    if (_data.type == Camera::Type::Nav) {
        ui->line_anchorNav->setText(QString(_data.nav.anchor.c_str()));
        ui->line_aim->setText(QString(_data.nav.aim.c_str()));
        ui->line_referenceFrame->setText(QString(_data.nav.referenceFrame.c_str()));
        ui->line_posX->setText(QString(_data.nav.position[0].c_str()));
        ui->line_posY->setText(QString(_data.nav.position[1].c_str()));
        ui->line_posZ->setText(QString(_data.nav.position[2].c_str()));
        ui->line_upX->setText(QString(_data.nav.up[0].c_str()));
        ui->line_upY->setText(QString(_data.nav.up[1].c_str()));
        ui->line_upZ->setText(QString(_data.nav.up[2].c_str()));
        ui->line_yaw->setText(QString(_data.nav.yaw.c_str()));
        ui->line_pitch->setText(QString(_data.nav.pitch.c_str()));
    }
    if (_data.type == Camera::Type::Geo) {
        ui->line_anchorGeo->setText(QString(_data.geo.anchor.c_str()));
        ui->line_lat->setText(QString(_data.geo.latitude.c_str()));
        ui->line_long->setText(QString(_data.geo.longitude.c_str()));
        ui->line_altitude->setText(QString(_data.geo.altitude.c_str()));
    }
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(approved()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
}

bool camera::isNumericalValue(QLineEdit* le) {
    QString s = le->text();
    bool validConversion = false;
    s.toFloat(&validConversion);
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

camera::~camera() {
    delete ui;
}

void camera::cancel() {

}

bool camera::areRequiredFormsFilledAndValid() {
    bool allFormsOk = true;

    if (ui->tabWidget->currentIndex() == 0) {
        checkFormFilled(ui->label_anchorNav, ui->line_anchorNav, "Anchor:", allFormsOk,
            false, true);
        checkFormFilled(ui->label_posX, ui->line_posX, "X", allFormsOk, true, true);
        checkFormFilled(ui->label_posY, ui->line_posY, "Y", allFormsOk, true, true);
        checkFormFilled(ui->label_posZ, ui->line_posZ, "Z", allFormsOk, true, true);
        checkFormFilled(ui->label_upX, ui->line_upX, "X", allFormsOk, true, false);
        checkFormFilled(ui->label_upY, ui->line_upY, "Y", allFormsOk, true, false);
        checkFormFilled(ui->label_upZ, ui->line_upZ, "Z", allFormsOk, true, false);
        checkFormFilled(ui->label_yaw, ui->line_yaw, "Yaw angle:", allFormsOk,
            true, false);
        checkFormFilled(ui->label_pitch, ui->line_pitch, "Pitch angle:", allFormsOk,
            true, false);
        checkFormRange(ui->label_yaw, ui->line_yaw, "Yaw angle:", -360.0, 360.0,
            allFormsOk, false);
        checkFormRange(ui->label_pitch, ui->line_pitch, "Pitch angle:", -360.0, 360.0,
            allFormsOk, false);
    }

    if (ui->tabWidget->currentIndex() == 1) {
        checkFormFilled(ui->label_anchorGeo, ui->line_anchorGeo, "Anchor:", allFormsOk,
            false, true);
        checkFormFilled(ui->label_lat, ui->line_lat, "Latitude:", allFormsOk,
            true, true);
        checkFormFilled(ui->label_long, ui->line_long, "Longitude:", allFormsOk,
            true, true);
        checkFormFilled(ui->label_altitude, ui->line_altitude, "Altitude:", allFormsOk,
            true, false);
        checkFormRange(ui->label_lat, ui->line_lat, "Latitude:", -90.0, 90.0,
            allFormsOk, true);
        checkFormRange(ui->label_long, ui->line_long, "Longitude:", -180.0, 180.0,
            allFormsOk, true);
    }
    return allFormsOk;
}
void camera::checkFormFilled(QLabel* label, QLineEdit* value, const QString& labelTxt,
                             bool& allFormsOk, bool isNumber, bool isRequiredValue)
{
    bool isThisFormValid = true;
    if (value->text().length() == 0 && isRequiredValue) {
        isThisFormValid = false;
    }
    if (value->text().length() > 0 && isNumber && !isNumericalValue(value)) {
        isThisFormValid = false;
    }
    setErrorTextFormat(label, labelTxt, !isThisFormValid);
    if (!isThisFormValid) {
        allFormsOk = false;
    }
}

void camera::checkFormRange(QLabel* label, QLineEdit* value, const QString& labelTxt,
                            float min, float max, bool& allFormsOk, bool isRequiredValue)
{
    if (value->text().length() == 0 && !isRequiredValue) {
        setErrorTextFormat(label, labelTxt, false);
        return;
    }
    bool isThisFormValid = true;
    checkFormFilled(label, value, labelTxt, isThisFormValid, true, isRequiredValue);
    if (isThisFormValid) {
        if (!inNumericalRange(value, min, max)) {
            isThisFormValid = false;
        }
    }
    setErrorTextFormat(label, labelTxt, !isThisFormValid);
    if (!isThisFormValid && (isRequiredValue || value->text().length() > 0)) {
        allFormsOk = false;
    }
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
        _data.type = static_cast<Camera::Type>(ui->tabWidget->currentIndex());
        if (_data.type == Camera::Type::Nav) {
            _data.nav.anchor = ui->line_anchorNav->text().toUtf8().constData();
            _data.nav.aim = ui->line_aim->text().toUtf8().constData();
            _data.nav.referenceFrame = ui->line_referenceFrame->text().toUtf8().constData();
            _data.nav.position[0] = ui->line_posX->text().toUtf8().constData();
            _data.nav.position[1] = ui->line_posY->text().toUtf8().constData();
            _data.nav.position[2] = ui->line_posZ->text().toUtf8().constData();
            _data.nav.up[0] = ui->line_upX->text().toUtf8().constData();
            _data.nav.up[1] = ui->line_upY->text().toUtf8().constData();
            _data.nav.up[2] = ui->line_upZ->text().toUtf8().constData();
            _data.nav.yaw = ui->line_yaw->text().toUtf8().constData();
            _data.nav.pitch = ui->line_pitch->text().toUtf8().constData();
        }
        if (_data.type == Camera::Type::Geo) {
            _data.geo.anchor = ui->line_anchorGeo->text().toUtf8().constData();
            _data.geo.latitude = ui->line_lat->text().toUtf8().constData();
            _data.geo.longitude = ui->line_long->text().toUtf8().constData();
            _data.geo.altitude = ui->line_altitude->text().toUtf8().constData();
        }

        _imported = _data;
        accept();
    }
}
