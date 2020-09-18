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
                ui->line_aim->setText(QString(nav.aim.c_str()));
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

camera::~camera() {
    delete ui;
}

void camera::cancel() {

}

bool camera::areRequiredFormsFilledAndValid() {
    bool allFormsOk = true;

    if (ui->tabWidget->currentIndex() == static_cast<int>(cameraTypeTab::Nav)) {
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

    if (ui->tabWidget->currentIndex() == static_cast<int>(cameraTypeTab::Geo)) {
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

void camera::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    QDialog::keyPressEvent(evt);
}

