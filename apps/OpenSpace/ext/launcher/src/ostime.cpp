#include <openspace/scene/profile.h>
#include "ostime.h"
#include "./ui_ostime.h"
#include <algorithm>

ostime::ostime(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::time)
    , _imported(imported)
{
    ui->setupUi(this);

    if (_imported->time().has_value()) {
        QStringList types { "Absolute", "Relative" };
        ui->combo_type->addItems(types);
	_data = _imported->time().value();
    }
    else {
        _data.type = openspace::Profile::Time::Type::Relative;
	_data.time = "";
    }
    _initializedAsAbsolute = (_data.type == openspace::Profile::Time::Type::Absolute);
    enableAccordingToType(static_cast<int>(_data.type));

    connect(ui->combo_type, SIGNAL(currentIndexChanged(int)), this,
        SLOT(enableAccordingToType(int)));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(approved()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
}

void ostime::enableAccordingToType(int idx) {
    openspace::Profile::Time::Type comboIdx = static_cast<openspace::Profile::Time::Type>(idx);
    bool setFormatForAbsolute = (comboIdx == openspace::Profile::Time::Type::Absolute);
    enableFormatForAbsolute(setFormatForAbsolute);
    ui->combo_type->setCurrentIndex(idx);
    if (comboIdx == openspace::Profile::Time::Type::Relative) {
        ui->label_relative->setText("<font color='black'>Relative Time:</font>");
        if (_initializedAsAbsolute) {
            ui->line_relative->setText("");
        }
        else {
            ui->line_relative->setText(QString(_data.time.c_str()));
        }
    }
    else {
        ui->label_relative->setText("<font color='gray'>Relative Time:</font>");
        size_t tIdx = _data.time.find_first_of('T', 0);
        QString importDate = QString(_data.time.substr(0, tIdx).c_str());
        QString importTime = QString(_data.time.substr(tIdx + 1).c_str());
        ui->dateEdit->setDate(QDate::fromString(importDate, Qt::DateFormat::ISODate));
        ui->timeEdit->setTime(QTime::fromString(importTime));
        ui->line_relative->setText("");
    }
}

void ostime::enableFormatForAbsolute(bool enableAbs) {
    ui->label_absolete->setEnabled(enableAbs);
    ui->dateEdit->setEnabled(enableAbs);
    ui->timeEdit->setEnabled(enableAbs);
    ui->label_relative->setEnabled(!enableAbs);
    ui->line_relative->setEnabled(!enableAbs);
}

ostime::~ostime() {
    delete ui;
}

void ostime::cancel() {

}

void ostime::approved() {
    if (ui->combo_type->currentIndex() == static_cast<int>(openspace::Profile::Time::Type::Relative)) {
        if (ui->line_relative->text().length() == 0) {
            //ui->label_relative->setText("<font color='red'>Relative Time:</font>");
            //return;
	    _imported->clearTime();
        }
	else {
	    openspace::Profile::Time t;
            t.type = openspace::Profile::Time::Type::Relative;
            t.time = ui->line_relative->text().toUtf8().constData();
	    _imported->setTime(t);
	}
    }
    else {
        openspace::Profile::Time t;
        t.type = openspace::Profile::Time::Type::Absolute;
        QString res = ui->dateEdit->date().toString("yyyy-MM-dd") + "T" + ui->timeEdit->time().toString();
        t.time = res.toUtf8().constData();
        _imported->setTime(t);
    }
    accept();
}
