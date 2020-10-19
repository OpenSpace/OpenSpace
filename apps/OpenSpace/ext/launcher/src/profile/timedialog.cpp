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

#include "profile/timedialog.h"

#include "profile/line.h"
#include <QComboBox>
#include <QDateTimeEdit>
#include <QDialogButtonBox>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QVBoxLayout>
#include <fmt/format.h>
#include <algorithm>

using namespace openspace;

TimeDialog::TimeDialog(openspace::Profile& profile, QWidget* parent)
    : QDialog(parent)
    , _profile(profile)
{
    setWindowTitle("Time");
    createWidgets();

    QStringList types = { "Absolute", "Relative" };
    _typeCombo->addItems(types);
    if (_profile.time().has_value()) {
        _data = *_profile.time();
        if (_data.type == Profile::Time::Type::Relative) {
            if (_data.value == "") {
                _data.value = "now";
            }
            _relativeEdit->setSelection(0, _relativeEdit->text().length());
        }
        else {
            _absoluteEdit->setSelectedSection(QDateTimeEdit::YearSection);
        }
    }
    else {
        _data.type = Profile::Time::Type::Relative;
        _data.value = "now";
    }
    _initializedAsAbsolute = (_data.type == Profile::Time::Type::Absolute);
    enableAccordingToType(static_cast<int>(_data.type));
}

void TimeDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        layout->addWidget(new QLabel("Time Type"));
        _typeCombo = new QComboBox;
        _typeCombo->setToolTip("Types: Absolute defined time or Relative to actual time");
        connect(
            _typeCombo, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &TimeDialog::enableAccordingToType
        );
        layout->addWidget(_typeCombo);
    }
    {
        _absoluteLabel = new QLabel("Absolute UTC:");
        layout->addWidget(_absoluteLabel);
        _absoluteEdit = new QDateTimeEdit;
        _absoluteEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm:ss");
        _absoluteEdit->setDateTime(QDateTime::currentDateTime());
        layout->addWidget(_absoluteEdit);
    }
    {
        _relativeLabel = new QLabel("Relative Time:");
        layout->addWidget(_relativeLabel);
        _relativeEdit = new QLineEdit;
        _relativeEdit->setToolTip(
            "String for relative time to actual (e.g. \"-1d\" for back 1 day)"
        );
        layout->addWidget(_relativeEdit);
    }
    layout->addWidget(new Line);
    {
        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);

        connect(buttons, &QDialogButtonBox::accepted, this, &TimeDialog::approved);
        QObject::connect(buttons, &QDialogButtonBox::rejected, this, &TimeDialog::reject);
        layout->addWidget(buttons);
    }
}

void TimeDialog::enableAccordingToType(int idx) {
    Profile::Time::Type comboIdx = static_cast<Profile::Time::Type>(idx);
    bool setFormatForAbsolute = (comboIdx == Profile::Time::Type::Absolute);
    enableFormatForAbsolute(setFormatForAbsolute);
    _typeCombo->setCurrentIndex(idx);
    if (comboIdx == Profile::Time::Type::Relative) {
        _relativeEdit->setText("<font color='black'>Relative Time:</font>");
        if (_initializedAsAbsolute) {
            _relativeEdit->setText("now");
        }
        else {
            _relativeEdit->setText(QString::fromStdString(_data.value));
        }
        _relativeEdit->setFocus(Qt::OtherFocusReason);
    }
    else {
        _relativeEdit->setText("<font color='gray'>Relative Time:</font>");
        size_t tIdx = _data.value.find_first_of('T', 0);
        QString importDate = QString::fromStdString(_data.value.substr(0, tIdx));
        QString importTime = QString::fromStdString(_data.value.substr(tIdx + 1));
        _absoluteEdit->setDate(QDate::fromString(importDate, Qt::DateFormat::ISODate));
        _absoluteEdit->setTime(QTime::fromString(importTime));
        _relativeEdit->clear();
        _absoluteEdit->setFocus(Qt::OtherFocusReason);
    }
}

void TimeDialog::enableFormatForAbsolute(bool enableAbs) {
    _absoluteLabel->setEnabled(enableAbs);
    _absoluteEdit->setEnabled(enableAbs);
    _relativeLabel->setEnabled(!enableAbs);
    _relativeEdit->setEnabled(!enableAbs);
}

void TimeDialog::approved() {
    constexpr const int Relative = static_cast<int>(Profile::Time::Type::Relative);
    if (_typeCombo->currentIndex() == Relative) {
        if (_relativeEdit->text().isEmpty()) {
            _profile.clearTime();
        }
        else {
            Profile::Time t;
            t.type = Profile::Time::Type::Relative;
            t.value = _relativeEdit->text().toStdString();
            _profile.setTime(t);
        }
    }
    else {
        Profile::Time t;
        t.type = Profile::Time::Type::Absolute;
        t.value = fmt::format(
            "{}T{}",
            _absoluteEdit->date().toString("yyyy-MM-dd").toStdString(),
            _absoluteEdit->time().toString().toStdString()
        );
        _profile.setTime(t);
    }
    accept();
}
