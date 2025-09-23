/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <QCheckBox>
#include <QComboBox>
#include <QDateTimeEdit>
#include <QDialogButtonBox>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QTabWidget>
#include <QVBoxLayout>
#include <format>
#include <algorithm>

using namespace openspace;

TimeDialog::TimeDialog(QWidget* parent, std::optional<openspace::Profile::Time>* time)
    : QDialog(parent)
    , _time(time)
{
    setWindowTitle("Time");
    createWidgets();

    if (_time->has_value()) {
        _timeData = **_time;
        if (_timeData.type == Profile::Time::Type::Relative) {
            if (_timeData.value.empty()) {
                _timeData.value = "0d";
            }
            const int len = static_cast<int>(_relativeEdit->text().length());
            _relativeEdit->setSelection(0, len);
        }
        else {
            _absoluteEdit->setSelectedSection(QDateTimeEdit::YearSection);
        }
    }
    else {
        _timeData.type = Profile::Time::Type::Relative;
        _timeData.value = "0d";
    }
    _startPaused->setChecked(_timeData.startPaused);

    if (_timeData.type == Profile::Time::Type::Relative) {
        _relativeEdit->setText(QString::fromStdString(_timeData.value));
        _relativeEdit->setFocus(Qt::OtherFocusReason);
    }
    else {
        const size_t tIdx = _timeData.value.find_first_of('T', 0);
        const QString importDate = QString::fromStdString(
            _timeData.value.substr(0, tIdx)
        );
        const QString importTime = QString::fromStdString(
            _timeData.value.substr(tIdx + 1)
        );
        _absoluteEdit->setDate(QDate::fromString(importDate, Qt::DateFormat::ISODate));
        _absoluteEdit->setTime(QTime::fromString(importTime));
        _relativeEdit->clear();
        _absoluteEdit->setFocus(Qt::OtherFocusReason);
    }

    _tabWidget->setCurrentIndex(static_cast<int>(_timeData.type));
}

void TimeDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);

    _tabWidget = new QTabWidget;

    {
        QWidget* container = new QWidget;
        QBoxLayout* l = new QVBoxLayout(container);
        _absoluteLabel = new QLabel("Absolute UTC:");
        l->addWidget(_absoluteLabel);

        _absoluteEdit = new QDateTimeEdit;
        _absoluteEdit->setDisplayFormat("yyyy-MM-dd  T  hh:mm:ss");
        _absoluteEdit->setDateTime(QDateTime::currentDateTime());
        _absoluteEdit->setAccessibleName("Set absolute time");
        l->addWidget(_absoluteEdit);

        l->addStretch();

        _tabWidget->addTab(container, "Absolute");
    }
    {
        QWidget* container = new QWidget;
        QBoxLayout* l = new QVBoxLayout(container);
        _relativeLabel = new QLabel("Relative Time:");
        l->addWidget(_relativeLabel);

        _relativeEdit = new QLineEdit;
        _relativeEdit->setAccessibleName("Set relative time");
        _relativeEdit->setToolTip(
            "String for relative time to actual (e.g. \"-1d\" for back 1 day)"
        );
        l->addWidget(_relativeEdit);

        QLabel* desc = new QLabel(
            "This field modifies the default start time. It has to be of the form "
            "[-]XX(s,m,h,d,M,y). For example '-1d' will cause the profile to start at "
            "yesterday's date."
        );
        desc->setObjectName("information");
        desc->setWordWrap(true);
        l->addWidget(desc);

        _tabWidget->addTab(container, "Relative");
    }

    layout->addWidget(_tabWidget);

    {
        _startPaused = new QCheckBox("Start with time paused");
        _startPaused->setChecked(false);
        _startPaused->setToolTip(
            "If this is checked, the profile will start with the delta time paused"
        );
        layout->addWidget(_startPaused);
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

void TimeDialog::approved() {
    constexpr int Relative = static_cast<int>(Profile::Time::Type::Relative);
    if (_tabWidget->currentIndex() == Relative) {
        if (_relativeEdit->text().isEmpty()) {
            *_time = std::nullopt;
        }
        else {
            Profile::Time t;
            t.type = Profile::Time::Type::Relative;
            t.value = _relativeEdit->text().toStdString();
            t.startPaused = _startPaused->isChecked();
            *_time = t;
        }
    }
    else {
        Profile::Time t;
        t.type = Profile::Time::Type::Absolute;
        t.value = std::format(
            "{}T{}",
            _absoluteEdit->date().toString("yyyy-MM-dd").toStdString(),
            _absoluteEdit->time().toString().toStdString()
        );
        t.startPaused = _startPaused->isChecked();
        *_time = t;
    }
    accept();
}
