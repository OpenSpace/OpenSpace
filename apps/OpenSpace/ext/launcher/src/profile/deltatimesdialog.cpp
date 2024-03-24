/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include "profile/deltatimesdialog.h"

#include "profile/line.h"
#include <openspace/scene/profile.h>
#include <ghoul/fmt.h>
#include <QDialogButtonBox>
#include <QDoubleValidator>
#include <QEvent>
#include <QKeyEvent>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPushButton>
#include <QVBoxLayout>
#include <array>
#include <iostream>

namespace {
    constexpr int MaxNumberOfKeys = 30;

    struct TimeInterval {
        uint64_t secondsPerInterval;
        std::string intervalName;
    };

    const std::array<TimeInterval, 7> TimeIntervals = {
        TimeInterval{ 31536000, "year" },
        TimeInterval{ 18144000, "month" },
        TimeInterval{ 604800, "week" },
        TimeInterval{ 86400, "day" },
        TimeInterval{ 3600, "hour" },
        TimeInterval{ 60, "minute" },
        TimeInterval{ 1, "second" }
    };

    std::string checkForTimeDescription(int intervalIndex, double value) {
        double amount = value / TimeIntervals[intervalIndex].secondsPerInterval;
        std::string description = std::format("{}", amount);
        description += " " + TimeIntervals[intervalIndex].intervalName + "/sec";
        return description;
    }

    std::string timeDescription(double value) {
        if (value == 0) {
            return "";
        }

        size_t i = 0;
        for (i = 0; i < (TimeIntervals.size() - 1); i++) {
            if (std::abs(value) >= TimeIntervals[i].secondsPerInterval) {
                break;
            }
        }
        return checkForTimeDescription(static_cast<int>(i), value);
    }
} // namespace

DeltaTimesDialog::DeltaTimesDialog(QWidget* parent, std::vector<double>* deltaTimes)
    : QDialog(parent)
    , _deltaTimes(deltaTimes)
    , _deltaTimesData(*_deltaTimes)
{
    setWindowTitle("Simulation Time Increments");
    createWidgets();

    for (size_t d = 0; d < _deltaTimesData.size(); ++d) {
        const std::string summary = createSummaryForDeltaTime(d, true);
        _listWidget->addItem(new QListWidgetItem(QString::fromStdString(summary)));
    }

    transitionEditMode(_listWidget->count() - 1, false);
}

void DeltaTimesDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);

    _listWidget = new QListWidget;
    connect(
        _listWidget, &QListWidget::itemSelectionChanged,
        this, &DeltaTimesDialog::listItemSelected
    );
    _listWidget->setAutoScroll(true);
    _listWidget->setLayoutMode(QListView::SinglePass);
    layout->addWidget(_listWidget);
    
    {
        QBoxLayout* buttonLayout = new QHBoxLayout;
        _addButton = new QPushButton("Add Entry");
        connect(
            _addButton, &QPushButton::clicked,
            this, &DeltaTimesDialog::addDeltaTimeValue
        );
        buttonLayout->addWidget(_addButton);

        _removeButton = new QPushButton("Remove Last Entry");
        connect(
            _removeButton, &QPushButton::clicked,
            this, &DeltaTimesDialog::removeDeltaTimeValue
        );
        buttonLayout->addWidget(_removeButton);

        buttonLayout->addStretch();
        layout->addLayout(buttonLayout);
    }

    _adjustLabel = new QLabel("Set Simulation Time Increment for key");
    layout->addWidget(_adjustLabel);
    
    {
        QBoxLayout* box = new QHBoxLayout;
        _seconds = new QLineEdit;
        _seconds->setValidator(new QDoubleValidator);
        connect(_seconds, &QLineEdit::textChanged, this, &DeltaTimesDialog::valueChanged);
        box->addWidget(_seconds);

        _value = new QLabel;
        box->addWidget(_value);
        layout->addLayout(box);
    }

    {
        QBoxLayout* box = new QHBoxLayout;
        _saveButton = new QPushButton("Save");
        connect(
            _saveButton, &QPushButton::clicked,
            this, &DeltaTimesDialog::saveDeltaTimeValue
        );
        box->addWidget(_saveButton);

        _discardButton = new QPushButton("Discard");
        connect(
            _discardButton, &QPushButton::clicked,
            this, &DeltaTimesDialog::discardDeltaTimeValue
        );
        box->addWidget(_discardButton);

        box->addStretch();
        layout->addLayout(box);
    }
    layout->addWidget(new Line);
    {
        QBoxLayout* footer = new QHBoxLayout;
        _errorMsg = new QLabel;
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footer->addWidget(_errorMsg);

        _buttonBox = new QDialogButtonBox;
        _buttonBox->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(
            _buttonBox, &QDialogButtonBox::accepted,
            this, &DeltaTimesDialog::parseSelections
        );
        connect(_buttonBox, &QDialogButtonBox::rejected, this, &DeltaTimesDialog::reject);
        footer->addWidget(_buttonBox);
        layout->addLayout(footer);
    }
}

std::string DeltaTimesDialog::createSummaryForDeltaTime(size_t idx, bool forListView) {
    int k = (idx % 10 == 9) ? 0 : idx % 10 + 1;
    k = (idx == 0) ? 1 : k;
    const std::string key = std::to_string(k);

    std::string s;
    if (idx >= 20) {
        s = "CTRL + " + key;
    }
    else if (idx >= 10) {
        s = "SHIFT + " + key;
    }
    else {
        s = key;
        if (forListView) {
            s += "      ";
        }
    }

    if (forListView) {
        s += std::format(
            "\t{}\t{}", _deltaTimesData.at(idx), timeDescription(_deltaTimesData.at(idx))
        );
    }
    return s;
}

void DeltaTimesDialog::listItemSelected() {
    QListWidgetItem *item = _listWidget->currentItem();
    const int index = _listWidget->row(item);

    if (index < (static_cast<int>(_deltaTimesData.size()) - 1)) {
        _listWidget->setCurrentRow(index);
    }

    if (!_deltaTimesData.empty()) {
        if (_deltaTimesData.at(index) == 0) {
            _seconds->clear();
        }
        else {
            _seconds->setText(QString::number(_deltaTimesData.at(index)));
        }
    }
    _editModeNewItem = true;
    transitionEditMode(index, true);
}

void DeltaTimesDialog::setLabelForKey(int index, bool editMode, std::string_view color) {
    std::string labelS = "Set Simulation Time Increment for key";
    if (index >= static_cast<int>(_deltaTimesData.size())) {
        index = static_cast<int>(_deltaTimesData.size()) - 1;
    }
    if (editMode) {
        labelS += " '" + createSummaryForDeltaTime(index, false) + "':";
    }
    _adjustLabel->setText(QString::fromStdString(std::format(
        "<font color='{}'>{}</font>", color, labelS
    )));
}

void DeltaTimesDialog::valueChanged(const QString& text) {
    if (text.isEmpty()) {
        _errorMsg->setText("");
    }
    else {
        const double value = text.toDouble();
        if (value != 0.0) {
            _value->setText(QString::fromStdString(timeDescription(value)));
            _errorMsg->setText("");
        }
    }
}

bool DeltaTimesDialog::isLineEmpty(int index) {
    bool isEmpty = true;
    if (!_listWidget->item(index)->text().isEmpty()) {
        isEmpty = false;
    }
    if (!_deltaTimesData.empty() && (_deltaTimesData.at(0) != 0)) {
        isEmpty = false;
    }
    return isEmpty;
}

void DeltaTimesDialog::addDeltaTimeValue() {
    const int currentListSize = _listWidget->count();
    const QString messageAddValue = "  (Enter integer value below & click 'Save')";

    if ((currentListSize == 1) && (isLineEmpty(0))) {
        // Special case where list is "empty" but really has one line that is blank.
        // This is done because QListWidget does not seem to like having its sole
        // remaining item being removed.
        _deltaTimesData.at(0) = 0;
        _listWidget->item(0)->setText(messageAddValue);
    }
    else if (_deltaTimesData.size() < MaxNumberOfKeys) {
        _deltaTimesData.push_back(0);
        _listWidget->addItem(new QListWidgetItem(messageAddValue));
    }
    else {
        _errorMsg->setText("Exceeded maximum amount of simulation time increments");
    }
    _listWidget->setCurrentRow(_listWidget->count() - 1);
    _seconds->setFocus(Qt::OtherFocusReason);
    _editModeNewItem = true;
}

void DeltaTimesDialog::saveDeltaTimeValue() {
    QListWidgetItem* item = _listWidget->currentItem();
    if (item && !_deltaTimesData.empty()) {
        const int index = _listWidget->row(item);
        _deltaTimesData.at(index) = _seconds->text().toDouble();
        const std::string summary = createSummaryForDeltaTime(index, true);
        _listWidget->item(index)->setText(QString::fromStdString(summary));
        transitionEditMode(index, false);
        _editModeNewItem = false;
    }
}

void DeltaTimesDialog::discardDeltaTimeValue() {
    listItemSelected();
    transitionEditMode(_listWidget->count() - 1, false);
    if (_editModeNewItem && !_deltaTimesData.empty() && _deltaTimesData.back() == 0) {
        removeDeltaTimeValue();
    }
    _editModeNewItem = false;
}

void DeltaTimesDialog::removeDeltaTimeValue() {
    if (_listWidget->count() > 0) {
        if (_listWidget->count() == 1) {
            _deltaTimesData.at(0) = 0;
            _listWidget->item(0)->setText("");
        }
        else {
            delete _listWidget->takeItem(_listWidget->count() - 1);
            if (!_deltaTimesData.empty()) {
                _deltaTimesData.pop_back();
            }
        }
    }
    _listWidget->clearSelection();
    transitionEditMode(_listWidget->count() - 1, false);
}

void DeltaTimesDialog::transitionEditMode(int index, bool state) {
    _listWidget->setEnabled(!state);
    _addButton->setEnabled(!state);
    _removeButton->setEnabled(!state);
    _buttonBox->setEnabled(!state);

    _saveButton->setEnabled(state);
    _discardButton->setEnabled(state);
    _adjustLabel->setEnabled(state);
    _seconds->setEnabled(state);
    _errorMsg->clear();

    if (state) {
        _seconds->setFocus(Qt::OtherFocusReason);
        setLabelForKey(index, true, "black");
    }
    else {
        _addButton->setFocus(Qt::OtherFocusReason);
        setLabelForKey(index, false, "light gray");
        _value->clear();
    }
    _errorMsg->clear();
}

void DeltaTimesDialog::parseSelections() {
    if ((_deltaTimesData.size() == 1) && (_deltaTimesData.at(0) == 0)) {
        _deltaTimesData.clear();
    }
    int finalNonzeroIndex = static_cast<int>(_deltaTimesData.size()) - 1;
    for (; finalNonzeroIndex >= 0; --finalNonzeroIndex) {
        if (_deltaTimesData.at(finalNonzeroIndex) != 0) {
            break;
        }
    }
    std::vector<double> tempDt;
    tempDt.reserve(finalNonzeroIndex);
    for (int i = 0; i < (finalNonzeroIndex + 1); i++) {
        tempDt.push_back(_deltaTimesData[i]);
    }
    *_deltaTimes = std::move(_deltaTimesData);
    accept();
}

void DeltaTimesDialog::keyPressEvent(QKeyEvent* evt) {
    if (evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return) {
        if (_editModeNewItem) {
            saveDeltaTimeValue();
        }
        else {
            addDeltaTimeValue();
        }
        return;
    }

    if (evt->key() == Qt::Key_Escape) {
        if (_editModeNewItem) {
            discardDeltaTimeValue();
            return;
        }
    }

    QDialog::keyPressEvent(evt);
}
