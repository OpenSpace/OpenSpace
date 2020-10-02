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

#include "deltatimes.h"
#include "./ui_deltatimes.h"
#include <qevent.h>
#include <iostream>
#include <QKeyEvent>

deltaTimes::deltaTimes(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::deltaTimes)
    , _imported(imported)
{
    ui->setupUi(this);

    _data.clear();
    for (double dt : imported->deltaTimes()) {
        _data.push_back(static_cast<int>(dt));
    }

    for (size_t d = 0; d < _data.size(); ++d) {
        QString summary = createSummaryForDeltaTime(d, true);
        ui->listWidget->addItem(new QListWidgetItem(summary));
    }
    connect(ui->listWidget, SIGNAL(itemSelectionChanged()), this,
        SLOT(listItemSelected()));
    connect(ui->line_seconds, &QLineEdit::textChanged, this, &deltaTimes::valueChanged);
    connect(ui->button_save, SIGNAL(clicked()), this, SLOT(saveDeltaTimeValue()));
    connect(ui->button_cancel, SIGNAL(clicked()), this, SLOT(cancelDeltaTimeValue()));
    connect(ui->button_add, SIGNAL(clicked()), this, SLOT(addDeltaTimeValue()));
    connect(ui->button_remove, SIGNAL(clicked()), this, SLOT(removeDeltaTimeValue()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));
    transitionFromEditMode(ui->listWidget->count() - 1);
}

QString deltaTimes::createSummaryForDeltaTime(size_t idx, bool forListView) {
    std::string s;
    int k = (idx%10 == 9) ? 0 : idx%10 + 1;
    k = (idx == 0) ? 1 : k;
    if (idx >= 20) {
        s = "ctrl + " + std::to_string(k);
    }
    else if (idx >= 10) {
        s = "shift + " + std::to_string(k);
    }
    else {
        s = std::to_string(k);
        if (forListView) {
            s += "      ";
        }
    }

    if (forListView) {
        s += "\t" + std::to_string(_data.at(idx));
        s += "\t";
        s += timeDescription(_data.at(idx)).toUtf8().constData();
    }
    return QString(s.c_str());
}

void deltaTimes::listItemSelected() {
    QListWidgetItem *item = ui->listWidget->currentItem();
    int index = ui->listWidget->row(item);

    if (index < (static_cast<int>(_data.size()) - 1)) {
        ui->listWidget->setCurrentRow(index);
    }

    if (_data.size() > 0) {
        if (_data.at(index) == 0) {
            ui->line_seconds->setText("");
        }
        else {
            ui->line_seconds->setText(QString::number(_data.at(index)));
        }
    }
    _editModeNewItem = true;
    transitionToEditMode(index);
}

void deltaTimes::setLabelForKey(int index, bool editMode, QString color) {
    QString labelS = "Set Simulation Time Increment for key";
    if (index >= _data.size()) {
        index = _data.size() - 1;
    }
    if (editMode) {
        labelS += " '";
        labelS += createSummaryForDeltaTime(index, false);
        labelS += "':";
    }
    ui->label_adjust->setText("<font color='" + color + "'>" + labelS + "</font>");
}

QString deltaTimes::timeDescription(int value) {
    QString description;

    if (value == 0) {
        return "";
    }

    size_t i;
    for (i = 0; i < (_timeIntervals.size() - 1); ++i) {
        if (abs(value) >= _timeIntervals[i].secondsPerInterval) {
            break;
        }
    }
    return checkForTimeDescription(i, value);
}

void deltaTimes::valueChanged(const QString& text) {
    if (ui->line_seconds->text() == "") {
        ui->label_value->setText("");
    }
    else if (!isNumericalValue(ui->line_seconds)) {
        ui->label_value->setText("<font color='red'>Not an integer value</font>");
    }
    else {
        int value = ui->line_seconds->text().toInt();
        if (value != 0) {
            ui->label_value->setText("<font color='black'>" +
                timeDescription(ui->line_seconds->text().toInt()) + "</font>");
        }
    }
}

QString deltaTimes::checkForTimeDescription(int intervalIndex, int value) {
    double amount = static_cast<double>(value)
        /_timeIntervals[intervalIndex].secondsPerInterval;
    QString description = QString::number(amount, 'g', 2);
    return description += " " + _timeIntervals[intervalIndex].intervalName + "/sec";
}

bool deltaTimes::isLineEmpty(int index) {
    bool isEmpty = true;
    if (ui->listWidget->item(index)->text().compare("") != 0) {
        isEmpty = false;
    }
    if ((_data.size() > 0) && (_data.at(0) != 0)) {
        isEmpty = false;
    }
    return isEmpty;
}

void deltaTimes::addDeltaTimeValue() {
    int currentListSize = ui->listWidget->count();
    const QString messageAddValue = "  (Enter integer value below & click 'Save')";

    if ((currentListSize == 1) && (isLineEmpty(0))) {
        //Special case where list is "empty" but really has one line that is blank.
        // This is done because QListWidget does not seem to like having its sole
        // remaining item being removed.
        _data.at(0) = 0;
        ui->listWidget->item(0)->setText(messageAddValue);
    }
    else if (_data.size() < _maxSize) {
        if (_data.size() != 0 && _data.back() == 0) {
            return;
        }
        _data.push_back(0);
        ui->listWidget->addItem(new QListWidgetItem(messageAddValue));
    }
    ui->listWidget->setCurrentRow(ui->listWidget->count() - 1);
    ui->line_seconds->setFocus(Qt::OtherFocusReason);
    _editModeNewItem = true;
}

void deltaTimes::saveDeltaTimeValue() {
    QListWidgetItem *item = ui->listWidget->currentItem();
    if (item != nullptr) {
        int index = ui->listWidget->row(item);
        if (_data.size() > 0) {
            if (isNumericalValue(ui->line_seconds)) {
                int value = ui->line_seconds->text().toInt();
                if (value != 0) {
                    _data.at(index) = value;
                    QString summary = createSummaryForDeltaTime(index, true);
                    ui->listWidget->item(index)->setText(summary);
                    setLabelForKey(index, true, "black");
                    transitionFromEditMode(index);
                    _editModeNewItem = false;
                }
                else {
                    setLabelForKey(index, true, "red");
                }
            }
            else {
                setLabelForKey(index, true, "red");
            }
        }
    }
}

void deltaTimes::cancelDeltaTimeValue(void) {
    listItemSelected();
    transitionFromEditMode(ui->listWidget->count() - 1);
    if (_editModeNewItem) {
        if (_data.size() > 0) {
            if (_data.back() == 0) {
                removeDeltaTimeValue();
            }
        }
    }
    _editModeNewItem = false;
}

bool deltaTimes::isNumericalValue(QLineEdit* le) {
    QString s = le->text();
    bool validConversion = false;
    s.toInt(&validConversion, 10);
    return validConversion;
}

void deltaTimes::removeDeltaTimeValue() {
    if (ui->listWidget->count() > 0) {
        if (ui->listWidget->count() == 1) {
            _data.at(0) = 0;
            ui->listWidget->item(0)->setText("");
        }
        else {
            delete ui->listWidget->takeItem(ui->listWidget->count() - 1);
            if (_data.size() > 0) {
                _data.pop_back();
            }
        }
    }
    ui->listWidget->clearSelection();
    transitionFromEditMode(ui->listWidget->count() - 1);
}

void deltaTimes::transitionToEditMode(int index) {
    ui->listWidget->setDisabled(true);
    ui->button_add->setDisabled(true);
    ui->button_remove->setDisabled(true);
    ui->button_cancel->setDisabled(false);
    ui->button_save->setDisabled(false);
    ui->buttonBox->setDisabled(true);

    ui->line_seconds->setFocus(Qt::OtherFocusReason);
    setLabelForKey(index, true, "black");
    editBoxDisabled(false);
}

void deltaTimes::transitionFromEditMode(int index) {
    ui->listWidget->setDisabled(false);
    ui->button_add->setDisabled(false);
    ui->button_remove->setDisabled(false);
    ui->button_cancel->setDisabled(true);
    ui->button_save->setDisabled(true);
    ui->buttonBox->setDisabled(false);

    ui->button_add->setFocus(Qt::OtherFocusReason);
    editBoxDisabled(true);
    setLabelForKey(index, false, "light gray");
    ui->label_value->setText("");
}

void deltaTimes::editBoxDisabled(bool disabled) {
    ui->label_adjust->setDisabled(disabled);
    ui->line_seconds->setDisabled(disabled);
}

void deltaTimes::parseSelections() {
    if ((_data.size() == 1) && (_data.at(0) == 0)) {
        _data.clear();
    }
    int finalNonzeroIndex = _data.size() - 1;
    for (; finalNonzeroIndex >= 0; --finalNonzeroIndex) {
        if (_data.at(finalNonzeroIndex) != 0) {
            break;
        }
    }
    std::vector<double> tempDt;
    for (size_t i = 0; i < (finalNonzeroIndex + 1); ++i) {
        tempDt.push_back(static_cast<double>(_data[i]));
    }
    _imported->setDeltaTimes(tempDt);
    accept();
}

deltaTimes::~deltaTimes() {
    delete ui;
}

void deltaTimes::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return) {
        if (_editModeNewItem) {
            saveDeltaTimeValue();
        }
        else {
            addDeltaTimeValue();
        }
        return;
    }
    else if(evt->key() == Qt::Key_Escape) {
        if (_editModeNewItem) {
            cancelDeltaTimeValue();
            return;
        }
    }
    QDialog::keyPressEvent(evt);
}
