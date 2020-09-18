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
        QString summary = createSummaryForDeltaTime(d, _data.at(d), true);
        ui->listWidget->addItem(new QListWidgetItem(summary));
    }
    connect(ui->listWidget, SIGNAL(itemSelectionChanged()), this, SLOT(listItemSelected()));
    connect(ui->button_save, SIGNAL(clicked()), this, SLOT(saveDeltaTimeValue()));
    connect(ui->button_add, SIGNAL(clicked()), this, SLOT(addDeltaTimeValue()));
    connect(ui->button_remove, SIGNAL(clicked()), this, SLOT(removeDeltaTimeValue()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));
}

QString deltaTimes::createSummaryForDeltaTime(size_t idx, int dt, bool forListView) {
    std::string s;
    int k = (idx%10 == 9) ? 0 : idx%10 + 1;
    k = (idx == 0) ? 1 : k;
    if (idx >= 20) {
        s = "shift + " + std::to_string(k);
    }
    else if (idx >= 10) {
        s = "ctrl + " + std::to_string(k);
    }
    else {
        s = std::to_string(k);
        if (forListView) {
            s += "      ";
        }
    }

    if (dt != 0) {
        s += (forListView) ? "\t" + std::to_string(dt) : "";
    }

    if (forListView) {
        s += "\t";
        s += timeDescription(dt).toUtf8().constData();
    }
    return QString(s.c_str());
}

void deltaTimes::listItemSelected() {
    QListWidgetItem *item = ui->listWidget->currentItem();
    int index = ui->listWidget->row(item);

    if (index < (static_cast<int>(_data.size()) - 1)) {
        //if (index > lastSelectableItem() + 1) {
        //    index = lastSelectableItem() + 1;
        ui->listWidget->setCurrentRow(index);
        //}
    }

    if (_data.size() > 0) {
        QString labelS = "Set Delta Time for key ";
        labelS += createSummaryForDeltaTime(index, _data.at(index), false);
        labelS += " :\t";

        ui->label_adjust->setText(labelS);
        ui->line_seconds->setText(QString::number(_data.at(index)));
    }
}

/*int deltaTimes::lastSelectableItem() {
    if (_data.size() == 0) {
        return 0;
    }
    int i;
    for (i = _data.size() - 1; i >= 0; --i) {
        if (_data._times.at(i) != 0) {
            break;
        }
    }

    if (i < 0) {
        return 0;
    }
    else if (i == (static_cast<int>(_data.size()) - 1)) {
        return _data.size() - 1;
    }
    else {
        return i + 1;
    }
}*/

QString deltaTimes::timeDescription(int value) {
    QString description;

    if (value == 0) {
        return "";
    }

    size_t i;
    for (i = 0; i < _timeIntervals.size(); ++i) {
        if (value >= _timeIntervals[i].secondsPerInterval) {
            break;
        }
    }
    return checkForTimeDescription(i, value);
}

QString deltaTimes::checkForTimeDescription(int intervalIndex, int value) {
//    double amount = static_cast<double>(value) /
//        static_cast<double>(_timeIntervals[intervalIndex].secondsPerInterval);
    int amount = value /_timeIntervals[intervalIndex].secondsPerInterval;
    QString description = QString::number(amount, 'g', 2);
    return description += " " + _timeIntervals[intervalIndex].intervalName + "/sec";
}

void deltaTimes::addDeltaTimeValue() {
    if (_data.size() < _maxSize) {
        if (_data.size() != 0 && _data.back() == 0) {
            return;
        }
        _data.push_back(0);
        QString summary = createSummaryForDeltaTime(_data.size() - 1, 0, true);
        ui->listWidget->addItem(new QListWidgetItem(summary));
    }
}

void deltaTimes::saveDeltaTimeValue() {
    QListWidgetItem *item = ui->listWidget->currentItem();
    if (item != nullptr) {
        int index = ui->listWidget->row(item);
        if (_data.size() > 0) {
            if (isNumericalValue(ui->line_seconds) /*&& index <= lastSelectableItem() + 1*/) {
                _data.at(index) = ui->line_seconds->text().toInt();
                QString summary = createSummaryForDeltaTime(index, _data.at(index), true);
                ui->listWidget->item(index)->setText(summary);
            }
        }
    }
}

bool deltaTimes::isNumericalValue(QLineEdit* le) {
    QString s = le->text();
    bool validConversion = false;
    s.toDouble(&validConversion);
    return validConversion;
}

void deltaTimes::removeDeltaTimeValue() {
    if (ui->listWidget->count() > 0) {
        delete ui->listWidget->takeItem(ui->listWidget->count() - 1);
        if (_data.size() > 0) {
            _data.pop_back();
        }
    }
    ui->listWidget->clearSelection();
}

void deltaTimes::parseSelections() {
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
    for (size_t i = 0; i < ui->listWidget->count(); ++i) {
        delete ui->listWidget->takeItem(i);
    }
    delete ui;
}

void deltaTimes::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    QDialog::keyPressEvent(evt);
}
