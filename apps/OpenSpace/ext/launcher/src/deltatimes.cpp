#include "deltatimes.h"
#include "./ui_deltatimes.h"
#include <qevent.h>

deltaTimes::deltaTimes(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::deltaTimes)
    , _imported(imported)
    , _data(imported->deltaTimes())
{
    ui->setupUi(this);

    for (size_t d = 0; d < _data.totalSize(); ++d) {
        QString summary = createSummaryForDeltaTime(d, _data._times.at(d), true);
        _deltaListItems.push_back(new QListWidgetItem(summary));
        ui->listWidget->addItem(_deltaListItems[d]);
    }
    connect(ui->listWidget, SIGNAL(itemSelectionChanged()), this, SLOT(listItemSelected()));
    connect(ui->button_save, SIGNAL(clicked()), this, SLOT(saveDeltaTimeValue()));
    connect(ui->button_remove, SIGNAL(clicked()), this, SLOT(clearDeltaTimeValue()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));
}

QString deltaTimes::createSummaryForDeltaTime(size_t idx, double dt, bool forListView) {
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
        s += (forListView) ? "\t" + std::to_string(static_cast<int>(dt)) : "";
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

    if (index < (static_cast<int>(_data.totalSize()) - 1)) {
        if (index > lastSelectableItem() + 1) {
            index = lastSelectableItem() + 1;
            ui->listWidget->setCurrentRow(index);
        }
    }

    QString labelS = "Set Delta Time for key ";
    labelS += createSummaryForDeltaTime(index, _data._times.at(index), false);
    labelS += " :\t";

    ui->label_adjust->setText(labelS);
    ui->line_seconds->setText(QString::number(_data._times.at(index)));
}

int deltaTimes::lastSelectableItem() {
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
}

QString deltaTimes::timeDescription(double value) {
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

QString deltaTimes::checkForTimeDescription(int intervalIndex, double value) {
    double amount = static_cast<double>(value) /
        static_cast<double>(_timeIntervals[intervalIndex].secondsPerInterval);
    QString description = QString::number(amount, 'g', 2);
    return description += " " + _timeIntervals[intervalIndex].intervalName + "/sec";
}

void deltaTimes::saveDeltaTimeValue() {
    QListWidgetItem *item = ui->listWidget->currentItem();
    int index = ui->listWidget->row(item);
    if (isNumericalValue(ui->line_seconds) && index <= lastSelectableItem() + 1) {
        _data._times.at(index) = ui->line_seconds->text().toDouble();
        QString summary = createSummaryForDeltaTime(index, _data._times.at(index), true);
        _deltaListItems.at(index)->setText(summary);
    }
}

bool deltaTimes::isNumericalValue(QLineEdit* le) {
    QString s = le->text();
    bool validConversion = false;
    s.toDouble(&validConversion);
    return validConversion;
}

void deltaTimes::clearDeltaTimeValue() {
    int i;
    for (i = _data.size() - 1; i >= 0; --i) {
        if (_data._times.at(i) != 0) {
            break;
        }
    }
    if (i >= 0) {
        _data._times.at(i) = 0;
        QString summary = createSummaryForDeltaTime(i, _data._times.at(i), true);
        _deltaListItems.at(i)->setText(summary);
        ui->listWidget->setCurrentRow(i);
    }
}

void deltaTimes::parseSelections() {
    _imported->setDeltaTimes(_data._times);
    accept();
}

deltaTimes::~deltaTimes() {
    for (auto d : _deltaListItems) {
        delete d;
    }
    delete ui;
}
