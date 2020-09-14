#include <openspace/scene/profile.h>
#include "properties.h"
#include "./ui_properties.h"
#include <qevent.h>

properties::properties(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::properties)
    , _imported(imported)
    , _data(imported->properties())
{
    ui->setupUi(this);

    for (size_t i = 0; i < _data.size(); ++i) {
        _propListItems.push_back(new QListWidgetItem(createOneLineSummary(_data[i])));
        ui->list->addItem(_propListItems[i]);
    }

    connect(ui->list, SIGNAL(itemSelectionChanged()), this, SLOT(listItemSelected()));
    connect(ui->button_add, SIGNAL(clicked()), this, SLOT(listItemAdded()));
    connect(ui->button_save, SIGNAL(clicked()), this, SLOT(listItemSave()));
    connect(ui->button_cancel, SIGNAL(clicked()), this, SLOT(listItemCancelSave()));
    connect(ui->button_remove, SIGNAL(clicked()), this, SLOT(listItemRemove()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));

    QStringList commandOptions { "SetPropertyValue", "SetPropertyValueSingle" };
    ui->combo_command->addItems(commandOptions);
    transitionFromEditMode();
}

QString properties::createOneLineSummary(openspace::Profile::Property p) {
    QString summary = QString(p.name.c_str());
    summary += " = ";
    summary += QString(p.value.c_str());
    summary += " (SetPropertyValue";
    if (p.setType == openspace::Profile::Property::SetType::SetPropertyValueSingle) {
        summary += "Single";
    }
    summary += ")";
    return summary;
}

void properties::listItemSelected(void) {
    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    openspace::Profile::Property& p = _data[index];
    if (p.setType == openspace::Profile::Property::SetType::SetPropertyValue) {
        ui->combo_command->setCurrentIndex(0);
    }
    else {
        ui->combo_command->setCurrentIndex(1);
    }
    ui->line_property->setText(QString(p.name.c_str()));
    ui->line_value->setText(QString(p.value.c_str()));
    transitionToEditMode();
}

void properties::listItemAdded(void) {
    //Add new line at bottom of props list
    _data.push_back({openspace::Profile::Property::SetType::SetPropertyValue, "", ""});
    _propListItems.push_back(new QListWidgetItem("  (Enter details below and click 'Save')"));
    ui->list->addItem(_propListItems.back());

    //Scroll down to that blank line highlighted
    ui->list->setCurrentItem(_propListItems.back());

    //Blank-out the 2 text fields, set combo box to index 0
    ui->combo_command->setCurrentIndex(0);
    ui->line_property->setText(QString(_data.back().name.c_str()));
    ui->line_value->setText(QString(_data.back().value.c_str()));
    _editModeNewItem = true;
}

void properties::listItemSave(void) {
    if (!areRequiredFormsFilled()) {
        return;
    }

    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    if (ui->combo_command->currentIndex() == 0) {
        _data[index].setType = openspace::Profile::Property::SetType::SetPropertyValue;
    }
    else {
        _data[index].setType = openspace::Profile::Property::SetType::SetPropertyValueSingle;
    }
    _data[index].name = ui->line_property->text().toUtf8().constData();
    _data[index].value = ui->line_value->text().toUtf8().constData();

    _propListItems.at(index)->setText(createOneLineSummary(_data[index]));
    transitionFromEditMode();
    _editModeNewItem = false;
}

bool properties::areRequiredFormsFilled() {
    bool requiredFormsFilled = true;
    if (ui->line_property->text().length() == 0) {
        ui->label_property->setText("<font color='red'>Property</font>");
        requiredFormsFilled = false;
    }
    else {
        ui->label_property->setText("<font color='black'>Property</font>");
    }
    if (ui->line_value->text().length() == 0) {
        ui->label_value->setText("<font color='red'>Value to set</font>");
        requiredFormsFilled = false;
    }
    else {
        ui->label_value->setText("<font color='black'>Value to set</font>");
    }
    return requiredFormsFilled;
}

void properties::listItemCancelSave(void) {
    listItemSelected();
    transitionFromEditMode();
    if (_editModeNewItem) {
        if(_data.back().name.length() == 0 || _data.back().value.length() == 0) {
            listItemRemove();
        }
    }
    _editModeNewItem = false;
}

void properties::listItemRemove(void) {
    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    ui->list->takeItem(index);
    _data.erase(_data.begin() + index);
    _propListItems.erase(_propListItems.begin() + index);
    transitionFromEditMode();
}

void properties::transitionToEditMode(void) {
    ui->list->setDisabled(true);
    ui->button_add->setDisabled(true);
    ui->button_remove->setDisabled(true);

    editBoxDisabled(false);
}

void properties::transitionFromEditMode(void) {
    ui->list->setDisabled(false);
    ui->button_add->setDisabled(false);
    ui->button_remove->setDisabled(false);

    editBoxDisabled(true);
    ui->label_property->setText("<font color='black'>Property</font>");
    ui->label_value->setText("<font color='black'>Value to set</font>");
}

void properties::editBoxDisabled(bool disabled) {
    ui->label_command->setDisabled(disabled);
    ui->combo_command->setDisabled(disabled);
    ui->label_property->setDisabled(disabled);
    ui->line_property->setDisabled(disabled);
    ui->label_value->setDisabled(disabled);
    ui->line_value->setDisabled(disabled);
    ui->button_cancel->setDisabled(disabled);
    ui->button_save->setDisabled(disabled);
}

/*void properties::resizeEvent(QResizeEvent* event)
{
    int sideMargin = 10;
    int windowWidth = event->size().width();
    //QRect qr(sideMargin, 10, windowWidth - (sideMargin * 2), 320);
    //ui->verticalLayout->setGeometry(qr);

    ui->list->setGeometry(
        sideMargin,
        ui->list->y(),
        windowWidth - (sideMargin * 2),
        ui->list->height()
    );

    ui->line_property->setGeometry(
        sideMargin,
        ui->line_property->y(),
        windowWidth - (sideMargin * 2),
        ui->line_property->height()
    );

    ui->line_value->setGeometry(
        sideMargin,
        ui->line_value->y(),
        windowWidth > 400 ? 400 : windowWidth - (sideMargin * 2),
        ui->line_value->height()
    );
}*/

void properties::parseSelections() {
    _imported->setProperties(_data);
    accept();
}

properties::~properties() {
    for (auto p : _propListItems) {
        delete p;
    }
    delete ui;
}
