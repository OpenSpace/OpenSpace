#include "osmodules.h"
#include "./ui_osmodules.h"
#include <qevent.h>

osmodules::osmodules(std::vector<Module>& imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::osmodules)
    , _imported(imported)
    , _data(imported)
{
    ui->setupUi(this);

    for (size_t i = 0; i < _data.size(); ++i) {
        _modulesListItems.push_back(new QListWidgetItem(createOneLineSummary(_data[i])));
        ui->list->addItem(_modulesListItems[i]);
    }

    connect(ui->list, SIGNAL(itemSelectionChanged()), this, SLOT(listItemSelected()));
    connect(ui->button_add, SIGNAL(clicked()), this, SLOT(listItemAdded()));
    connect(ui->button_save, SIGNAL(clicked()), this, SLOT(listItemSave()));
    connect(ui->button_cancel, SIGNAL(clicked()), this, SLOT(listItemCancelSave()));
    connect(ui->button_remove, SIGNAL(clicked()), this, SLOT(listItemRemove()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));

    transitionFromEditMode();
}

QString osmodules::createOneLineSummary(Module m) {
    QString summary = QString(m.name.c_str());
    bool hasCommandForLoaded = (m.loadedInstruction.length() > 0);
    bool hasCommandForNotLoaded = (m.notLoadedInstruction.length() > 0);

    if (hasCommandForLoaded && hasCommandForNotLoaded) {
        summary += " (commands set for both loaded & not-loaded conditions)";
    }
    else if (hasCommandForLoaded) {
        summary += " (command set only for loaded condition)";
    }
    else if (hasCommandForNotLoaded) {
        summary += " (command set only for NOT loaded condition)";
    }
    else {
        summary += " (no commands set)";
    }
    return summary;
}

void osmodules::listItemSelected(void) {
    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    Module& m = _data[index];
    ui->line_module->setText(QString(m.name.c_str()));
    ui->line_loaded->setText(QString(m.loadedInstruction.c_str()));
    ui->line_notLoaded->setText(QString(m.notLoadedInstruction.c_str()));
    transitionToEditMode();
}

void osmodules::listItemAdded(void) {
    //Add new line at bottom of props list
    _data.push_back({"", "", ""});
    _modulesListItems.push_back(new QListWidgetItem("  (Enter details below and click 'Save')"));
    ui->list->addItem(_modulesListItems.back());

    //Scroll down to that blank line highlighted
    ui->list->setCurrentItem(_modulesListItems.back());

    //Blank-out the 2 text fields, set combo box to index 0
    ui->line_module->setText(QString(_data.back().name.c_str()));
    ui->line_loaded->setText(QString(_data.back().loadedInstruction.c_str()));
    ui->line_notLoaded->setText(QString(_data.back().notLoadedInstruction.c_str()));
    _editModeNewItem = true;
}

void osmodules::listItemSave(void) {
    if (ui->line_module->text().length() == 0) {
        ui->label_module->setText("<font color='red'>Module</font>");
        return;
    }

    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    _data[index].name = ui->line_module->text().toUtf8().constData();
    _data[index].loadedInstruction = ui->line_loaded->toPlainText().toUtf8().constData();
    _data[index].notLoadedInstruction = ui->line_notLoaded->toPlainText().toUtf8().constData();

    _modulesListItems.at(index)->setText(createOneLineSummary(_data[index]));
    transitionFromEditMode();
    _editModeNewItem = false;
}

void osmodules::listItemCancelSave(void) {
    listItemSelected();
    transitionFromEditMode();
    if (_editModeNewItem) {
        if(_data.back().name.length() == 0) {
            listItemRemove();
        }
    }
    _editModeNewItem = false;
}

void osmodules::listItemRemove(void) {
    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    ui->list->takeItem(index);
    _data.erase(_data.begin() + index);
    _modulesListItems.erase(_modulesListItems.begin() + index);
    transitionFromEditMode();
}

void osmodules::transitionToEditMode(void) {
    ui->list->setDisabled(true);
    ui->button_add->setDisabled(true);
    ui->button_remove->setDisabled(true);

    editBoxDisabled(false);
}

void osmodules::transitionFromEditMode(void) {
    ui->list->setDisabled(false);
    ui->button_add->setDisabled(false);
    ui->button_remove->setDisabled(false);

    editBoxDisabled(true);
    ui->label_module->setText("<font color='black'>Module</font>");
}

void osmodules::editBoxDisabled(bool disabled) {
    ui->label_module->setDisabled(disabled);
    ui->line_module->setDisabled(disabled);
    ui->label_loaded->setDisabled(disabled);
    ui->line_loaded->setDisabled(disabled);
    ui->label_notLoaded->setDisabled(disabled);
    ui->line_notLoaded->setDisabled(disabled);
    ui->button_cancel->setDisabled(disabled);
    ui->button_save->setDisabled(disabled);
}

//osmodules::save() {
//
//}

void osmodules::parseSelections() {
    _imported = _data;
    accept();
}

osmodules::~osmodules() {
    for (auto p : _modulesListItems) {
        delete p;
    }
    delete ui;
}
