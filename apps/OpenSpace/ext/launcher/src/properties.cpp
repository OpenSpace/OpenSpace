#include <openspace/scene/profile.h>
#include "properties.h"
#include "./ui_properties.h"
#include <qevent.h>
#include <QKeyEvent>
#include <iostream>

properties::properties(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::properties)
    , _imported(imported)
    , _data(imported->properties())
{
    ui->setupUi(this);

    for (size_t i = 0; i < _data.size(); ++i) {
        ui->list->addItem(new QListWidgetItem(createOneLineSummary(_data[i])));
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

    if (_data.size() > 0) {
        openspace::Profile::Property& p = _data[index];
        if (p.setType == openspace::Profile::Property::SetType::SetPropertyValue) {
            ui->combo_command->setCurrentIndex(0);
        }
        else {
            ui->combo_command->setCurrentIndex(1);
        }
        ui->line_property->setText(QString(p.name.c_str()));
        ui->line_value->setText(QString(p.value.c_str()));
    }
    transitionToEditMode();
}

bool properties::isLineEmpty(int index) {
    bool isEmpty = true;
    if (ui->list->item(index)->text().compare("") != 0) {
        isEmpty = false;
    }
    if ((_data.size() > 0) && (_data.at(0).name.compare("") != 0)) {
        isEmpty = false;
    }
    return isEmpty;
}

void properties::listItemAdded(void) {
    int currentListSize = ui->list->count();

     if ((currentListSize == 1) && (isLineEmpty(0))) {
         //Special case where list is "empty" but really has one line that is blank.
         // This is done because QListWidget does not seem to like having its sole
         // remaining item being removed.
         _data.at(0) = kBlank;
         ui->list->item(0)->setText("  (Enter details below & click 'Save')");
     }
     else {
         _data.push_back(kBlank);
         ui->list->addItem(new QListWidgetItem("  (Enter details below & click 'Save')"));
     }

    //Scroll down to that blank line highlighted
    ui->list->setCurrentRow(ui->list->count() - 1);

    //Blank-out the 2 text fields, set combo box to index 0
    ui->combo_command->setCurrentIndex(0);
    ui->line_property->setText(QString(_data.back().name.c_str()));
    ui->line_value->setText(QString(_data.back().value.c_str()));
    ui->combo_command->setFocus(Qt::OtherFocusReason);
    _editModeNewItem = true;
}

void properties::listItemSave(void) {
    if (!areRequiredFormsFilled()) {
        return;
    }

    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    if ( _data.size() > 0) {
        if (ui->combo_command->currentIndex() == 0) {
            _data[index].setType = openspace::Profile::Property::SetType::SetPropertyValue;
        }
        else {
            _data[index].setType = openspace::Profile::Property::SetType::SetPropertyValueSingle;
        }
        _data[index].name = ui->line_property->text().toUtf8().constData();
        _data[index].value = ui->line_value->text().toUtf8().constData();
        ui->list->item(index)->setText(createOneLineSummary(_data[index]));
    }
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
        if (_data.size() > 0) {
            if (_data.back().name.length() == 0 || _data.back().value.length() == 0) {
                listItemRemove();
            }
        }
    }
    _editModeNewItem = false;
}

void properties::listItemRemove(void) {
    if (ui->list->count() > 0) {
        if (ui->list->currentRow() >= 0 && ui->list->currentRow() < ui->list->count()) {
            if (ui->list->count() == 1) {
                //Special case where last remaining item is being removed (QListWidget does
                // not like the final item being removed so instead clear it & leave it)
                _data.at(0) = kBlank;
                ui->list->item(0)->setText("");
            }
            else {
                int index = ui->list->currentRow();
                if (index >= 0 && index < ui->list->count()) {
                    delete ui->list->takeItem(index);
                    if (_data.size() > 0) {
                        _data.erase(_data.begin() + index);
                    }
                }
            }
        }
    }
    transitionFromEditMode();
}

void properties::transitionToEditMode(void) {
    ui->list->setDisabled(true);
    ui->button_add->setDisabled(true);
    ui->button_remove->setDisabled(true);
    ui->button_cancel->setDisabled(true);
    ui->button_save->setDisabled(true);
    ui->buttonBox->setDisabled(true);

    editBoxDisabled(false);
}

void properties::transitionFromEditMode(void) {
    ui->list->setDisabled(false);
    ui->button_add->setDisabled(false);
    ui->button_remove->setDisabled(false);
    ui->button_cancel->setDisabled(false);
    ui->button_save->setDisabled(false);
    ui->buttonBox->setDisabled(false);

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
    //Handle case with only one remaining but empty line
    if ((_data.size() == 1) && (_data.at(0).name.compare("") == 0)) {
        _data.clear();
    }
    _imported->setProperties(_data);
    accept();
}

properties::~properties() {
    for (size_t i = 0; i < ui->list->count(); ++i) {
        delete ui->list->takeItem(i);
    }
    delete ui;
}

void properties::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return) {
        if (_editModeNewItem) {
            listItemSave();
        }
        return;
    }
    else if(evt->key() == Qt::Key_Escape) {
        if (_editModeNewItem) {
            listItemCancelSave();
        }
        return;
    }
    QDialog::keyPressEvent(evt);
}

