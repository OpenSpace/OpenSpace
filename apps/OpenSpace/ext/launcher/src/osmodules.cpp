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

#include <openspace/scene/profile.h>
#include "osmodules.h"
#include "./ui_osmodules.h"
#include <qevent.h>
#include <QKeyEvent>
#include <iostream>

osmodules::osmodules(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::osmodules)
    , _imported(imported)
    , _data(imported->modules())
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

    ui->line_loaded->setTabChangesFocus(true);
    ui->line_notLoaded->setTabChangesFocus(true);
    transitionFromEditMode();
}

QString osmodules::createOneLineSummary(openspace::Profile::Module m) {
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

    if (_data.size() > 0) {
        openspace::Profile::Module& m = _data[index];
        ui->line_module->setText(QString(m.name.c_str()));
        ui->line_loaded->setText(QString(m.loadedInstruction.c_str()));
        ui->line_notLoaded->setText(QString(m.notLoadedInstruction.c_str()));
    }
    transitionToEditMode();
}

bool osmodules::isLineEmpty(int index) {
    bool isEmpty = true;
    if (ui->list->item(index)->text().compare("") != 0) {
        isEmpty = false;
    }
    if ((_data.size() > 0) && (_data.at(0).name.compare("") != 0)) {
        isEmpty = false;
    }
    return isEmpty;
}

void osmodules::listItemAdded(void) {
    int currentListSize = ui->list->count();

    if ((currentListSize == 1) && (isLineEmpty(0))) {
        //Special case where list is "empty" but really has one line that is blank.
        // This is done because QListWidget does not seem to like having its sole
        // remaining item being removed.
        _data.at(0) = kBlank;
        ui->list->item(0)->setText("  (Enter details below & click 'Save')");
        ui->list->setCurrentRow(0);
        transitionToEditMode();
    }
    else {
        _data.push_back(kBlank);
        ui->list->addItem(new QListWidgetItem("  (Enter details below & click 'Save')"));
        //Scroll down to that blank line highlighted
        ui->list->setCurrentRow(ui->list->count() - 1);
        ui->label_error->setText("");
    }

    //Blank-out the 2 text fields, set combo box to index 0
    ui->line_module->setText(QString(_data.back().name.c_str()));
    ui->line_loaded->setText(QString(_data.back().loadedInstruction.c_str()));
    ui->line_notLoaded->setText(QString(_data.back().notLoadedInstruction.c_str()));
    ui->line_module->setFocus(Qt::OtherFocusReason);
    _editModeNewItem = true;
}

void osmodules::listItemSave(void) {
    if (ui->line_module->text().length() == 0) {
        //ui->label_module->setText("<font color='red'>Module</font>");
        ui->label_error->setText("<font color='red'>Missing module name</font>");
        return;
    }

    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    if ( _data.size() > 0) {
        _data[index].name = ui->line_module->text().toUtf8().constData();
        _data[index].loadedInstruction
            = ui->line_loaded->toPlainText().toUtf8().constData();
        _data[index].notLoadedInstruction
            = ui->line_notLoaded->toPlainText().toUtf8().constData();
        ui->list->item(index)->setText(createOneLineSummary(_data[index]));
    }
    transitionFromEditMode();
    _editModeNewItem = false;
}

void osmodules::listItemCancelSave(void) {
    transitionFromEditMode();
    if (_editModeNewItem) {
        if (_data.size() > 0) {
            if(_data.back().name.length() == 0) {
                listItemRemove();
            }
        }
    }
    _editModeNewItem = false;
}

void osmodules::listItemRemove(void) {
    if (ui->list->count() > 0) {
        if (ui->list->currentRow() >= 0 && ui->list->currentRow() < ui->list->count()) {
            if (ui->list->count() == 1) {
                //Special case where last remaining item is being removed (QListWidget
                // doesn't like the final item being removed so instead clear it)
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

void osmodules::transitionToEditMode(void) {
    ui->list->setDisabled(true);
    ui->button_add->setDisabled(true);
    ui->button_remove->setDisabled(true);
    ui->button_cancel->setDisabled(true);
    ui->button_save->setDisabled(true);
    ui->buttonBox->setDisabled(true);

    ui->label_module->setText("<font color='black'>Module</font>");
    ui->label_loaded->setText("<font color='black'>Command if Module is Loaded</font>");
    ui->label_notLoaded->setText("<font color='black'>Command if Module is NOT Loaded</font>");
    editBoxDisabled(false);
    ui->label_error->setText("");
}

void osmodules::transitionFromEditMode(void) {
    ui->list->setDisabled(false);
    ui->button_add->setDisabled(false);
    ui->button_remove->setDisabled(false);
    ui->button_cancel->setDisabled(false);
    ui->button_save->setDisabled(false);
    ui->buttonBox->setDisabled(false);

    editBoxDisabled(true);
    ui->label_module->setText("<font color='light gray'>Module</font>");
    ui->label_loaded->setText("<font color='light gray'>Command if Module is Loaded</font>");
    ui->label_notLoaded->setText("<font color='light gray'>Command if Module is NOT Loaded</font>");
    ui->label_error->setText("");
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

void osmodules::parseSelections() {
    //Handle case with only one remaining but empty line
    if ((_data.size() == 1) && (_data.at(0).name.compare("") == 0)) {
        _data.clear();
    }
    _imported->setModules(_data);
    accept();
}

osmodules::~osmodules() {
    delete ui;
}

void osmodules::keyPressEvent(QKeyEvent *evt)
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
            return;
        }
    }
    QDialog::keyPressEvent(evt);
}

