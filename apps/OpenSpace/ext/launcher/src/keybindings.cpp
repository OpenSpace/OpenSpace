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
#include <openspace/util/keys.h>
#include "keybindings.h"
#include "./ui_keybindings.h"
#include <qevent.h>
#include <algorithm>
#include <QKeyEvent>

keybindings::keybindings(openspace::Profile* imported, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::keybindings)
    , _imported(imported)
    , _data(imported->keybindings())
{
    ui->setupUi(this);

    for (size_t i = 0; i < _data.size(); ++i) {
        ui->list->addItem(new QListWidgetItem(createOneLineSummary(_data[i])));
    }

    QStringList comboModKeysStringList;
    int modIdx = 0;
    for (auto const& m : openspace::KeyModifierNames) {
        comboModKeysStringList += QString(m.second.c_str());
        _mapModKeyComboBoxIndexToKeyValue.push_back(modIdx++);
    }
    ui->combo_keyMod->addItems(comboModKeysStringList);

    QStringList comboKeysStringList;
    for (int i = 0; i < static_cast<int>(openspace::Key::Last); ++i) {
        if (openspace::KeyNames.find(i) != openspace::KeyNames.end()) {
            comboKeysStringList += QString(openspace::KeyNames.at(i).c_str());
            //Create map to relate key combo box to actual integer value defined in Key
            _mapKeyComboBoxIndexToKeyValue.push_back(i);
        }
    }
    ui->combo_key->addItems(comboKeysStringList);

    connect(ui->list, SIGNAL(itemSelectionChanged()), this, SLOT(listItemSelected()));
    connect(ui->button_add, SIGNAL(clicked()), this, SLOT(listItemAdded()));
    connect(ui->button_save, SIGNAL(clicked()), this, SLOT(listItemSave()));
    connect(ui->button_cancel, SIGNAL(clicked()), this, SLOT(listItemCancelSave()));
    connect(ui->button_remove, SIGNAL(clicked()), this, SLOT(listItemRemove()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));

    ui->text_script->setTabChangesFocus(true);
    transitionFromEditMode();
}

QString keybindings::createOneLineSummary(openspace::Profile::Keybinding k) {
    std::string summary;

    int keymod = static_cast<int>(k.key.modifier);
    if (keymod != static_cast<int>(openspace::KeyModifier::NoModifier)) {
        summary += openspace::KeyModifierNames.at(keymod) + " ";
    }
    int keyname = static_cast<int>(k.key.key);

    summary += openspace::KeyNames.at(keyname) + "  ";
    summary += truncateString(k.name) + " (";
    summary += truncateString(k.documentation) + ") @ ";
    summary += truncateString(k.guiPath) + " ";
    summary += (k.isLocal) ? "local" : "remote";
    summary += " `" + truncateString(k.script) + "`";

    return QString(summary.c_str());
}

std::string keybindings::truncateString(std::string& s) {
    const size_t maxLength = 50;
    replaceChars(s, "\n", ";");
    if (s.length() > maxLength) {
        s.resize(maxLength);
        s += "...";
    }
    return s;
}

void keybindings::replaceChars(std::string& src, const std::string& from,
                               const std::string& to)
{
    std::string newString;
    std::string::size_type found, last = 0;

    while((found = src.find(from, last)) != std::string::npos)
    {
        newString.append(src, last, (found - last));
        newString += to;
        last = found + from.length();
    }
    newString += src.substr(last);
    src.swap(newString);
}

void keybindings::listItemSelected(void) {
    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    if (_data.size() > 0) {
        openspace::Profile::Keybinding& k = _data[index];
        ui->combo_keyMod->setCurrentIndex(
            indexInKeyMapping(_mapModKeyComboBoxIndexToKeyValue,
            static_cast<int>(k.key.modifier)));
        ui->combo_key->setCurrentIndex(
            indexInKeyMapping(_mapKeyComboBoxIndexToKeyValue,
            static_cast<int>(k.key.key)));

        //Do key here
        ui->line_name->setText(QString(k.name.c_str()));
        ui->line_documentation->setText(QString(k.documentation.c_str()));
        ui->line_guiPath->setText(QString(k.guiPath.c_str()));
        ui->text_script->setText(QString(k.script.c_str()));
        ui->checkBox_local->setChecked(k.isLocal);
    }
    transitionToEditMode();
}

int keybindings::indexInKeyMapping(std::vector<int>& mapVector, int keyInt) {
    auto it = std::find (
        mapVector.begin(),
        mapVector.end(),
        keyInt
    );
    return std::distance(mapVector.begin(), it);
}

bool keybindings::isLineEmpty(int index) {
    bool isEmpty = true;
    if (ui->list->item(index)->text().compare("") != 0) {
        isEmpty = false;
    }
    if ((_data.size() > 0) && (_data.at(0).name.compare("") != 0)) {
        isEmpty = false;
    }
    return isEmpty;
}

void keybindings::listItemAdded(void) {
    int currentListSize = ui->list->count();

     if ((currentListSize == 1) && (isLineEmpty(0))) {
         //Special case where list is "empty" but really has one line that is blank.
         // This is done because QListWidget does not seem to like having its sole
         // remaining item being removed.
         _data.at(0) = kBlank;
         ui->list->item(0)->setText("  (Enter details below & click 'Save')");
         transitionToEditMode();
     }
     else {
        _data.push_back(kBlank);
        ui->list->addItem(new QListWidgetItem("  (Enter details below & click 'Save')"));
        //Scroll down to that blank line highlighted
        ui->list->setCurrentRow(ui->list->count() - 1);
     }

    //Blank-out the 2 text fields, set combo box to index 0
    ui->line_name->setText(QString(_data.back().name.c_str()));
    ui->line_documentation->setText(QString(_data.back().documentation.c_str()));
    ui->line_guiPath->setText(QString("/"));
    ui->text_script->setText(QString(_data.back().script.c_str()));

    ui->combo_keyMod->setCurrentIndex(static_cast<int>(_data.back().key.modifier));
    ui->combo_key->setCurrentIndex(static_cast<int>(_data.back().key.key));
    ui->combo_keyMod->setFocus(Qt::OtherFocusReason);

    ui->checkBox_local->setChecked(false);
    _editModeNewItem = true;
}

void keybindings::listItemSave(void) {
    if (!areRequiredFormsFilled()) {
        return;
    }

    QListWidgetItem *item = ui->list->currentItem();
    int index = ui->list->row(item);

    if (_data.size() > 0) {
        int keyModIdx = _mapModKeyComboBoxIndexToKeyValue.at(
            ui->combo_keyMod->currentIndex());
        _data[index].key.modifier = static_cast<openspace::KeyModifier>(keyModIdx);
        int keyIdx = _mapKeyComboBoxIndexToKeyValue.at(ui->combo_key->currentIndex());
        _data[index].key.key = static_cast<openspace::Key>(keyIdx);
        _data[index].name = ui->line_name->text().toUtf8().constData();
        _data[index].documentation = ui->line_documentation->text().toUtf8().constData();
        _data[index].guiPath = ui->line_guiPath->text().toUtf8().constData();
        _data[index].script = ui->text_script->toPlainText().toUtf8().constData();
        _data[index].isLocal = (ui->checkBox_local->isChecked());
        ui->list->item(index)->setText(createOneLineSummary(_data[index]));
    }
    transitionFromEditMode();
}

bool keybindings::areRequiredFormsFilled() {
    bool requiredFormsFilled = true;
    QString errors;
    if (ui->combo_key->currentIndex() < 0) {
        errors += "Missing key";
        requiredFormsFilled = false;
    }
    if (ui->line_name->text().length() == 0) {
        if (errors.length() > 0) {
            errors += ", ";
        }
        errors += "Missing keybinding name";
        requiredFormsFilled = false;
    }
    if (ui->text_script->toPlainText().length() == 0) {
        if (errors.length() > 0) {
            errors += ", ";
        }
        errors += "Missing script";
        requiredFormsFilled = false;
    }
    ui->label_error->setText("<font color='red'>" + errors + "</font>");
    return requiredFormsFilled;
}

void keybindings::listItemCancelSave(void) {
    listItemSelected();
    transitionFromEditMode();
    if (_editModeNewItem) {
        if (_data.size() > 0) {
            if(_data.back().name.length() == 0 ||
               _data.back().script.length() == 0 ||
               _data.back().key.key == openspace::Key::Unknown)
            {
                listItemRemove();
            }
        }
    }
    _editModeNewItem = false;
}

void keybindings::listItemRemove(void) {
    if (ui->list->count() > 0) {
        if (ui->list->count() == 1) {
            //Special case where last remaining item is being removed (QListWidget does
            // not like the final item being removed so instead clear it & leave it)
            _data.at(0) = kBlank;
            ui->list->item(0)->setText("");
        }
        else {
            int index = ui->list->currentRow();
            if (index >= 0 && index < ui->list->count()) {
                ui->list->takeItem(index);
                if (_data.size() > 0) {
                    _data.erase(_data.begin() + index);
                }
            }
        }
    }
    ui->list->clearSelection();
    transitionFromEditMode();
}

void keybindings::transitionToEditMode(void) {
    ui->list->setDisabled(true);
    ui->button_add->setDisabled(true);
    ui->button_remove->setDisabled(true);
    ui->button_save->setDisabled(true);
    ui->button_cancel->setDisabled(true);
    ui->buttonBox->setDisabled(true);
    ui->label_key->setText("<font color='black'>Key</font>");
    ui->label_keyMod->setText("<font color='black'>Key Modifier</font>");
    ui->label_name->setText("<font color='black'>Name</font>");
    ui->label_script->setText("<font color='black'>Script</font>");
    ui->label_guiPath->setText("<font color='black'>GUI Path</font>");
    ui->label_documentation->setText("<font color='black'>Documentation</font>");

    editBoxDisabled(false);
    ui->label_error->setText("");
}

void keybindings::transitionFromEditMode(void) {
    ui->list->setDisabled(false);
    ui->button_add->setDisabled(false);
    ui->button_remove->setDisabled(false);
    ui->button_save->setDisabled(false);
    ui->button_cancel->setDisabled(false);
    ui->buttonBox->setDisabled(false);

    ui->label_key->setText("<font color='light gray'>Key</font>");
    ui->label_keyMod->setText("<font color='light gray'>Key Modifier</font>");
    ui->label_name->setText("<font color='light gray'>Name</font>");
    ui->label_script->setText("<font color='light gray'>Script</font>");
    ui->label_guiPath->setText("<font color='light gray'>GUI Path</font>");
    ui->label_documentation->setText("<font color='light gray'>Documentation</font>");
    editBoxDisabled(true);
    ui->label_error->setText("");
}

void keybindings::editBoxDisabled(bool disabled) {
    ui->label_key->setDisabled(disabled);
    ui->combo_key->setDisabled(disabled);
    ui->combo_keyMod->setDisabled(disabled);
    ui->label_name->setDisabled(disabled);
    ui->line_name->setDisabled(disabled);
    ui->label_documentation->setDisabled(disabled);
    ui->line_documentation->setDisabled(disabled);
    ui->label_guiPath->setDisabled(disabled);
    ui->line_guiPath->setDisabled(disabled);
    ui->checkBox_local->setDisabled(disabled);
    ui->label_script->setDisabled(disabled);
    ui->text_script->setDisabled(disabled);
    ui->button_cancel->setDisabled(disabled);
    ui->button_save->setDisabled(disabled);
}

void keybindings::parseSelections() {
    //Handle case with only one remaining but empty line
    if ((_data.size() == 1) && (_data.at(0).name.compare("") == 0)) {
        _data.clear();
    }
    _imported->setKeybindings(_data);
    accept();
}

keybindings::~keybindings() {
    delete ui;
}

void keybindings::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    else if(evt->key() == Qt::Key_Escape) {
        if (_editModeNewItem) {
            listItemCancelSave();
            return;
        }
    }
    QDialog::keyPressEvent(evt);
}

