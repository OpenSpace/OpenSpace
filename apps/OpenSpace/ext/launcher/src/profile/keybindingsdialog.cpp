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

#include "profile/keybindingsdialog.h"

#include <openspace/scene/profile.h>
#include <openspace/util/keys.h>
#include <qevent.h>
#include <algorithm>
#include <QKeyEvent>
#include <QCheckBox>
#include <QVBoxLayout>
#include <QPushButton>
#include <QLabel>
#include <QComboBox>
#include <QLineEdit>
#include <QTextEdit>
#include <QDialogButtonBox>

namespace {
    const openspace::Profile::Keybinding kBlank = {
        { openspace::Key::Unknown, openspace::KeyModifier::NoModifier },
        "",
        "",
        "",
        true,
        ""
    };

    void replaceChars(std::string& src, const std::string& from, const std::string& to) {
        std::string newString;
        std::string::size_type found, last = 0;

        while ((found = src.find(from, last)) != std::string::npos)
        {
            newString.append(src, last, (found - last));
            newString += to;
            last = found + from.length();
        }
        newString += src.substr(last);
        src.swap(newString);
    }

    std::string truncateString(std::string& s) {
        const size_t maxLength = 50;
        replaceChars(s, "\n", ";");
        if (s.length() > maxLength) {
            s.resize(maxLength);
            s += "...";
        }
        return s;
    }

    std::string createOneLineSummary(openspace::Profile::Keybinding k) {
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

        return summary;
    }

} // namespace

KeybindingsDialog::KeybindingsDialog(openspace::Profile& profile, QWidget *parent)
    : QDialog(parent)
    , _profile(profile)
    , _data(_profile.keybindings())
{
    setWindowTitle("Assign Keybindings");

    QBoxLayout* layout = new QVBoxLayout(this);
    {
        _list = new QListWidget;
        connect(
            _list, &QListWidget::itemSelectionChanged,
            this, &KeybindingsDialog::listItemSelected
        );
        _list->setAlternatingRowColors(true);
        _list->setMovement(QListView::Free);
        _list->setResizeMode(QListView::Adjust);

        for (size_t i = 0; i < _data.size(); ++i) {
            std::string summary = createOneLineSummary(_data[i]);
            _list->addItem(new QListWidgetItem(QString::fromStdString(summary)));
        }

        layout->addWidget(_list);
    }
    {
        QBoxLayout* box = new QHBoxLayout;
        _addButton = new QPushButton("Add new");
        connect(
            _addButton, &QPushButton::clicked,
            this, &KeybindingsDialog::listItemAdded
        );
        box->addWidget(_addButton);

        _removeButton = new QPushButton("Remove");
        connect(
            _removeButton, &QPushButton::clicked,
            this, &KeybindingsDialog::listItemRemove
        );
        box->addWidget(_removeButton);
        box->addStretch();
        layout->addLayout(box);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        layout->addWidget(line);
    }
    {
        QGridLayout* box = new QGridLayout;

        _keyModLabel = new QLabel("Key Modifier");
        box->addWidget(_keyModLabel, 0, 0);
        _keyModCombo = new QComboBox;
        _keyModCombo->setToolTip(
            "Modifier keys to hold while key is pressed (blank means none)"
        );

        QStringList comboModKeysStringList;
        int modIdx = 0;
        for (const std::pair<const int, std::string>& m : openspace::KeyModifierNames) {
            comboModKeysStringList += QString::fromStdString(m.second);
            _mapModKeyComboBoxIndexToKeyValue.push_back(modIdx++);
        }
        _keyModCombo->addItems(comboModKeysStringList);
        box->addWidget(_keyModCombo, 0, 1);


        _keyLabel = new QLabel("Key");
        box->addWidget(_keyLabel, 1, 0);
        _keyCombo = new QComboBox;
        _keyCombo->setToolTip("Key to press for this keybinding");

        QStringList comboKeysStringList;
        for (int i = 0; i < static_cast<int>(openspace::Key::Last); ++i) {
            if (openspace::KeyNames.find(i) != openspace::KeyNames.end()) {
                comboKeysStringList += QString(openspace::KeyNames.at(i).c_str());
                // Create map to relate key combo box to integer value defined in Key
                _mapKeyComboBoxIndexToKeyValue.push_back(i);
            }
        }
        _keyCombo->addItems(comboKeysStringList);
        box->addWidget(_keyCombo, 1, 1);


        _nameLabel = new QLabel("Name:");
        box->addWidget(_nameLabel, 2, 0);
        _nameEdit = new QLineEdit;
        _nameEdit->setToolTip("Name assigned to this keybinding");
        box->addWidget(_nameEdit, 2, 1);


        _guiPathLabel = new QLabel("GUI Path:");
        box->addWidget(_guiPathLabel, 3, 0);
        _guiPathEdit = new QLineEdit;
        _guiPathEdit->setToolTip(
            "[OPTIONAL] Path for where this keybinding appears in GUI menu"
        );
        box->addWidget(_guiPathEdit, 3, 1);


        _documentationLabel = new QLabel("Documentation:");
        box->addWidget(_documentationLabel, 4, 0);
        _documentationEdit = new QLineEdit;
        _documentationEdit->setToolTip(
            "[OPTIONAL] Documentation entry for keybinding"
        );
        box->addWidget(_documentationEdit, 4, 1);


        _localCheck = new QCheckBox("Local");
        _localCheck->setToolTip(
            "Determines whether the command, when executed, should be shared with "
            "connected instances or only executed locally"
        );
        box->addWidget(_localCheck, 5, 0, 1, 2);


        _scriptLabel = new QLabel("Script");
        box->addWidget(_scriptLabel, 6, 0, 1, 2);
        _scriptEdit = new QTextEdit;
        _scriptEdit->setToolTip("Command(s) to execute at keypress event");
        _scriptEdit->setTabChangesFocus(true);
        box->addWidget(_scriptEdit, 7, 0, 1, 2);
        box->setRowStretch(7, 1);

        QBoxLayout* buttonBox = new QHBoxLayout;
        _saveButton = new QPushButton("Save");
        connect(
            _saveButton, &QPushButton::clicked,
            this, &KeybindingsDialog::listItemSave
        );
        buttonBox->addWidget(_saveButton);

        _cancelButton = new QPushButton("Cancel");
        connect(
            _cancelButton, &QPushButton::clicked,
            this, &KeybindingsDialog::listItemCancelSave
        );
        buttonBox->addWidget(_cancelButton);
        buttonBox->addStretch();
        box->addLayout(buttonBox, 8, 1, 1, 2);
        layout->addLayout(box);
    }
    {
        QFrame* line = new QFrame;
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);
        layout->addWidget(line);
    }
    {
        QBoxLayout* footerLayout = new QHBoxLayout;

        _errorMsg = new QLabel;
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footerLayout->addWidget(_errorMsg);

        _buttonBox = new QDialogButtonBox;
        _buttonBox->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        QObject::connect(
            _buttonBox, &QDialogButtonBox::accepted,
            this, &KeybindingsDialog::parseSelections
        );
        QObject::connect(
            _buttonBox, &QDialogButtonBox::rejected,
            this, &KeybindingsDialog::reject
        );
        footerLayout->addWidget(_buttonBox);
        layout->addLayout(footerLayout);
    }

    transitionFromEditMode();
}

void KeybindingsDialog::listItemSelected(void) {
    QListWidgetItem *item = _list->currentItem();
    int index = _list->row(item);

    if (_data.size() > 0) {
        openspace::Profile::Keybinding& k = _data[index];
        _keyModCombo->setCurrentIndex(
            indexInKeyMapping(_mapModKeyComboBoxIndexToKeyValue,
            static_cast<int>(k.key.modifier))
        );
        _keyCombo->setCurrentIndex(
            indexInKeyMapping(_mapKeyComboBoxIndexToKeyValue,
            static_cast<int>(k.key.key))
        );

        // Do key here
        _nameEdit->setText(QString(k.name.c_str()));
        _guiPathEdit->setText(QString(k.guiPath.c_str()));
        _documentationEdit->setText(QString(k.documentation.c_str()));
        _localCheck->setChecked(k.isLocal);
        _scriptEdit->setText(QString(k.script.c_str()));
    }
    transitionToEditMode();
}

int KeybindingsDialog::indexInKeyMapping(std::vector<int>& mapVector, int keyInt) {
    auto it = std::find(mapVector.begin(), mapVector.end(), keyInt);
    return std::distance(mapVector.begin(), it);
}

bool KeybindingsDialog::isLineEmpty(int index) {
    bool isEmpty = true;
    if (!_list->item(index)->text().isEmpty()) {
        isEmpty = false;
    }
    if (!_data.empty() && !_data.at(0).name.empty()) {
        isEmpty = false;
    }
    return isEmpty;
}

void KeybindingsDialog::listItemAdded(void) {
    int currentListSize = _list->count();

     if ((currentListSize == 1) && (isLineEmpty(0))) {
         //Special case where list is "empty" but really has one line that is blank.
         // This is done because QListWidget does not seem to like having its sole
         // remaining item being removed.
         _data.at(0) = kBlank;
         _list->item(0)->setText("  (Enter details below & click 'Save')");
         transitionToEditMode();
     }
     else {
        _data.push_back(kBlank);
        _list->addItem(new QListWidgetItem("  (Enter details below & click 'Save')"));
        //Scroll down to that blank line highlighted
        _list->setCurrentRow(_list->count() - 1);
     }

    //Blank-out the 2 text fields, set combo box to index 0
     _keyModCombo->setCurrentIndex(static_cast<int>(_data.back().key.modifier));
     _keyCombo->setCurrentIndex(static_cast<int>(_data.back().key.key));
     _keyModCombo->setFocus(Qt::OtherFocusReason);
     _nameEdit->setText(QString::fromStdString(_data.back().name));
    _guiPathEdit->setText("/");
    _documentationEdit->setText(QString::fromStdString(_data.back().documentation));
    _localCheck->setChecked(false);
    _scriptEdit->setText(QString::fromStdString(_data.back().script));

    _editModeNewItem = true;
}

void KeybindingsDialog::listItemSave(void) {
    if (!areRequiredFormsFilled()) {
        return;
    }

    QListWidgetItem* item = _list->currentItem();
    int index = _list->row(item);

    if (!_data.empty()) {
        int keyModIdx = _mapModKeyComboBoxIndexToKeyValue.at(
            _keyModCombo->currentIndex());
        _data[index].key.modifier = static_cast<openspace::KeyModifier>(keyModIdx);
        int keyIdx = _mapKeyComboBoxIndexToKeyValue.at(_keyCombo->currentIndex());
        _data[index].key.key = static_cast<openspace::Key>(keyIdx);
        _data[index].name = _nameEdit->text().toUtf8().constData();
        _data[index].guiPath = _guiPathEdit->text().toUtf8().constData();
        _data[index].documentation = _documentationEdit->text().toUtf8().constData();
        _data[index].script = _scriptEdit->toPlainText().toUtf8().constData();
        _data[index].isLocal = (_localCheck->isChecked());
        std::string summary = createOneLineSummary(_data[index]);
        _list->item(index)->setText(QString::fromStdString(summary));
    }
    transitionFromEditMode();
}

bool KeybindingsDialog::areRequiredFormsFilled() {
    bool requiredFormsFilled = true;
    std::string errors;
    if (_keyCombo->currentIndex() < 0) {
        errors += "Missing key";
        requiredFormsFilled = false;
    }
    if (_nameEdit->text().length() == 0) {
        if (errors.length() > 0) {
            errors += ", ";
        }
        errors += "Missing keybinding name";
        requiredFormsFilled = false;
    }
    if (_scriptEdit->toPlainText().length() == 0) {
        if (errors.length() > 0) {
            errors += ", ";
        }
        errors += "Missing script";
        requiredFormsFilled = false;
    }
    _errorMsg->setText(QString::fromStdString(errors));
    return requiredFormsFilled;
}

void KeybindingsDialog::listItemCancelSave(void) {
    listItemSelected();
    transitionFromEditMode();
    if (_editModeNewItem && !_data.empty() &&
        (_data.back().name.length() == 0 || _data.back().script.length() == 0 ||
        _data.back().key.key == openspace::Key::Unknown))
    {
        listItemRemove();
    }
    _editModeNewItem = false;
}

void KeybindingsDialog::listItemRemove(void) {
    if (_list->count() > 0) {
        if (_list->count() == 1) {
            // Special case where last remaining item is being removed (QListWidget does
            // not like the final item being removed so instead clear it & leave it)
            _data.at(0) = kBlank;
            _list->item(0)->setText("");
        }
        else {
            int index = _list->currentRow();
            if (index >= 0 && index < _list->count()) {
                _list->takeItem(index);
                if (_data.size() > 0) {
                    _data.erase(_data.begin() + index);
                }
            }
        }
    }
    _list->clearSelection();
    transitionFromEditMode();
}

void KeybindingsDialog::transitionToEditMode() {
    _list->setDisabled(true);
    _addButton->setDisabled(true);
    _removeButton->setDisabled(true);
    _saveButton->setDisabled(true);
    _cancelButton->setDisabled(true);
    _buttonBox->setDisabled(true);
    _keyLabel->setText("<font color='black'>Key</font>");
    _keyModLabel->setText("<font color='black'>Key Modifier</font>");
    _nameLabel->setText("<font color='black'>Name</font>");
    _scriptLabel->setText("<font color='black'>Script</font>");
    _guiPathLabel->setText("<font color='black'>GUI Path</font>");
    _documentationLabel->setText("<font color='black'>Documentation</font>");

    editBoxDisabled(false);
    _errorMsg->setText("");
}

void KeybindingsDialog::transitionFromEditMode(void) {
    _list->setDisabled(false);
    _addButton->setDisabled(false);
    _removeButton->setDisabled(false);
    _saveButton->setDisabled(false);
    _cancelButton->setDisabled(false);
    _buttonBox->setDisabled(false);

    _keyLabel->setText("<font color='light gray'>Key</font>");
    _keyModLabel->setText("<font color='light gray'>Key Modifier</font>");
    _nameLabel->setText("<font color='light gray'>Name</font>");
    _scriptLabel->setText("<font color='light gray'>Script</font>");
    _guiPathLabel->setText("<font color='light gray'>GUI Path</font>");
    _documentationLabel->setText("<font color='light gray'>Documentation</font>");
    editBoxDisabled(true);
    _errorMsg->setText("");
}

void KeybindingsDialog::editBoxDisabled(bool disabled) {
    _keyLabel->setDisabled(disabled);
    _keyCombo->setDisabled(disabled);
    _keyModLabel->setDisabled(disabled);
    _keyModCombo->setDisabled(disabled);
    _nameLabel->setDisabled(disabled);
    _nameEdit->setDisabled(disabled);
    _guiPathLabel->setDisabled(disabled);
    _guiPathEdit->setDisabled(disabled);
    _documentationLabel->setDisabled(disabled);
    _documentationEdit->setDisabled(disabled);
    _localCheck->setDisabled(disabled);
    _scriptLabel->setDisabled(disabled);
    _scriptEdit->setDisabled(disabled);
    _cancelButton->setDisabled(disabled);
    _saveButton->setDisabled(disabled);
}

void KeybindingsDialog::parseSelections() {
    // Handle case with only one remaining but empty line
    if ((_data.size() == 1) && (_data.at(0).name.empty())) {
        _data.clear();
    }
    _profile.setKeybindings(_data);
    accept();
}

void KeybindingsDialog::keyPressEvent(QKeyEvent* evt) {
    if (evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return) {
        return;
    }
    else if (evt->key() == Qt::Key_Escape) {
        if (_editModeNewItem) {
            listItemCancelSave();
            return;
        }
    }
    QDialog::keyPressEvent(evt);
}

