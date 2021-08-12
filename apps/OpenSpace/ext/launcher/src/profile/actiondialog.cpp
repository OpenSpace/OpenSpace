/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include "profile/actiondialog.h"

#include "profile/line.h"
#include <openspace/util/keys.h>
#include <QCheckBox>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QMessageBox>
#include <QPushButton>
#include <QTextEdit>
#include <QVBoxLayout>

using namespace openspace;

namespace {
    void updateListItem(QListWidgetItem* item, const Profile::Action& action) {
        ghoul_assert(item, "Item must exist at this point");
        item->setText(
            action.name.empty() ?
            QString::fromStdString(action.identifier) :
            QString::fromStdString(action.name)
        );
    }

    void updateListItem(QListWidgetItem* item, const Profile::Keybinding& kb) {
        ghoul_assert(item, "Item must exist at this point");
        std::string name = fmt::format("{}\t{}", ghoul::to_string(kb.key), kb.action);
        item->setText(QString::fromStdString(name));
    }
} // namespace

ActionDialog::ActionDialog(Profile& profile, QWidget* parent)
    : QDialog(parent)
    , _profile(profile)
{
    _actions.data = _profile.actions;
    _keybindings.data = _profile.keybindings;

    setWindowTitle("Actions and Keybindings");
    createWidgets();
}

void ActionDialog::createWidgets() {
    //         Column 0            Column 1         Column 2
    //  *----------------------*---------------*----------------*
    //  |                      | Identifier    | [oooooooooooo] |    Row 0
    //  |                      | Name          | [oooooooooooo] |    Row 1
    //  |                      | GUI Path      | [oooooooooooo] |    Row 2
    //  |                      | Documentation | [oooooooooooo] |    Row 3
    //  |                      | Is Local      | []             |    Row 4
    //  |                      | Script        | [oooooooooooo] |    Row 5
    //  *----------------------*---------------*----------------*
    //  | [+] [-]              |               | [Save] [Cancel]|    Row 6
    //  *----------------------*---------------*----------------*
    //  |=======================================================|    Row 7
    //  *----------------------*---------------*----------------|
    //  |                      | Modifier      | []S []C []A    |    Row 8
    //  |                      | Key           | DDDDDDDDDDDD>  |    Row 9
    //  |                      | Actions       | [oooooooooooo] |    Row 10
    //  |                      | Add action    | DDDDDDDDD> [+] |    Row 11
    //  |                      |               |                |    Row 12
    //  *----------------------*---------------*----------------*
    //  | [+] [-]              |               | [Save] [Cancel]|    Row 13
    //  *----------------------*---------------*----------------*
    //  |=======================================================|    Row 14
    //  *----------------------*---------------*----------------*
    //  |                                      | [Save] [Cancel]|    Row 15
    //  *----------------------*---------------*----------------*

    QGridLayout* layout = new QGridLayout(this);
    
    createActionWidgets(layout);
    clearActionFields();

    layout->addWidget(new Line, 7, 0, 1, 3);

    createKeyboardWidgets(layout);
    clearKeybindingFields();

    layout->addWidget(new Line, 14, 0, 1, 3);
    
    QDialogButtonBox* buttonBox = new QDialogButtonBox;
    buttonBox->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
    QObject::connect(
        buttonBox, &QDialogButtonBox::accepted,
        this, &ActionDialog::applyChanges
    );
    QObject::connect(
        buttonBox, &QDialogButtonBox::rejected,
        this, &ActionDialog::reject
    );
    layout->addWidget(buttonBox, 15, 2, Qt::AlignRight);
}

void ActionDialog::createActionWidgets(QGridLayout* layout) {
    _actions.list = new QListWidget;
    connect(
        _actions.list, &QListWidget::itemSelectionChanged,
        this, &ActionDialog::actionSelected
    );
    _actions.list->setAlternatingRowColors(true);
    _actions.list->setResizeMode(QListView::Adjust);

    for (size_t i = 0; i < _actions.data.size(); ++i) {
        const Profile::Action& action = _actions.data[i];
        std::string name = action.name.empty() ? action.identifier : action.name;
        _actions.list->addItem(new QListWidgetItem(QString::fromStdString(name)));
    }

    layout->addWidget(_actions.list, 0, 0, 6, 1);

    layout->addWidget(new QLabel("Identifier"), 0, 1);
    _actions.identifier = new QLineEdit;
    _actions.identifier->setEnabled(false);
    layout->addWidget(_actions.identifier, 0, 2);

    layout->addWidget(new QLabel("Name"), 1, 1);
    _actions.name = new QLineEdit;
    _actions.name->setEnabled(false);
    layout->addWidget(_actions.name, 1, 2);

    layout->addWidget(new QLabel("GUI Path"), 2, 1);
    _actions.guiPath = new QLineEdit;
    _actions.guiPath->setEnabled(false);
    layout->addWidget(_actions.guiPath, 2, 2);

    layout->addWidget(new QLabel("Documentation"), 3, 1);
    _actions.documentation = new QLineEdit;
    _actions.documentation->setEnabled(false);
    layout->addWidget(_actions.documentation, 3, 2);

    layout->addWidget(new QLabel("Is Local"), 4, 1);
    _actions.isLocal = new QCheckBox;
    _actions.isLocal->setEnabled(false);
    layout->addWidget(_actions.isLocal, 4, 2);

    layout->addWidget(new QLabel("Script"), 5, 1);
    _actions.script = new QTextEdit;
    _actions.script->setEnabled(false);
    layout->addWidget(_actions.script, 5, 2);


    // + / - buttons
    QWidget* container = new QWidget;
    QBoxLayout* containerLayout = new QHBoxLayout(container);
    _actions.addButton = new QPushButton("+");
    _actions.addButton->setObjectName("add-button");
    QObject::connect(
        _actions.addButton, &QPushButton::clicked,
        this, &ActionDialog::actionAdd
    );
    containerLayout->addWidget(_actions.addButton);

    _actions.removeButton = new QPushButton("-");
    _actions.removeButton->setObjectName("remove-button");
    _actions.removeButton->setEnabled(false);
    QObject::connect(
        _actions.removeButton, &QPushButton::clicked,
        this, &ActionDialog::actionRemove
    );
    containerLayout->addWidget(_actions.removeButton);
    layout->addWidget(container, 6, 0, Qt::AlignLeft);


    // Save / Cancel buttons
    _actions.saveButtons = new QDialogButtonBox;
    _actions.saveButtons->setEnabled(false);
    _actions.saveButtons->setStandardButtons(
        QDialogButtonBox::Save | QDialogButtonBox::Cancel
    );
    QObject::connect(
        _actions.saveButtons, &QDialogButtonBox::accepted,
        this, &ActionDialog::actionSaved
    );
    QObject::connect(
        _actions.saveButtons, &QDialogButtonBox::rejected,
        this, &ActionDialog::actionRejected
    );
    layout->addWidget(_actions.saveButtons, 6, 2, Qt::AlignRight);
}

void ActionDialog::createKeyboardWidgets(QGridLayout* layout) {
    _keybindings.list = new QListWidget;
    connect(
        _keybindings.list, &QListWidget::itemSelectionChanged,
        this, &ActionDialog::keybindingSelected
    );
    _keybindings.list->setAlternatingRowColors(true);
    _keybindings.list->setResizeMode(QListView::Adjust);

    for (size_t i = 0; i < _keybindings.data.size(); ++i) {
        const Profile::Keybinding& kv = _keybindings.data[i];
        QListWidgetItem* item = new QListWidgetItem;
        updateListItem(item, kv);
        _keybindings.list->addItem(item);
    }

    layout->addWidget(_keybindings.list, 8, 0, 5, 1);

    //QWidget* controls = new QWidget;
    //QGridLayout* controlsLayout = new QGridLayout(controls);
    layout->addWidget(new QLabel("Modifier"), 8, 1);
    {
        QWidget* container = new QWidget;
        QBoxLayout* containerLayout = new QHBoxLayout(container);
        _keybindings.shiftModifier = new QCheckBox("Shift");
        _keybindings.shiftModifier->setEnabled(false);
        containerLayout->addWidget(_keybindings.shiftModifier);
        _keybindings.ctrlModifier = new QCheckBox("Control");
        _keybindings.ctrlModifier->setEnabled(false);
        containerLayout->addWidget(_keybindings.ctrlModifier);
        _keybindings.altModifier = new QCheckBox("Alt");
        _keybindings.altModifier->setEnabled(false);
        containerLayout->addWidget(_keybindings.altModifier);
        layout->addWidget(container, 8, 2);
    }

    layout->addWidget(new QLabel("Key"), 9, 1);
    _keybindings.key = new QComboBox;
    QStringList keyList;
    for (const KeyInfo& ki : KeyInfos) {
        keyList += QString::fromStdString(std::string(ki.name));
    }
    _keybindings.key->addItems(keyList);
    _keybindings.key->setCurrentIndex(-1);
    _keybindings.key->setEnabled(false);
    layout->addWidget(_keybindings.key, 9, 2);

    layout->addWidget(new QLabel("Action"), 10, 1);
    _keybindings.action = new QComboBox;
    for (const Profile::Action& action : _actions.data) {
        _keybindings.action->addItem(QString::fromStdString(action.identifier));
    }
    _keybindings.action->setEnabled(false);
    layout->addWidget(_keybindings.action, 10, 2);


    // +/- buttons
    QWidget* container = new QWidget;
    QBoxLayout* containerLayout = new QHBoxLayout(container);
    _keybindings.addButton = new QPushButton("+");
    _keybindings.addButton->setObjectName("add-button");
    QObject::connect(
        _keybindings.addButton, &QPushButton::clicked,
        this, &ActionDialog::keybindingAdd
    );
    containerLayout->addWidget(_keybindings.addButton);

    _keybindings.removeButton = new QPushButton("-");
    _keybindings.removeButton->setObjectName("remove-button");
    _keybindings.removeButton->setEnabled(false);
    QObject::connect(
        _keybindings.removeButton, &QPushButton::clicked,
        this, &ActionDialog::keybindingRemove
    );
    containerLayout->addWidget(_keybindings.removeButton);
    layout->addWidget(container, 13, 0, Qt::AlignLeft);

    // Save/Cancel
    _keybindings.saveButtons = new QDialogButtonBox;
    _keybindings.saveButtons->setEnabled(false);
    _keybindings.saveButtons->setStandardButtons(
        QDialogButtonBox::Save | QDialogButtonBox::Cancel
    );
    QObject::connect(
        _keybindings.saveButtons, &QDialogButtonBox::accepted,
        this, &ActionDialog::keybindingSaved
    );
    QObject::connect(
        _keybindings.saveButtons, &QDialogButtonBox::rejected,
        this, &ActionDialog::keybindingRejected
    );

    layout->addWidget(_keybindings.saveButtons, 13, 2, Qt::AlignRight);
}

void ActionDialog::applyChanges() {
    _profile.actions = std::move(_actions.data);
    _profile.keybindings = std::move(_keybindings.data);
    accept();
}

Profile::Action* ActionDialog::selectedAction() {
    QListWidgetItem* item = _actions.list->currentItem();
    const int idx = _actions.list->row(item);
    return idx != -1 ? &_actions.data[idx] : nullptr;
}

void ActionDialog::actionAdd() {
    _actions.list->addItem("");
    _actions.data.push_back(Profile::Action());
    _actions.list->setCurrentRow(_actions.list->count() - 1);
}

void ActionDialog::actionRemove() {
    const openspace::Profile::Action* action = selectedAction();
    ghoul_assert(action, "An action must exist at this point");

    ghoul_assert(
        _actions.list->count() == static_cast<int>(_actions.data.size()),
        "Action list and data has desynced"
    );

    // We can't remove an action if it has a keyboard shortcut attached to it
    for (size_t i = 0; i < _keybindings.data.size(); ++i) {
        const Profile::Keybinding& kb = _keybindings.data[i];
        if (kb.action != action->identifier) {
            continue;
        }
        QMessageBox::StandardButton button = QMessageBox::information(
            this,
            "Remove action",
            QString::fromStdString(fmt::format(
                "Action '{}' is used in the keybind '{}' and cannot be removed unless "
                "the keybind is removed as well. Do you want to remove the keybind as "
                "well?",
                action->identifier, ghoul::to_string(kb.key)
            )),
            QMessageBox::StandardButton::Yes,
            QMessageBox::StandardButton::No
        );
        if (button == QMessageBox::StandardButton::Yes) {
            _keybindings.data.erase(_keybindings.data.begin() + i);
            delete _keybindings.list->takeItem(static_cast<int>(i));
            i--;
        }
        else {
            // If the user chooses 'No' at least once, we have to bail
            return;
        }
    }

    for (size_t i = 0; i < _actions.data.size(); ++i) {
        if (_actions.data[i].identifier == action->identifier) {
            _actions.data.erase(_actions.data.begin() + i);
            delete _actions.list->takeItem(static_cast<int>(i));
            clearActionFields();
            return;
        }
    }

    ghoul_assert(false, "We shouldn't be able to get here");
}

void ActionDialog::actionSelected() {
    const Profile::Action* action = selectedAction();
    if (action) {
        // Action selected
        _actions.identifier->setText(QString::fromStdString(action->identifier));
        _actions.identifier->setEnabled(true);
        _actions.name->setText(QString::fromStdString(action->name));
        _actions.name->setEnabled(true);
        _actions.guiPath->setText(QString::fromStdString(action->guiPath));
        _actions.guiPath->setEnabled(true);
        _actions.documentation->setText(QString::fromStdString(action->documentation));
        _actions.documentation->setEnabled(true);
        _actions.isLocal->setChecked(action->isLocal);
        _actions.isLocal->setEnabled(true);
        _actions.script->setText(QString::fromStdString(action->script));
        _actions.script->setEnabled(true);
        _actions.addButton->setEnabled(false);
        _actions.removeButton->setEnabled(true);
        _actions.saveButtons->setEnabled(true);
    }
    else {
        // No action selected
        _actions.addButton->setEnabled(true);
        _actions.removeButton->setEnabled(false);
        _actions.saveButtons->setEnabled(false);
    }
}

void ActionDialog::actionSaved() {
    std::string newIdentifier = _actions.identifier->text().toStdString();
    if (newIdentifier.empty()) {
        QMessageBox::critical(this, "Empty identifier", "Identifier must not be empty");
        return;
    }

    Profile::Action* action = selectedAction();
    std::string oldIdentifier = action->identifier;
    if (oldIdentifier != newIdentifier) {
        // The identifier is a bit special as we need to make sure that we didn't
        // accidentally create a duplicate.  Also if we did create a duplicate we need to
        // make sure that we update all keybinds that referenced the old value are updated
        // to the new one

        const auto it = std::find_if(
            _actions.data.begin(), _actions.data.end(),
            [id = newIdentifier](const Profile::Action& action) {
                return action.identifier == id;
            }
        );
        if (it != _actions.data.end()) {
            QMessageBox::critical(
                this,
                "Duplicate identifier",
                "The chosen identifier was already used in another action. Identifiers "
                "have to be unique. Please choose a different identfier."
            );
            return;
        }

        // If we got this far, we have a new identifier and it is a new one, so we need to
        // update other keybinds now
        ghoul_assert(
            _keybindings.list->count() == _keybindings.data.size(),
            "The list and data got out of sync"
        );
        for (int i = 0; i < _keybindings.list->count(); ++i) {
            if (_keybindings.data[i].action == oldIdentifier) {
                _keybindings.data[i].action = newIdentifier;
                updateListItem(_keybindings.list->item(i), _keybindings.data[i]);
            }
        }
        for (int i = 0; i < _keybindings.action->count(); ++i) {
            if (_keybindings.action->itemText(i).toStdString() == oldIdentifier) {
                _keybindings.action->setItemText(
                    i,
                    QString::fromStdString(newIdentifier)
                );
            }
        }
        action->identifier = newIdentifier;
    }
    

    action->name = _actions.name->text().toStdString();
    action->guiPath = _actions.guiPath->text().toStdString();
    action->documentation = _actions.documentation->text().toStdString();
    action->isLocal = _actions.isLocal->isChecked();
    action->script = _actions.script->toPlainText().toStdString();

    updateListItem(_actions.list->currentItem(), *action);
    clearActionFields();
}

void ActionDialog::clearActionFields() {
    _actions.list->setCurrentRow(-1);
    _actions.identifier->clear();
    _actions.identifier->setEnabled(false);
    _actions.name->clear();
    _actions.name->setEnabled(false);
    _actions.guiPath->clear();
    _actions.guiPath->setEnabled(false);
    _actions.documentation->clear();
    _actions.documentation->setEnabled(false);
    _actions.isLocal->setChecked(false);
    _actions.isLocal->setEnabled(false);
    _actions.script->clear();
    _actions.script->setEnabled(false);
    _actions.saveButtons->setEnabled(false);
}

void ActionDialog::actionRejected() {
    if (_actions.data.back().identifier.empty()) {
        // This happens if someone creates a new action and never gave an identifier
        delete _actions.list->takeItem(_actions.list->count() - 1);
        _actions.data.erase(_actions.data.begin() + _actions.data.size() - 1);
    }

    clearActionFields();
}

Profile::Keybinding* ActionDialog::selectedKeybinding() {
    QListWidgetItem* item = _keybindings.list->currentItem();
    const int idx = _keybindings.list->row(item);
    return idx != -1 ? &_keybindings.data[idx] : nullptr;
}

void ActionDialog::keybindingAdd() {
    _keybindings.list->addItem("");
    _keybindings.data.push_back(Profile::Keybinding());
    _keybindings.list->setCurrentRow(_keybindings.list->count() - 1);
}

void ActionDialog::keybindingRemove() {
    const Profile::Keybinding* keybinding = selectedKeybinding();
    ghoul_assert(keybinding, "A keybinding must be selected at this point");
    
    for (size_t i = 0; i < _keybindings.data.size(); ++i) {
        if (_keybindings.data[i].key == keybinding->key &&
            _keybindings.data[i].action == keybinding->action)
        {
            _keybindings.data.erase(_keybindings.data.begin() + i);
            delete _keybindings.list->takeItem(static_cast<int>(i));
            clearKeybindingFields();
            return;
        }
    }
}

void ActionDialog::keybindingSelected() {
    const Profile::Keybinding* keybinding = selectedKeybinding();
    if (keybinding) {
        _keybindings.shiftModifier->setEnabled(true);
        _keybindings.shiftModifier->setChecked(
            hasKeyModifier(keybinding->key.modifier, KeyModifier::Shift)
        );
        _keybindings.ctrlModifier->setEnabled(true);
        _keybindings.ctrlModifier->setChecked(
            hasKeyModifier(keybinding->key.modifier, KeyModifier::Control)
        );
        _keybindings.altModifier->setEnabled(true);
        _keybindings.altModifier->setChecked(
            hasKeyModifier(keybinding->key.modifier, KeyModifier::Alt)
        );

        std::string key = ghoul::to_string(keybinding->key.key);
        _keybindings.key->setCurrentText(QString::fromStdString(key));
        _keybindings.key->setEnabled(true);
        _keybindings.action->setCurrentText(QString::fromStdString(keybinding->action));
        _keybindings.action->setEnabled(true);
        _keybindings.addButton->setEnabled(false);
        _keybindings.removeButton->setEnabled(true);
        _keybindings.saveButtons->setEnabled(true);
    }
    else {
        // No keybinding selected
        _keybindings.addButton->setEnabled(true);
        _keybindings.removeButton->setEnabled(false);
        _keybindings.saveButtons->setEnabled(false);
    }
}

void ActionDialog::keybindingSaved() {
    Profile::Keybinding* keybinding = selectedKeybinding();
    ghoul_assert(keybinding, "There must be a selected action at this point");

    KeyModifier km = KeyModifier::None;
    if (_keybindings.shiftModifier->isChecked()) {
        km |= KeyModifier::Shift;
    }
    if (_keybindings.altModifier->isChecked()) {
        km |= KeyModifier::Alt;
    }
    if (_keybindings.ctrlModifier->isChecked()) {
        km |= KeyModifier::Control;
    }

    keybinding->key = stringToKey(_keybindings.key->currentText().toStdString());
    keybinding->key.modifier = km;
    keybinding->action = _keybindings.action->currentText().toStdString();

    updateListItem(_keybindings.list->currentItem(), *keybinding);
    clearKeybindingFields();
}

void ActionDialog::clearKeybindingFields() {
    _keybindings.list->setCurrentRow(-1);
    _keybindings.shiftModifier->setChecked(false);
    _keybindings.shiftModifier->setEnabled(false);
    _keybindings.ctrlModifier->setChecked(false);
    _keybindings.ctrlModifier->setEnabled(false);
    _keybindings.altModifier->setChecked(false);
    _keybindings.altModifier->setEnabled(false);
    _keybindings.key->setCurrentIndex(-1);
    _keybindings.key->setEnabled(false);
    _keybindings.action->setCurrentIndex(-1);
    _keybindings.action->setEnabled(false);
}

void ActionDialog::keybindingRejected() {
    clearKeybindingFields();
}
