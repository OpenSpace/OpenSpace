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

ActionDialog::ActionDialog(QWidget* parent,
                           std::vector<openspace::Profile::Action>* actions,
                           std::vector<openspace::Profile::Keybinding>* keybindings)
    : QDialog(parent)
    , _actions(actions)
    , _keybindings(keybindings)
    , _actionData(*_actions)
    , _keybindingsData(*_keybindings)
{
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
    //  |                      | Add actions   | DDDDDDDDDDDD>  |    Row 10
    //  |                      | Action        | [oooooooooooo] |    Row 11
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
    _actionWidgets.list = new QListWidget;
    connect(
        _actionWidgets.list, &QListWidget::itemSelectionChanged,
        this, &ActionDialog::actionSelected
    );
    _actionWidgets.list->setAlternatingRowColors(true);
    _actionWidgets.list->setResizeMode(QListView::Adjust);

    for (size_t i = 0; i < _actionData.size(); ++i) {
        const Profile::Action& action = _actionData[i];
        std::string name = action.name.empty() ? action.identifier : action.name;
        _actionWidgets.list->addItem(new QListWidgetItem(QString::fromStdString(name)));
    }

    layout->addWidget(_actionWidgets.list, 0, 0, 6, 1);

    layout->addWidget(new QLabel("Identifier"), 0, 1);
    _actionWidgets.identifier = new QLineEdit;
    _actionWidgets.identifier->setEnabled(false);
    layout->addWidget(_actionWidgets.identifier, 0, 2);

    layout->addWidget(new QLabel("Name"), 1, 1);
    _actionWidgets.name = new QLineEdit;
    _actionWidgets.name->setEnabled(false);
    layout->addWidget(_actionWidgets.name, 1, 2);

    layout->addWidget(new QLabel("GUI Path"), 2, 1);
    _actionWidgets.guiPath = new QLineEdit;
    _actionWidgets.guiPath->setEnabled(false);
    layout->addWidget(_actionWidgets.guiPath, 2, 2);

    layout->addWidget(new QLabel("Documentation"), 3, 1);
    _actionWidgets.documentation = new QLineEdit;
    _actionWidgets.documentation->setEnabled(false);
    layout->addWidget(_actionWidgets.documentation, 3, 2);

    layout->addWidget(new QLabel("Is Local"), 4, 1);
    _actionWidgets.isLocal = new QCheckBox;
    _actionWidgets.isLocal->setEnabled(false);
    layout->addWidget(_actionWidgets.isLocal, 4, 2);

    layout->addWidget(new QLabel("Script"), 5, 1);
    _actionWidgets.script = new QTextEdit;
    _actionWidgets.script->setEnabled(false);
    layout->addWidget(_actionWidgets.script, 5, 2);


    // + / - buttons
    QWidget* container = new QWidget;
    QBoxLayout* containerLayout = new QHBoxLayout(container);
    _actionWidgets.addButton = new QPushButton("+");
    _actionWidgets.addButton->setObjectName("add-button");
    QObject::connect(
        _actionWidgets.addButton, &QPushButton::clicked,
        this, &ActionDialog::actionAdd
    );
    containerLayout->addWidget(_actionWidgets.addButton);

    _actionWidgets.removeButton = new QPushButton("-");
    _actionWidgets.removeButton->setObjectName("remove-button");
    _actionWidgets.removeButton->setEnabled(false);
    QObject::connect(
        _actionWidgets.removeButton, &QPushButton::clicked,
        this, &ActionDialog::actionRemove
    );
    containerLayout->addWidget(_actionWidgets.removeButton);
    layout->addWidget(container, 6, 0, Qt::AlignLeft);


    // Save / Cancel buttons
    _actionWidgets.saveButtons = new QDialogButtonBox;
    _actionWidgets.saveButtons->setEnabled(false);
    _actionWidgets.saveButtons->setStandardButtons(
        QDialogButtonBox::Save | QDialogButtonBox::Cancel
    );
    QObject::connect(
        _actionWidgets.saveButtons, &QDialogButtonBox::accepted,
        this, &ActionDialog::actionSaved
    );
    QObject::connect(
        _actionWidgets.saveButtons, &QDialogButtonBox::rejected,
        this, &ActionDialog::actionRejected
    );
    layout->addWidget(_actionWidgets.saveButtons, 6, 2, Qt::AlignRight);
}

void ActionDialog::createKeyboardWidgets(QGridLayout* layout) {
    _keybindingWidgets.list = new QListWidget;
    connect(
        _keybindingWidgets.list, &QListWidget::itemSelectionChanged,
        this, &ActionDialog::keybindingSelected
    );
    _keybindingWidgets.list->setAlternatingRowColors(true);
    _keybindingWidgets.list->setResizeMode(QListView::Adjust);

    for (size_t i = 0; i < _keybindingsData.size(); ++i) {
        const Profile::Keybinding& kv = _keybindingsData[i];
        QListWidgetItem* item = new QListWidgetItem;
        updateListItem(item, kv);
        _keybindingWidgets.list->addItem(item);
    }

    layout->addWidget(_keybindingWidgets.list, 8, 0, 5, 1);

    layout->addWidget(new QLabel("Modifier"), 8, 1);
    {
        QWidget* container = new QWidget;
        QBoxLayout* containerLayout = new QHBoxLayout(container);
        _keybindingWidgets.shiftModifier = new QCheckBox("Shift");
        _keybindingWidgets.shiftModifier->setEnabled(false);
        containerLayout->addWidget(_keybindingWidgets.shiftModifier);
        _keybindingWidgets.ctrlModifier = new QCheckBox("Control");
        _keybindingWidgets.ctrlModifier->setEnabled(false);
        containerLayout->addWidget(_keybindingWidgets.ctrlModifier);
        _keybindingWidgets.altModifier = new QCheckBox("Alt");
        _keybindingWidgets.altModifier->setEnabled(false);
        containerLayout->addWidget(_keybindingWidgets.altModifier);
        layout->addWidget(container, 8, 2);
    }

    layout->addWidget(new QLabel("Key"), 9, 1);
    _keybindingWidgets.key = new QComboBox;
    QStringList keyList;
    for (const KeyInfo& ki : KeyInfos) {
        keyList += QString::fromStdString(std::string(ki.name));
    }
    _keybindingWidgets.key->addItems(keyList);
    _keybindingWidgets.key->setCurrentIndex(-1);
    _keybindingWidgets.key->setEnabled(false);
    layout->addWidget(_keybindingWidgets.key, 9, 2);

    layout->addWidget(new QLabel("Action chooser"), 10, 1);
    _keybindingWidgets.action = new QComboBox;
    for (const Profile::Action& action : _actionData) {
        _keybindingWidgets.action->addItem(QString::fromStdString(action.identifier));
    }
    connect(
        _keybindingWidgets.action, QOverload<int>::of(&QComboBox::currentIndexChanged),
        this, &ActionDialog::keybindingActionSelected
    );

    _keybindingWidgets.action->setEnabled(false);
    layout->addWidget(_keybindingWidgets.action, 10, 2);

    layout->addWidget(new QLabel("Action"), 11, 1);
    _keybindingWidgets.actionText = new QLineEdit;
    _keybindingWidgets.actionText->setEnabled(false);
    layout->addWidget(_keybindingWidgets.actionText, 11, 2);


    // +/- buttons
    QWidget* container = new QWidget;
    QBoxLayout* containerLayout = new QHBoxLayout(container);
    _keybindingWidgets.addButton = new QPushButton("+");
    _keybindingWidgets.addButton->setObjectName("add-button");
    QObject::connect(
        _keybindingWidgets.addButton, &QPushButton::clicked,
        this, &ActionDialog::keybindingAdd
    );
    containerLayout->addWidget(_keybindingWidgets.addButton);

    _keybindingWidgets.removeButton = new QPushButton("-");
    _keybindingWidgets.removeButton->setObjectName("remove-button");
    _keybindingWidgets.removeButton->setEnabled(false);
    QObject::connect(
        _keybindingWidgets.removeButton, &QPushButton::clicked,
        this, &ActionDialog::keybindingRemove
    );
    containerLayout->addWidget(_keybindingWidgets.removeButton);
    layout->addWidget(container, 13, 0, Qt::AlignLeft);

    // Save/Cancel
    _keybindingWidgets.saveButtons = new QDialogButtonBox;
    _keybindingWidgets.saveButtons->setEnabled(false);
    _keybindingWidgets.saveButtons->setStandardButtons(
        QDialogButtonBox::Save | QDialogButtonBox::Cancel
    );
    QObject::connect(
        _keybindingWidgets.saveButtons, &QDialogButtonBox::accepted,
        this, &ActionDialog::keybindingSaved
    );
    QObject::connect(
        _keybindingWidgets.saveButtons, &QDialogButtonBox::rejected,
        this, &ActionDialog::keybindingRejected
    );

    layout->addWidget(_keybindingWidgets.saveButtons, 13, 2, Qt::AlignRight);
}

void ActionDialog::applyChanges() {
    *_actions = std::move(_actionData);
    *_keybindings = std::move(_keybindingsData);
    accept();
}

Profile::Action* ActionDialog::selectedAction() {
    QListWidgetItem* item = _actionWidgets.list->currentItem();
    const int idx = _actionWidgets.list->row(item);
    return idx != -1 ? &_actionData[idx] : nullptr;
}

void ActionDialog::actionAdd() {
    _actionWidgets.list->addItem("");
    _actionData.push_back(Profile::Action());
    _actionWidgets.list->setCurrentRow(_actionWidgets.list->count() - 1);
}

void ActionDialog::actionRemove() {
    const openspace::Profile::Action* action = selectedAction();
    ghoul_assert(action, "An action must exist at this point");

    ghoul_assert(
        _actionWidgets.list->count() == static_cast<int>(_actionData.size()),
        "Action list and data has desynced"
    );

    // We can't remove an action if it has a keyboard shortcut attached to it
    for (size_t i = 0; i < _keybindingsData.size(); ++i) {
        const Profile::Keybinding& kb = _keybindingsData[i];
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
            _keybindingsData.erase(_keybindingsData.begin() + i);
            delete _keybindingWidgets.list->takeItem(static_cast<int>(i));
            i--;
        }
        else {
            // If the user chooses 'No' at least once, we have to bail
            return;
        }
    }

    for (size_t i = 0; i < _actionData.size(); ++i) {
        if (_actionData[i].identifier == action->identifier) {
            _actionData.erase(_actionData.begin() + i);
            delete _actionWidgets.list->takeItem(static_cast<int>(i));
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
        _actionWidgets.identifier->setText(QString::fromStdString(action->identifier));
        _actionWidgets.identifier->setEnabled(true);
        _actionWidgets.name->setText(QString::fromStdString(action->name));
        _actionWidgets.name->setEnabled(true);
        _actionWidgets.guiPath->setText(QString::fromStdString(action->guiPath));
        _actionWidgets.guiPath->setEnabled(true);
        _actionWidgets.documentation->setText(
            QString::fromStdString(action->documentation)
        );
        _actionWidgets.documentation->setEnabled(true);
        _actionWidgets.isLocal->setChecked(action->isLocal);
        _actionWidgets.isLocal->setEnabled(true);
        _actionWidgets.script->setText(QString::fromStdString(action->script));
        _actionWidgets.script->setEnabled(true);
        _actionWidgets.addButton->setEnabled(false);
        _actionWidgets.removeButton->setEnabled(true);
        _actionWidgets.saveButtons->setEnabled(true);
    }
    else {
        // No action selected
        _actionWidgets.addButton->setEnabled(true);
        _actionWidgets.removeButton->setEnabled(false);
        _actionWidgets.saveButtons->setEnabled(false);
    }
}

void ActionDialog::actionSaved() {
    std::string newIdentifier = _actionWidgets.identifier->text().toStdString();
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
            _actionData.begin(), _actionData.end(),
            [id = newIdentifier](const Profile::Action& action) {
                return action.identifier == id;
            }
        );
        if (it != _actionData.end()) {
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
            _keybindingWidgets.list->count() == _keybindingsData.size(),
            "The list and data got out of sync"
        );
        for (int i = 0; i < _keybindingWidgets.list->count(); ++i) {
            if (_keybindingsData[i].action == oldIdentifier) {
                _keybindingsData[i].action = newIdentifier;
                updateListItem(_keybindingWidgets.list->item(i), _keybindingsData[i]);
            }
        }
        for (int i = 0; i < _keybindingWidgets.action->count(); ++i) {
            if (_keybindingWidgets.action->itemText(i).toStdString() == oldIdentifier) {
                _keybindingWidgets.action->setItemText(
                    i,
                    QString::fromStdString(newIdentifier)
                );
            }
        }
        action->identifier = newIdentifier;
    }
    

    action->name = _actionWidgets.name->text().toStdString();
    action->guiPath = _actionWidgets.guiPath->text().toStdString();
    action->documentation = _actionWidgets.documentation->text().toStdString();
    action->isLocal = _actionWidgets.isLocal->isChecked();
    action->script = _actionWidgets.script->toPlainText().toStdString();

    updateListItem(_actionWidgets.list->currentItem(), *action);
    clearActionFields();
}

void ActionDialog::clearActionFields() {
    _actionWidgets.list->setCurrentRow(-1);
    _actionWidgets.identifier->clear();
    _actionWidgets.identifier->setEnabled(false);
    _actionWidgets.name->clear();
    _actionWidgets.name->setEnabled(false);
    _actionWidgets.guiPath->clear();
    _actionWidgets.guiPath->setEnabled(false);
    _actionWidgets.documentation->clear();
    _actionWidgets.documentation->setEnabled(false);
    _actionWidgets.isLocal->setChecked(false);
    _actionWidgets.isLocal->setEnabled(false);
    _actionWidgets.script->clear();
    _actionWidgets.script->setEnabled(false);
    _actionWidgets.saveButtons->setEnabled(false);
}

void ActionDialog::actionRejected() {
    if (_actionData.back().identifier.empty()) {
        // This happens if someone creates a new action and never gave an identifier
        delete _actionWidgets.list->takeItem(_actionWidgets.list->count() - 1);
        _actionData.erase(_actionData.begin() + _actionData.size() - 1);
    }

    clearActionFields();
}

Profile::Keybinding* ActionDialog::selectedKeybinding() {
    QListWidgetItem* item = _keybindingWidgets.list->currentItem();
    const int idx = _keybindingWidgets.list->row(item);
    return idx != -1 ? &_keybindingsData[idx] : nullptr;
}

void ActionDialog::keybindingAdd() {
    _keybindingWidgets.list->addItem("");
    _keybindingsData.push_back(Profile::Keybinding());
    _keybindingWidgets.list->setCurrentRow(_keybindingWidgets.list->count() - 1);
}

void ActionDialog::keybindingRemove() {
    const Profile::Keybinding* keybinding = selectedKeybinding();
    ghoul_assert(keybinding, "A keybinding must be selected at this point");
    
    for (size_t i = 0; i < _keybindingsData.size(); ++i) {
        if (_keybindingsData[i].key == keybinding->key &&
            _keybindingsData[i].action == keybinding->action)
        {
            _keybindingsData.erase(_keybindingsData.begin() + i);
            delete _keybindingWidgets.list->takeItem(static_cast<int>(i));
            clearKeybindingFields();
            return;
        }
    }
}

void ActionDialog::keybindingSelected() {
    const Profile::Keybinding* keybinding = selectedKeybinding();
    if (keybinding) {
        _keybindingWidgets.shiftModifier->setEnabled(true);
        _keybindingWidgets.shiftModifier->setChecked(
            hasKeyModifier(keybinding->key.modifier, KeyModifier::Shift)
        );
        _keybindingWidgets.ctrlModifier->setEnabled(true);
        _keybindingWidgets.ctrlModifier->setChecked(
            hasKeyModifier(keybinding->key.modifier, KeyModifier::Control)
        );
        _keybindingWidgets.altModifier->setEnabled(true);
        _keybindingWidgets.altModifier->setChecked(
            hasKeyModifier(keybinding->key.modifier, KeyModifier::Alt)
        );

        std::string key = ghoul::to_string(keybinding->key.key);
        _keybindingWidgets.key->setCurrentText(QString::fromStdString(key));
        _keybindingWidgets.key->setEnabled(true);
        _keybindingWidgets.action->setCurrentText(QString::fromStdString(keybinding->action));
        _keybindingWidgets.action->setEnabled(true);
        _keybindingWidgets.actionText->setText(
            QString::fromStdString(keybinding->action)
        );
        _keybindingWidgets.actionText->setEnabled(true);
        _keybindingWidgets.addButton->setEnabled(false);
        _keybindingWidgets.removeButton->setEnabled(true);
        _keybindingWidgets.saveButtons->setEnabled(true);
    }
    else {
        // No keybinding selected
        _keybindingWidgets.addButton->setEnabled(true);
        _keybindingWidgets.removeButton->setEnabled(false);
        _keybindingWidgets.saveButtons->setEnabled(false);
    }
}

void ActionDialog::keybindingActionSelected(int) {
    _keybindingWidgets.actionText->setText(_keybindingWidgets.action->currentText());
}

void ActionDialog::keybindingSaved() {
    Profile::Keybinding* keybinding = selectedKeybinding();
    ghoul_assert(keybinding, "There must be a selected action at this point");

    KeyModifier km = KeyModifier::None;
    if (_keybindingWidgets.shiftModifier->isChecked()) {
        km |= KeyModifier::Shift;
    }
    if (_keybindingWidgets.altModifier->isChecked()) {
        km |= KeyModifier::Alt;
    }
    if (_keybindingWidgets.ctrlModifier->isChecked()) {
        km |= KeyModifier::Control;
    }

    keybinding->key = stringToKey(_keybindingWidgets.key->currentText().toStdString());
    keybinding->key.modifier = km;
    keybinding->action = _keybindingWidgets.actionText->text().toStdString();

    updateListItem(_keybindingWidgets.list->currentItem(), *keybinding);
    clearKeybindingFields();
}

void ActionDialog::clearKeybindingFields() {
    _keybindingWidgets.list->setCurrentRow(-1);
    _keybindingWidgets.shiftModifier->setChecked(false);
    _keybindingWidgets.shiftModifier->setEnabled(false);
    _keybindingWidgets.ctrlModifier->setChecked(false);
    _keybindingWidgets.ctrlModifier->setEnabled(false);
    _keybindingWidgets.altModifier->setChecked(false);
    _keybindingWidgets.altModifier->setEnabled(false);
    _keybindingWidgets.key->setCurrentIndex(-1);
    _keybindingWidgets.key->setEnabled(false);
    _keybindingWidgets.action->setCurrentIndex(-1);
    _keybindingWidgets.action->setEnabled(false);
    _keybindingWidgets.actionText->clear();
    _keybindingWidgets.actionText->setEnabled(false);
}

void ActionDialog::keybindingRejected() {
    clearKeybindingFields();
}
