/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include "profile/scriptlogdialog.h"
#include <openspace/util/keys.h>
#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
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
        const std::string n = std::format("{}\t{}", ghoul::to_string(kb.key), kb.action);
        item->setText(QString::fromStdString(n));
    }
} // namespace

ActionDialog::ActionDialog(QWidget* parent,
                           std::vector<openspace::Profile::Action>* actions,
                           std::vector<openspace::Profile::Keybinding>* keybindings)
    : QDialog(parent)
    , _actions(actions)
    , _actionData(*_actions)
    , _keybindings(keybindings)
    , _keybindingsData(*_keybindings)
{
    setWindowTitle("Actions and Keybindings");
    createWidgets();
}

void ActionDialog::createWidgets() {
    //         Column 0            Column 1      Column 2   Col3
    //  *----------------------*---------------*----------|------*
    //  | Actions                                                |    Row 0
    //  |                      | Identifier    | [oooooo] | Info |    Row 1
    //  |                      | Name          | [ooooooooooooo] |    Row 2
    //  |                      | GUI Path      | [ooooooooooooo] |    Row 3
    //  |                      | Documentation | [ooooooooooooo] |    Row 4
    //  |                      | Is Local      | []  [choosescr] |    Row 5
    //  |                      | Script        | [ooooooooooooo] |    Row 6
    //  *----------------------*---------------*-----------------*
    //  | [+] [-]              |               | <Save> <Cancel> |    Row 7
    //  *----------------------*---------------*-----------------*
    //  |========================================================|    Row 8
    //  | Keybindings                                            |    Row 9
    //  *----------------------*---------------*-----------------|
    //  |                      | Modifier      | []S []C []A     |    Row 10
    //  |                      | Key           | DDDDDDDDDDDD>   |    Row 11
    //  |                      | Add actions   | DDDDDDDDDDDD>   |    Row 12
    //  |                      | Action        | [ooooooooooooo] |    Row 13
    //  *----------------------*---------------*-----------------*
    //  | [+] [-]              |               | <Save> <Cancel> |    Row 14
    //  *----------------------*---------------*-----------------*
    //  |========================================================|    Row 16
    //  *----------------------*---------------*-----------------*
    //  |                                      | <Save> <Cancel> |    Row 17
    //  *----------------------*---------------*-----------------*

    QGridLayout* layout = new QGridLayout(this);
    
    createActionWidgets(layout);
    clearActionFields();

    layout->addWidget(new Line, 8, 0, 1, 4);

    createKeyboardWidgets(layout);
    clearKeybindingFields();

    layout->addWidget(new Line, 16, 0, 1, 4);

    _mainButton = new QDialogButtonBox;
    _mainButton->setStandardButtons(QDialogButtonBox::Close);
    QObject::connect(
        _mainButton, &QDialogButtonBox::rejected,
        this, &ActionDialog::reject
    );
    layout->addWidget(_mainButton, 17, 2, Qt::AlignRight);
}

void ActionDialog::createActionWidgets(QGridLayout* layout) {
    QLabel* title = new QLabel("Actions");
    title->setObjectName("heading");
    layout->addWidget(title, 0, 0, 1, 4);

    _actionWidgets.list = new QListWidget;
    _actionWidgets.list->setToolTip(
        "The list of all actions currently defined in the profile. Select one to edit it "
        "or use the + button below to create a new action"
    );
    _actionWidgets.list->setAlternatingRowColors(true);
    _actionWidgets.list->setResizeMode(QListView::Adjust);
    connect(
        _actionWidgets.list, &QListWidget::itemSelectionChanged,
        this, &ActionDialog::actionSelected
    );

    for (const Profile::Action& action : _actionData) {
        const std::string name = action.name.empty() ? action.identifier : action.name;
        _actionWidgets.list->addItem(new QListWidgetItem(QString::fromStdString(name)));
    }

    layout->addWidget(_actionWidgets.list, 1, 0, 6, 1);

    layout->addWidget(new QLabel("Identifier"), 1, 1);
    _actionWidgets.identifier = new QLineEdit;
    _actionWidgets.identifier->setToolTip(
        "The unique identifier for this action. The identifier name cannot be reused "
        "between different actions and will lead to a failure to load the profile if it "
        "happens. There are no restrictions on the name of the identifier, but a dot "
        "separated hierarchical structure is suggested to prevent name clashes"
    );
    _actionWidgets.identifier->setEnabled(false);
    connect(
        _actionWidgets.identifier, &QLineEdit::textEdited,
        [this]() {
            // Check if the identifier is legal
            const std::string id = _actionWidgets.identifier->text().toStdString();
            const bool isLegal = id.find_first_of("\t\n. ") == std::string::npos;
            if (isLegal) {
                _actionWidgets.infoText->clear();
                _actionWidgets.infoText->setHidden(true);
            }
            else {
                _actionWidgets.infoText->setText(
                    "Identifier must not contain whitespace or ."
                );
                _actionWidgets.infoText->setHidden(false);
            }
        }
    );
    layout->addWidget(_actionWidgets.identifier, 1, 2);

    _actionWidgets.infoText = new QLabel;
    _actionWidgets.infoText->setHidden(true);
    _actionWidgets.infoText->setObjectName("error-message");
    layout->addWidget(_actionWidgets.infoText, 1, 3);

    layout->addWidget(new QLabel("Name"), 2, 1);
    _actionWidgets.name = new QLineEdit;
    _actionWidgets.name->setToolTip(
        "The user-facing name of this action. As it is displayed in user interfaces, the "
        "name should be as concise and informative as possible"
    );
    _actionWidgets.name->setEnabled(false);
    layout->addWidget(_actionWidgets.name, 2, 2, 1, 2);

    layout->addWidget(new QLabel("GUI Path"), 3, 1);
    _actionWidgets.guiPath = new QLineEdit;
    _actionWidgets.guiPath->setToolTip(
        "The path under which this action will be shown in user interfaces. The path "
        "must use the '/' character as separators between folders and start with a '/' "
        "character that denotes the root folder"
    );
    _actionWidgets.guiPath->setEnabled(false);
    layout->addWidget(_actionWidgets.guiPath, 3, 2, 1, 2);

    layout->addWidget(new QLabel("Documentation"), 4, 1);
    _actionWidgets.documentation = new QLineEdit;
    _actionWidgets.documentation->setToolTip(
        "A longer user-facing documentation that describes the action in more detail. "
        "The user can request the documentation on demand, so it might be longer and "
        "more descriptive than the name itself and might also explain some optional "
        "parameters that that action can consume"
    );
    _actionWidgets.documentation->setEnabled(false);
    layout->addWidget(_actionWidgets.documentation, 4, 2, 1, 2);

    layout->addWidget(new QLabel("Is Local"), 5, 1);
    _actionWidgets.isLocal = new QCheckBox;
    _actionWidgets.isLocal->setToolTip(
        "If this value is checked, the action will only ever affect the OpenSpace "
        "instance that is executing it. If running a 'regular' OpenSpace instance, this "
        "setting will not make any difference, but it is necessary in a clustered "
        "environment or when using a parallel connection, in which case it determines "
        "whether a command should be executed only locally or send to all remote "
        "instances as well"
    );
    _actionWidgets.isLocal->setEnabled(false);
    layout->addWidget(_actionWidgets.isLocal, 5, 2, 1, 2);

    _actionWidgets.chooseScripts = new QPushButton("Choose Scripts");
    _actionWidgets.chooseScripts->setToolTip(
        "Press this button to choose scripts for your action from the logs/scriptlog.txt"
    );
    connect(
        _actionWidgets.chooseScripts, &QPushButton::clicked,
        this, &ActionDialog::chooseScripts
    );
    _actionWidgets.chooseScripts->setEnabled(false);
    layout->addWidget(_actionWidgets.chooseScripts, 5, 2, 1, 2, Qt::AlignRight);

    layout->addWidget(new QLabel("Script"), 6, 1);
    _actionWidgets.script = new QTextEdit;
    _actionWidgets.script->setToolTip(
        "This is the Lua script that gets executed when this action is triggered. "
        "Actions can make use of optional arguments which are already defined as the "
        "`args` variable when this script executes. If no arguments are passed, this "
        "variable does not exist"
    );
    _actionWidgets.script->setEnabled(false);
    layout->addWidget(_actionWidgets.script, 6, 2, 1, 2);


    // + / - buttons
    QWidget* container = new QWidget;
    QBoxLayout* containerLayout = new QHBoxLayout(container);
    _actionWidgets.addButton = new QPushButton("+");
    _actionWidgets.addButton->setObjectName("add-button");
    _actionWidgets.addButton->setToolTip("Adds a new action to the list of all actions");
    QObject::connect(
        _actionWidgets.addButton, &QPushButton::clicked,
        this, &ActionDialog::actionAdd
    );
    containerLayout->addWidget(_actionWidgets.addButton);

    _actionWidgets.removeButton = new QPushButton("-");
    _actionWidgets.removeButton->setObjectName("remove-button");
    _actionWidgets.removeButton->setToolTip("Removes the currently selected action");
    _actionWidgets.removeButton->setEnabled(false);
    QObject::connect(
        _actionWidgets.removeButton, &QPushButton::clicked,
        this, &ActionDialog::actionRemove
    );
    containerLayout->addWidget(_actionWidgets.removeButton);
    layout->addWidget(container, 7, 0, Qt::AlignLeft);


    // Save / Cancel buttons
    _actionWidgets.saveButtons = new QDialogButtonBox;
    _actionWidgets.saveButtons->setToolTip(
        "Saves or discards all changes to the currently selected action"
    );
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
    layout->addWidget(_actionWidgets.saveButtons, 7, 2, 1, 2, Qt::AlignRight);
}

void ActionDialog::createKeyboardWidgets(QGridLayout* layout) {
    QLabel* title = new QLabel("Keybindings");
    title->setObjectName("heading");
    layout->addWidget(title, 9, 0, 1, 4);

    _keybindingWidgets.list = new QListWidget;
    _keybindingWidgets.list->setToolTip(
        "The list of all keybindings currently assigned in this profile"
    );
    _keybindingWidgets.list->setAlternatingRowColors(true);
    _keybindingWidgets.list->setResizeMode(QListView::Adjust);
    connect(
        _keybindingWidgets.list, &QListWidget::itemSelectionChanged,
        this, &ActionDialog::keybindingSelected
    );

    for (const Profile::Keybinding& keybinding : _keybindingsData) {
        QListWidgetItem* item = new QListWidgetItem;
        updateListItem(item, keybinding);
        _keybindingWidgets.list->addItem(item);
    }

    layout->addWidget(_keybindingWidgets.list, 10, 0, 4, 1);

    layout->addWidget(new QLabel("Modifier"), 10, 1);
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
        layout->addWidget(container, 10, 2, 1, 2);
    }

    layout->addWidget(new QLabel("Key"), 11, 1);
    _keybindingWidgets.key = new QComboBox;
    QStringList keyList;
    for (const KeyInfo& ki : KeyInfos) {
        // We don't want to use the Shift, Alt, and Ctrl keys directly since we already
        // have them as modifier keys
        if (ki.key == Key::LeftShift   || ki.key == Key::RightShift   ||
            ki.key == Key::LeftAlt     || ki.key == Key::RightAlt     ||
            ki.key == Key::LeftControl || ki.key == Key::RightControl ||
            ki.key == Key::LeftSuper   || ki.key == Key::RightSuper   ||
            ki.key == Key::Menu        || ki.key == Key::NumLock      ||
            ki.key == Key::World1      || ki.key == Key::World2)
        {
            continue;
        }
        keyList += QString::fromStdString(std::string(ki.name));
    }
    _keybindingWidgets.key->addItems(keyList);
    _keybindingWidgets.key->setCurrentIndex(-1);
    _keybindingWidgets.key->setEnabled(false);
    connect(
        _keybindingWidgets.key, QOverload<int>::of(&QComboBox::currentIndexChanged),
        [this](int newIndex) {
            _keybindingWidgets.saveButtons->button(QDialogButtonBox::Save)->setEnabled(
                newIndex > 0
            );
        }
    );
    layout->addWidget(_keybindingWidgets.key, 11, 2, 1, 2);

    layout->addWidget(new QLabel("Action chooser"), 12, 1);
    _keybindingWidgets.action = new QComboBox;
    _keybindingWidgets.action->setToolTip(
        "You can select any of the actions defined above here to be associated with the "
        "selected keybind. Selecting an action from this dropdown menu will "
        "automatically enter it into the text field below and overwrite any value that "
        "already is entered in there"
    );
    for (const Profile::Action& action : _actionData) {
        _keybindingWidgets.action->addItem(QString::fromStdString(action.identifier));
    }
    connect(
        _keybindingWidgets.action, QOverload<int>::of(&QComboBox::currentIndexChanged),
        this, &ActionDialog::keybindingActionSelected
    );

    _keybindingWidgets.action->setEnabled(false);
    layout->addWidget(_keybindingWidgets.action, 12, 2, 1, 2);

    layout->addWidget(new QLabel("Action"), 13, 1);
    _keybindingWidgets.actionText = new QLineEdit;
    _keybindingWidgets.actionText->setToolTip(
        "This is the action that will be triggered when the keybind is pressed. In the "
        "majority of cases, you do not need to enter something here manually, but "
        "instead select the action from the dropdown list above. However, if you know "
        "that an action with a specific identifier will exist at runtime, for example if "
        "it is defined in an asset included in this profile, you can enter the "
        "identifier of that action manually here to associate a key with it. If the "
        "identifer does not exist, an error will be logged when trying to bind the key "
        "at startup"
    );
    _keybindingWidgets.actionText->setEnabled(false);
    layout->addWidget(_keybindingWidgets.actionText, 13, 2, 1, 2);


    // +/- buttons
    QWidget* container = new QWidget;
    QBoxLayout* containerLayout = new QHBoxLayout(container);
    _keybindingWidgets.addButton = new QPushButton("+");
    _keybindingWidgets.addButton->setObjectName("add-button");
    _keybindingWidgets.addButton->setToolTip(
        "Adds a new keybinding to the list of all keybindings"
    );
    QObject::connect(
        _keybindingWidgets.addButton, &QPushButton::clicked,
        this, &ActionDialog::keybindingAdd
    );
    containerLayout->addWidget(_keybindingWidgets.addButton);

    _keybindingWidgets.removeButton = new QPushButton("-");
    _keybindingWidgets.removeButton->setObjectName("remove-button");
    _keybindingWidgets.removeButton->setToolTip(
        "Removes the currently selected keybinding"
    );
    _keybindingWidgets.removeButton->setEnabled(false);
    QObject::connect(
        _keybindingWidgets.removeButton, &QPushButton::clicked,
        this, &ActionDialog::keybindingRemove
    );
    containerLayout->addWidget(_keybindingWidgets.removeButton);
    layout->addWidget(container, 14, 0, Qt::AlignLeft);

    // Save/Cancel
    _keybindingWidgets.saveButtons = new QDialogButtonBox;
    _keybindingWidgets.saveButtons->setToolTip(
        "Saves or discards all changes to the currently selected keybinding"
    );
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

    layout->addWidget(_keybindingWidgets.saveButtons, 14, 2, 1, 2, Qt::AlignRight);
}

Profile::Action* ActionDialog::selectedAction() {
    QListWidgetItem* item = _actionWidgets.list->currentItem();
    const int idx = _actionWidgets.list->row(item);
    return idx != -1 ? &_actionData[idx] : nullptr;
}

void ActionDialog::actionAdd() {
    _actionWidgets.list->addItem("");
    _actionData.emplace_back();
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
    for (size_t i = 0; i < _keybindingsData.size(); i++) {
        const Profile::Keybinding& kb = _keybindingsData[i];
        if (kb.action != action->identifier) {
            continue;
        }
        const QMessageBox::StandardButton button = QMessageBox::information(
            this,
            "Remove action",
            QString::fromStdString(std::format(
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
            //Save the updated keybindings to the profile
            if (_keybindings) {
                *_keybindings = _keybindingsData;
            }
        }
        else {
            // If the user chooses 'No' at least once, we have to bail
            return;
        }
    }

    for (size_t i = 0; i < _actionData.size(); i++) {
        if (_actionData[i].identifier == action->identifier) {
            clearActionFields();
            _actionData.erase(_actionData.begin() + i);
            delete _actionWidgets.list->takeItem(static_cast<int>(i));

            _keybindingWidgets.action->clear();
            for (const Profile::Action& a : _actionData) {
                _keybindingWidgets.action->addItem(QString::fromStdString(a.identifier));
            }
            clearKeybindingFields();
            //Save the updated actions to the profile
            if (_actions) {
                *_actions = _actionData;
            }
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
        _actionWidgets.chooseScripts->setEnabled(true);
        _actionWidgets.script->setText(QString::fromStdString(action->script));
        _actionWidgets.script->setEnabled(true);
        _actionWidgets.addButton->setEnabled(false);
        _actionWidgets.removeButton->setEnabled(true);
        _actionWidgets.saveButtons->setEnabled(true);
        if (_mainButton) {
            _mainButton->setEnabled(false);
        }
        _actionWidgets.list->setEnabled(false);
    }
    else {
        // No action selected
        _actionWidgets.addButton->setEnabled(true);
        _actionWidgets.removeButton->setEnabled(false);
        _actionWidgets.saveButtons->setEnabled(false);
        //Keybinding panel must also be in valid state to re-enable main start button
        if (_mainButton && !_keybindingWidgets.saveButtons->isEnabled()) {
            _mainButton->setEnabled(true);
        }
        _actionWidgets.list->setEnabled(true);
    }
}

void ActionDialog::actionSaved() {
    const std::string newIdentifier = _actionWidgets.identifier->text().toStdString();
    if (newIdentifier.empty()) {
        QMessageBox::critical(this, "Empty identifier", "Identifier must not be empty");
        return;
    }

    Profile::Action* action = selectedAction();
    const std::string oldIdentifier = action->identifier;
    if (oldIdentifier != newIdentifier) {
        // The identifier is a bit special as we need to make sure that we didn't
        // accidentally create a duplicate while renaming the currently selected action.
        // Also if we didn't create a duplicate, meaning that we renamed an action to a
        // new valid identifier, we need to make sure that we update all keybinds that
        // referenced the old value are updated to use the new name instead

        const auto it = std::find_if(
            _actionData.begin(), _actionData.end(),
            [id = newIdentifier](const Profile::Action& a) { return a.identifier == id; }
        );
        if (it != _actionData.end()) {
            QMessageBox::critical(
                this,
                "Duplicate identifier",
                "The chosen identifier was already used in another action. Identifiers "
                "have to be unique. Please choose a different identfier"
            );
            return;
        }

        // If we got this far, we have a new identifier and it is a new one, so we need to
        // update other keybinds now
        ghoul_assert(
            _keybindingWidgets.list->count() == static_cast<int>(_keybindingsData.size()),
            "The list and data got out of sync"
        );
        for (int i = 0; i < _keybindingWidgets.list->count(); i++) {
            if (_keybindingsData[i].action == oldIdentifier) {
                _keybindingsData[i].action = newIdentifier;
                updateListItem(_keybindingWidgets.list->item(i), _keybindingsData[i]);
            }
        }
        for (int i = 0; i < _keybindingWidgets.action->count(); i++) {
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
    std::string guiPath = _actionWidgets.guiPath->text().toStdString();
    if (!guiPath.starts_with('/')) {
        guiPath = "/" + guiPath;
    }
    action->guiPath = guiPath;
    action->documentation = _actionWidgets.documentation->text().toStdString();
    action->isLocal = _actionWidgets.isLocal->isChecked();
    action->script = _actionWidgets.script->toPlainText().toStdString();

    updateListItem(_actionWidgets.list->currentItem(), *action);

    // Update the list of actions available in the action chooser
    _keybindingWidgets.action->clear();
    for (const Profile::Action& a : _actionData) {
        _keybindingWidgets.action->addItem(QString::fromStdString(a.identifier));
    }
    //Save the updated actions to the profile
    if (_actions) {
        *_actions = _actionData;
    }
    clearKeybindingFields();
    clearActionFields();
}

void ActionDialog::clearActionFields() const {
    _actionWidgets.list->setCurrentRow(-1);
    _actionWidgets.identifier->clear();
    _actionWidgets.identifier->setEnabled(false);
    _actionWidgets.infoText->clear();
    _actionWidgets.infoText->setHidden(true);
    _actionWidgets.name->clear();
    _actionWidgets.name->setEnabled(false);
    _actionWidgets.guiPath->clear();
    _actionWidgets.guiPath->setEnabled(false);
    _actionWidgets.documentation->clear();
    _actionWidgets.documentation->setEnabled(false);
    _actionWidgets.isLocal->setChecked(false);
    _actionWidgets.isLocal->setEnabled(false);
    _actionWidgets.chooseScripts->setEnabled(false);
    _actionWidgets.script->clear();
    _actionWidgets.script->setEnabled(false);
    _actionWidgets.saveButtons->setEnabled(false);
    _actionWidgets.list->setEnabled(true);
}

void ActionDialog::actionRejected() {
    if (_actionData.back().identifier.empty()) {
        // This happens if someone creates a new action and never gave an identifier
        delete _actionWidgets.list->takeItem(_actionWidgets.list->count() - 1);
        _actionData.erase(_actionData.begin() + _actionData.size() - 1);
    }

    clearActionFields();
}

void ActionDialog::chooseScripts() {
    ScriptlogDialog d(this);
    connect(
        &d, &ScriptlogDialog::scriptsSelected,
        this, &ActionDialog::appendScriptsToTextfield
    );
    d.exec();
}

void ActionDialog::appendScriptsToTextfield(const std::vector<std::string>& scripts) const
{
    for (const std::string& script : scripts) {
        _actionWidgets.script->append(QString::fromStdString(script));
    }
}

Profile::Keybinding* ActionDialog::selectedKeybinding() {
    QListWidgetItem* item = _keybindingWidgets.list->currentItem();
    const int idx = _keybindingWidgets.list->row(item);
    return idx != -1 ? &_keybindingsData[idx] : nullptr;
}

void ActionDialog::keybindingAdd() {
    _keybindingWidgets.list->addItem("");
    _keybindingsData.emplace_back();
    _keybindingWidgets.list->setCurrentRow(_keybindingWidgets.list->count() - 1);
}

void ActionDialog::keybindingRemove() {
    const Profile::Keybinding* keybinding = selectedKeybinding();
    ghoul_assert(keybinding, "A keybinding must be selected at this point");
    
    for (size_t i = 0; i < _keybindingsData.size(); i++) {
        if (_keybindingsData[i].key == keybinding->key &&
            _keybindingsData[i].action == keybinding->action)
        {
            clearKeybindingFields();
            _keybindingsData.erase(_keybindingsData.begin() + i);
            delete _keybindingWidgets.list->takeItem(static_cast<int>(i));
            //Save the updated keybindings to the profile
            if (_keybindings) {
                *_keybindings = _keybindingsData;
            }
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

        const std::string key = ghoul::to_string(keybinding->key.key);
        _keybindingWidgets.key->setCurrentText(QString::fromStdString(key));
        _keybindingWidgets.key->setEnabled(true);
        _keybindingWidgets.action->setCurrentText(
            QString::fromStdString(keybinding->action)
        );
        _keybindingWidgets.action->setEnabled(true);
        _keybindingWidgets.actionText->setText(
            QString::fromStdString(keybinding->action)
        );
        _keybindingWidgets.actionText->setEnabled(true);
        _keybindingWidgets.addButton->setEnabled(false);
        _keybindingWidgets.removeButton->setEnabled(true);

        _keybindingWidgets.saveButtons->setEnabled(true);
        // Only enable the save buttons if a key is selected, otherwise we would get an
        // exception as the None key cannot be bound
        _keybindingWidgets.saveButtons->button(QDialogButtonBox::Save)->setEnabled(
            _keybindingWidgets.key->currentIndex() > 0
        );
        if (_mainButton) {
            _mainButton->setEnabled(false);
        }
        _keybindingWidgets.list->setEnabled(false);
    }
    else {
        // No keybinding selected
        _keybindingWidgets.addButton->setEnabled(true);
        _keybindingWidgets.removeButton->setEnabled(false);
        _keybindingWidgets.saveButtons->setEnabled(false);
        //Action panel must also be in valid state to re-enable main start button
        if (_mainButton && !_actionWidgets.saveButtons->isEnabled()) {
            _mainButton->setEnabled(true);
        }
        _keybindingWidgets.list->setEnabled(true);
    }
}

void ActionDialog::keybindingActionSelected(int) const {
    _keybindingWidgets.actionText->setText(_keybindingWidgets.action->currentText());
}

void ActionDialog::keybindingSaved() {
    if (_keybindingWidgets.key->currentIndex() == -1) {
        QMessageBox::critical(this, "Missing key", "Key must have an assignment");
        return;
    }
    //A selection can be made from the combo box without typing text, but selecting from
    //the combo will fill the text, so using the text box as criteria covers both cases.
    if (_keybindingWidgets.actionText->text().isEmpty()) {
        QMessageBox::critical(this, "Missing action", "Key action must not be empty");
        return;
    }
    Profile::Keybinding* keybinding = selectedKeybinding();
    ghoul_assert(keybinding, "There must be a selected keybinding at this point");

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
    //Save the updated keybindings to the profile
    if (_keybindings) {
        *_keybindings = _keybindingsData;
    }
    clearKeybindingFields();
}

void ActionDialog::clearKeybindingFields() const {
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
    _keybindingWidgets.list->setEnabled(true);
}

void ActionDialog::keybindingRejected() {
    const bool isKeyEmpty = (_keybindingsData.back().key.key == Key::Unknown);
    const bool isActionEmpty = _keybindingsData.back().action.empty();
    if (isKeyEmpty || isActionEmpty) {
        delete _keybindingWidgets.list->takeItem(_keybindingWidgets.list->count() - 1);
        _keybindingsData.erase(_keybindingsData.begin() + _keybindingsData.size() - 1);
    }
    clearKeybindingFields();
}
