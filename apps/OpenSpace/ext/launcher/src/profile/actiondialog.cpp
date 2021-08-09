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

ActionDialog::ActionDialog(openspace::Profile& profile, QWidget* parent)
    : _profile(profile)
{
    _actions.data = _profile.actions();
    _keybindings.data = _profile.keybindings();

    setWindowTitle("Actions and Keybindings");
    createWidgets();
}

void ActionDialog::keyPressEvent(QKeyEvent* evt) {

}

void ActionDialog::createWidgets() {
    QGridLayout* layout = new QGridLayout(this);
    {
        // Action part
        _actions.list = new QListWidget;
        connect(
            _actions.list, &QListWidget::itemSelectionChanged,
            this, &ActionDialog::actionSelected
        );
        _actions.list->setAlternatingRowColors(true);
        _actions.list->setResizeMode(QListView::Adjust);

        for (size_t i = 0; i < _actions.data.size(); ++i) {
            const openspace::Profile::Action& action = _actions.data[i];
            std::string name = action.name.empty() ? action.identifier : action.name;
            _actions.list->addItem(new QListWidgetItem(QString::fromStdString(name)));
        }

        layout->addWidget(_actions.list, 0, 0);

        QWidget* controls = new QWidget;
        QGridLayout* controlsLayout = new QGridLayout(controls);

        controlsLayout->addWidget(new QLabel("Identifier"), 0, 0);
        _actions.identifier = new QLineEdit;
        _actions.identifier->setEnabled(false);
        controlsLayout->addWidget(_actions.identifier, 0, 1);

        controlsLayout->addWidget(new QLabel("Name"), 1, 0);
        _actions.name = new QLineEdit;
        _actions.name->setEnabled(false);
        controlsLayout->addWidget(_actions.name, 1, 1);
        
        controlsLayout->addWidget(new QLabel("GUI Path"), 2, 0);
        _actions.guiPath = new QLineEdit;
        _actions.guiPath->setEnabled(false);
        controlsLayout->addWidget(_actions.guiPath, 2, 1);
        
        controlsLayout->addWidget(new QLabel("Documentation"), 3, 0);
        _actions.documentation = new QLineEdit;
        _actions.documentation->setEnabled(false);
        controlsLayout->addWidget(_actions.documentation, 3, 1);
        
        controlsLayout->addWidget(new QLabel("Is Local"), 4, 0);
        _actions.isLocal = new QCheckBox;
        _actions.isLocal->setEnabled(false);
        controlsLayout->addWidget(_actions.isLocal, 4, 1);
        
        controlsLayout->addWidget(new QLabel("Script"), 5, 0);
        _actions.script = new QTextEdit;
        _actions.script->setEnabled(false);
        controlsLayout->addWidget(_actions.script, 5, 1);

        _actions.buttons = new QDialogButtonBox;
        _actions.buttons->setEnabled(false);
        _actions.buttons->setStandardButtons(
            QDialogButtonBox::Save | QDialogButtonBox::Cancel
        );
        QObject::connect(
            _actions.buttons, &QDialogButtonBox::accepted,
            this, &ActionDialog::actionSaved
        );
        QObject::connect(
            _actions.buttons, &QDialogButtonBox::rejected,
            this, &ActionDialog::actionRejected
        );
        controlsLayout->addWidget(_actions.buttons);

        layout->addWidget(controls, 0, 1);
    }

    layout->addWidget(new Line, 1, 0, 1, 2);

    {
        // Keyboard binding part
        _keybindings.list = new QListWidget;
        connect(
            _keybindings.list, &QListWidget::itemSelectionChanged,
            this, &ActionDialog::keybindingSelected
        );
        _keybindings.list->setAlternatingRowColors(true);
        _keybindings.list->setResizeMode(QListView::Adjust);

        for (size_t i = 0; i < _keybindings.data.size(); ++i) {
            const openspace::Profile::Keybinding& kv = _keybindings.data[i];
            std::string name = fmt::format("{}\t{}", ghoul::to_string(kv.key), kv.action);
            _keybindings.list->addItem(new QListWidgetItem(QString::fromStdString(name)));
        }

        layout->addWidget(_keybindings.list, 2, 0);

        QWidget* controls = new QWidget;
        QGridLayout* controlsLayout = new QGridLayout(controls);

        controlsLayout->addWidget(new QLabel("Modifier"), 0, 0);
        _keybindings.modifier = new QComboBox;
        QStringList modifierList;
        int modIdx = 0;
        for (const std::pair<const int, std::string>& m : openspace::KeyModifierNames) {
            modifierList += QString::fromStdString(m.second);
            //_modifierKeyComboBoxIndexToKeyValue.push_back(modIdx++);
        }
        _keybindings.modifier->addItems(modifierList);
        _keybindings.modifier->setEnabled(false);
        controlsLayout->addWidget(_keybindings.modifier, 0, 1);

        controlsLayout->addWidget(new QLabel("Key"), 1, 0);
        _keybindings.key = new QComboBox;
        QStringList keyList;
        for (int i = 0; i < static_cast<int>(openspace::Key::Last); ++i) {
            if (openspace::KeyNames.find(i) != openspace::KeyNames.end()) {
                keyList += QString::fromStdString(openspace::KeyNames.at(i));
                // Create map to relate key combo box to integer value defined in Key
                //_keyComboBoxIndexToKeyValue.push_back(i);
            }
        }
        _keybindings.key->addItems(keyList);


        _keybindings.key->setEnabled(false);
        controlsLayout->addWidget(_keybindings.key, 1, 1);

        controlsLayout->addWidget(new QLabel("Action"), 2, 0);
        _keybindings.action = new QComboBox;
        for (const openspace::Profile::Action& action : _actions.data) {
            _keybindings.action->addItem(QString::fromStdString(action.identifier));
        }
        _keybindings.action->setEnabled(false);
        controlsLayout->addWidget(_keybindings.action, 2, 1);

        _keybindings.buttons = new QDialogButtonBox;
        _keybindings.buttons->setEnabled(false);
        _keybindings.buttons->setStandardButtons(
            QDialogButtonBox::Save | QDialogButtonBox::Cancel
        );
        QObject::connect(
            _keybindings.buttons, &QDialogButtonBox::accepted,
            this, &ActionDialog::keybindingSaved
        );
        QObject::connect(
            _keybindings.buttons, &QDialogButtonBox::rejected,
            this, &ActionDialog::keybindingRejected
        );
        controlsLayout->addWidget(_keybindings.buttons);


        layout->addWidget(controls, 2, 1);
    }

    layout->addWidget(new Line, 3, 0, 1, 2);
    
    QDialogButtonBox* buttonBox = new QDialogButtonBox;
    buttonBox->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
    layout->addWidget(buttonBox, 4, 1, Qt::AlignRight);
}

openspace::Profile::Action* ActionDialog::selectedAction() {
    QListWidgetItem* item = _actions.list->currentItem();
    const int idx = _actions.list->row(item);
    return &_actions.data[idx];
}

void ActionDialog::actionSelected() {
    const openspace::Profile::Action* action = selectedAction();
    ghoul_assert(action, "There must be a selected action at this point");
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
    _actions.buttons->setEnabled(true);
}

void ActionDialog::actionSaved() {
    openspace::Profile::Action* action = selectedAction();
    std::string oldIdentifier = action->identifier;
    std::string newIdentifier = _actions.identifier->text().toStdString();
    if (oldIdentifier != newIdentifier) {
        // The identifier is a bit special as we need to make sure that we didn't
        // accidentally create a duplicate.  Also if we did create a duplicate we need to
        // make sure that we update all keybinds that referenced the old value are updated
        // to the new one

        const auto it = std::find_if(
            _actions.data.begin(), _actions.data.end(),
            [id = newIdentifier](const openspace::Profile::Action& action) {
                return action.identifier == id;
            }
        );
        if (it == _actions.data.end()) {
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
        for (openspace::Profile::Keybinding& kb : _keybindings.data) {
            if (kb.action == oldIdentifier) {
                kb.action = newIdentifier;
            }
        }
    }
    
    action->name = _actions.name->text().toStdString();
    action->guiPath = _actions.guiPath->text().toStdString();
    action->documentation = _actions.documentation->text().toStdString();
    action->isLocal = _actions.isLocal->isChecked();
    action->script = _actions.script->toPlainText().toStdString();
    clearActionFields();
}

void ActionDialog::clearActionFields() {
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
    _actions.buttons->setEnabled(false);
}

void ActionDialog::actionRejected() {
    clearActionFields();
}

openspace::Profile::Keybinding* ActionDialog::selectedKeybinding() {
    QListWidgetItem* item = _keybindings.list->currentItem();
    const int idx = _keybindings.list->row(item);
    return &_keybindings.data[idx];
}

void ActionDialog::keybindingSelected() {
    const openspace::Profile::Keybinding* keybinding = selectedKeybinding();
    ghoul_assert(keybinding, "There must be a selected action at this point");
    
    std::string modifier = ghoul::to_string(keybinding->key.modifier);
    std::string key = ghoul::to_string(keybinding->key.key);
    std::string action = keybinding->action;

    _keybindings.modifier->setCurrentText(QString::fromStdString(modifier));
    _keybindings.modifier->setEnabled(true);
    _keybindings.key->setCurrentText(QString::fromStdString(key));
    _keybindings.key->setEnabled(true);
    _keybindings.action->setCurrentText(QString::fromStdString(action));
    _keybindings.action->setEnabled(true);
    _keybindings.buttons->setEnabled(true);
}

void ActionDialog::keybindingSaved() {
    openspace::Profile::Keybinding* keybinding = selectedKeybinding();
    ghoul_assert(keybinding, "There must be a selected action at this point");

    keybinding->key = openspace::stringToKey(fmt::format(
        "{}+{}",
        _keybindings.modifier->currentText().toStdString(),
        _keybindings.key->currentText().toStdString()
    ));
    keybinding->action = _keybindings.action->currentText().toStdString();
    clearKeybindingFields();
}

void ActionDialog::clearKeybindingFields() {
    _keybindings.modifier->setCurrentIndex(-1);
    _keybindings.modifier->setEnabled(false);
    _keybindings.key->setCurrentIndex(-1);
    _keybindings.key->setEnabled(false);
    _keybindings.action->setCurrentIndex(-1);
    _keybindings.action->setEnabled(false);
}

void ActionDialog::keybindingRejected() {
    clearKeybindingFields();
}
