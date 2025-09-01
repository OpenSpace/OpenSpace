/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "profile/modulesdialog.h"

#include "profile/line.h"
#include <QDialogButtonBox>
#include <QEvent>
#include <QKeyEvent>
#include <QLabel>
#include <QListWidget>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QVBoxLayout>

using namespace openspace;

namespace {
    const Profile::Module Blank = {
        .name = "",
        .loadedInstruction = std::nullopt,
        .notLoadedInstruction = std::nullopt
    };

    QString createOneLineSummary(Profile::Module m) {
        QString summary = QString::fromStdString(m.name);
        const bool hasCommandForLoaded = !m.loadedInstruction->empty();
        const bool hasCommandForNotLoaded = !m.notLoadedInstruction->empty();

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
} // namespace

ModulesDialog::ModulesDialog(QWidget* parent,
                             std::vector<openspace::Profile::Module>* modules)
    : QDialog(parent)
    , _modules(modules)
    , _moduleData(*_modules)
{
    setWindowTitle("Modules");
    createWidgets();

    transitionFromEditMode();
}

void ModulesDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);

    _list = new QListWidget;
    connect(
        _list, &QListWidget::itemSelectionChanged,
        this, &ModulesDialog::listItemSelected
    );
    _list->setAlternatingRowColors(true);
    _list->setMovement(QListView::Free);
    _list->setResizeMode(QListView::Adjust);

    for (const Profile::Module& m : _moduleData) {
        _list->addItem(new QListWidgetItem(createOneLineSummary(m)));
    }
    layout->addWidget(_list);

    {
        QBoxLayout* box = new QHBoxLayout;
        _buttonAdd = new QPushButton("Add new");
        connect(_buttonAdd, &QPushButton::clicked, this, &ModulesDialog::listItemAdded);
        _buttonAdd->setAccessibleName("Add new module");
        box->addWidget(_buttonAdd);

        _buttonRemove = new QPushButton("Remove");
        connect(
            _buttonRemove, &QPushButton::clicked,
            this, &ModulesDialog::listItemRemove
        );
        _buttonRemove->setAccessibleName("Remove module");
        box->addWidget(_buttonRemove);

        box->addStretch();

        layout->addLayout(box);
    }
    {
        _moduleLabel = new QLabel("Module");
        layout->addWidget(_moduleLabel);

        _moduleEdit = new QLineEdit;
        _moduleEdit->setToolTip("Name of OpenSpace module related to this profile");
        layout->addWidget(_moduleEdit);

        _loadedLabel = new QLabel("Command if Module is Loaded");
        layout->addWidget(_loadedLabel);

        _loadedEdit = new QLineEdit;
        _loadedEdit->setToolTip(
            "Lua command(s) to execute if OpenSpace has been compiled with the module"
        );
        layout->addWidget(_loadedEdit);

        _notLoadedLabel = new QLabel("Command if Module is NOT Loaded");
        layout->addWidget(_notLoadedLabel);

        _notLoadedEdit = new QLineEdit;
        _notLoadedEdit->setToolTip(
            "Lua command(s) to execute if the module is not present in the OpenSpace "
            "application (for example steps to account for a missing module)"
        );
        layout->addWidget(_notLoadedEdit);
    }
    {
        QBoxLayout* box = new QHBoxLayout;
        _buttonSave = new QPushButton("Save");
        _buttonSave->setToolTip("Save module changes to the above list");
        connect(_buttonSave, &QPushButton::clicked, this, &ModulesDialog::listItemSave);
        box->addWidget(_buttonSave);

        _buttonCancel = new QPushButton("Cancel");
        _buttonCancel->setToolTip("Cancel adding this module to the above list");
        connect(
            _buttonCancel, &QPushButton::clicked,
            this, &ModulesDialog::listItemCancelSave
        );
        box->addWidget(_buttonCancel);

        box->addStretch();

        layout->addLayout(box);
    }
    layout->addWidget(new Line);
    {
        QBoxLayout* footerLayout = new QHBoxLayout;
        _buttonBox = new QDialogButtonBox;
        _buttonBox->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        QObject::connect(
            _buttonBox, &QDialogButtonBox::accepted,
            this, &ModulesDialog::parseSelections
        );
        QObject::connect(
            _buttonBox, &QDialogButtonBox::rejected,
            this, &ModulesDialog::reject
        );
        footerLayout->addWidget(_buttonBox);
        layout->addLayout(footerLayout);
    }
}

void ModulesDialog::listItemSelected() {
    QListWidgetItem* item = _list->currentItem();
    const int index = _list->row(item);

    if (!_moduleData.empty()) {
        const Profile::Module& m = _moduleData[index];
        _moduleEdit->setText(QString::fromStdString(m.name));
        if (m.loadedInstruction.has_value()) {
            _loadedEdit->setText(QString::fromStdString(*m.loadedInstruction));
        }
        else {
            _loadedEdit->clear();
        }

        if (m.notLoadedInstruction.has_value()) {
            _notLoadedEdit->setText(QString::fromStdString(*m.notLoadedInstruction));
        }
        else {
            _notLoadedEdit->clear();
        }
    }
    transitionToEditMode();
}

bool ModulesDialog::isLineEmpty(int index) const {
    return
        _list->item(index)->text().isEmpty() &&
        _moduleData.empty() && !_moduleData.at(0).name.empty();
}

void ModulesDialog::listItemAdded() {
    const int currentListSize = _list->count();

    if ((currentListSize == 1) && (isLineEmpty(0))) {
        // Special case where list is "empty" but really has one line that is blank.
        // This is done because QListWidget does not seem to like having its sole
        // remaining item being removed.
        _moduleData.at(0) = Blank;
        _list->item(0)->setText("  (Enter details below & click 'Save')");
        _list->setCurrentRow(0);
        transitionToEditMode();
    }
    else {
        _moduleData.push_back(Blank);
        _list->addItem(new QListWidgetItem("  (Enter details below & click 'Save')"));
        //Scroll down to that blank line highlighted
        _list->setCurrentRow(_list->count() - 1);
    }

    // Blank-out the 2 text fields, set combo box to index 0
    _moduleEdit->setText(QString::fromStdString(_moduleData.back().name));
    if (_moduleData.back().loadedInstruction.has_value()) {
        _loadedEdit->setText(QString::fromStdString(
            *_moduleData.back().loadedInstruction
        ));
    }
    else {
        _loadedEdit->clear();
    }
    if (_moduleData.back().notLoadedInstruction.has_value()) {
        _notLoadedEdit->setText(
            QString::fromStdString(*_moduleData.back().notLoadedInstruction)
        );
    }
    else {
        _notLoadedEdit->clear();
    }
    _moduleEdit->setFocus(Qt::OtherFocusReason);
    _editModeNewItem = true;
}

void ModulesDialog::listItemSave() {
    if (_moduleEdit->text().isEmpty()) {
        QMessageBox::critical(this, "Error", "Missing module name");
        return;
    }

    QListWidgetItem* item = _list->currentItem();
    const int index = _list->row(item);

    if (!_moduleData.empty()) {
        _moduleData[index].name = _moduleEdit->text().toStdString();
        _moduleData[index].loadedInstruction = _loadedEdit->text().toStdString();
        _moduleData[index].notLoadedInstruction = _notLoadedEdit->text().toStdString();
        _list->item(index)->setText(createOneLineSummary(_moduleData[index]));
    }
    transitionFromEditMode();
    _editModeNewItem = false;
}

void ModulesDialog::listItemCancelSave() {
    transitionFromEditMode();
    if (_editModeNewItem && !_moduleData.empty() && _moduleData.back().name.empty()) {
        listItemRemove();
    }
    _editModeNewItem = false;
}

void ModulesDialog::listItemRemove() {
    if (_list->count() > 0 &&
        _list->currentRow() >= 0 && _list->currentRow() < _list->count())
    {
        if (_list->count() == 1) {
            // Special case where last remaining item is being removed (QListWidget
            // doesn't like the final item being removed so instead clear it)
            _moduleData.at(0) = Blank;
            _list->item(0)->setText("");
        }
        else {
            const int index = _list->currentRow();
            if (index >= 0 && index < _list->count()) {
                delete _list->takeItem(index);
                if (!_moduleData.empty()) {
                    _moduleData.erase(_moduleData.begin() + index);
                }
            }
        }
    }
    transitionFromEditMode();
}

void ModulesDialog::transitionToEditMode() {
    _buttonSave->setDisabled(true);
    _buttonCancel->setDisabled(true);
    _buttonBox->setDisabled(true);

    _moduleLabel->setText("<font color='black'>Module</font>");
    _loadedLabel->setText("<font color='black'>Command if Module is Loaded</font>");
    _notLoadedLabel->setText("<font color='black'>Command if Module is NOT Loaded</font>");
    editBoxDisabled(false);
}

void ModulesDialog::transitionFromEditMode() {
    _list->setDisabled(false);
    _buttonAdd->setDisabled(false);
    _buttonRemove->setDisabled(false);
    _buttonSave->setDisabled(false);
    _buttonCancel->setDisabled(false);
    _buttonBox->setDisabled(false);

    editBoxDisabled(true);
    _moduleLabel->setText("<font color='light gray'>Module</font>");
    _loadedLabel->setText("<font color='light gray'>Command if Module is Loaded</font>");
    _notLoadedLabel->setText("<font color='light gray'>Command if Module is NOT Loaded</font>");
}

void ModulesDialog::editBoxDisabled(bool disabled) {
    _moduleLabel->setDisabled(disabled);
    _moduleEdit->setDisabled(disabled);
    _loadedLabel->setDisabled(disabled);
    _loadedEdit->setDisabled(disabled);
    _notLoadedLabel->setDisabled(disabled);
    _notLoadedEdit->setDisabled(disabled);
    _buttonCancel->setDisabled(disabled);
    _buttonSave->setDisabled(disabled);
}

void ModulesDialog::parseSelections() {
    // Handle case with only one remaining but empty line
    if ((_moduleData.size() == 1) && (_moduleData.at(0).name.empty())) {
        _moduleData.clear();
    }
    *_modules = std::move(_moduleData);
    accept();
}

void ModulesDialog::keyPressEvent(QKeyEvent* evt) {
    if (evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return) {
        if (_editModeNewItem) {
            listItemSave();
        }
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
