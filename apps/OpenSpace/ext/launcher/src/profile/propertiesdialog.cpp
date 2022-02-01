/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include "profile/propertiesdialog.h"

#include "profile/line.h"
#include <ghoul/filesystem/filesystem.h>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QEvent>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPushButton>
#include <QTextStream>
#include <QVBoxLayout>
#include <iostream>

using namespace openspace;

namespace {
    const Profile::Property Blank {
        Profile::Property::SetType::SetPropertyValueSingle,
        "",
        ""
    };
} // namespace

PropertiesDialog::PropertiesDialog(QWidget* parent,
                                   std::vector<openspace::Profile::Property>* properties)
    : QDialog(parent)
    , _properties(properties)
    , _propertyData(*_properties)
{
    setWindowTitle("Set Property Values");
    createWidgets();

    transitionFromEditMode();
}

void PropertiesDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        _list = new QListWidget;
        connect(
            _list, &QListWidget::itemSelectionChanged,
            this, &PropertiesDialog::listItemSelected
        );
        for (size_t i = 0; i < _propertyData.size(); ++i) {
            _list->addItem(new QListWidgetItem(createOneLineSummary(_propertyData[i])));
        }
        layout->addWidget(_list);
    }
    {
        QBoxLayout* box = new QHBoxLayout;
        _addButton = new QPushButton("Add New");
        connect(
            _addButton, &QPushButton::clicked,
            this, &PropertiesDialog::listItemAdded
        );
        box->addWidget(_addButton);

        _removeButton = new QPushButton("Remove");
        connect(
            _removeButton, &QPushButton::clicked,
            this, &PropertiesDialog::listItemRemove
        );
        box->addWidget(_removeButton);

        box->addStretch();

        layout->addLayout(box);
    }
    layout->addWidget(new Line);
    {
        _fillFromScriptLog = new QPushButton("Fill from ScriptLog");
        connect(
            _fillFromScriptLog, &QPushButton::clicked,
            this, &PropertiesDialog::selectLineFromScriptLog
        );
        layout->addWidget(_fillFromScriptLog);

        _commandLabel = new QLabel("Property Set Command");
        layout->addWidget(_commandLabel);

        _commandCombo = new QComboBox;
        _commandCombo->addItems({ "SetPropertyValueSingle", "SetPropertyValue" });
        layout->addWidget(_commandCombo);

        _propertyLabel = new QLabel("Property");
        layout->addWidget(_propertyLabel);
        _propertyEdit = new QLineEdit;
        _propertyEdit->setToolTip("Exact string is required for the property to be set");
        layout->addWidget(_propertyEdit);

        _valueLabel = new QLabel("Value to Set");
        layout->addWidget(_valueLabel);
        _valueEdit = new QLineEdit;
        layout->addWidget(_valueEdit);

        {
            QBoxLayout* box = new QHBoxLayout;
            _saveButton = new QPushButton("Save");
            connect(
                _saveButton, &QPushButton::clicked,
                this, &PropertiesDialog::listItemSave
            );
            box->addWidget(_saveButton);

            _cancelButton = new QPushButton("Cancel");
            connect(
                _cancelButton, &QPushButton::clicked,
                this, &PropertiesDialog::listItemCancelSave
            );
            box->addWidget(_cancelButton);

            box->addStretch();

            layout->addLayout(box);
        }
    }
    layout->addWidget(new Line);
    {
        QBoxLayout* footerLayout = new QHBoxLayout;

        _errorMsg = new QLabel;
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footerLayout->addWidget(_errorMsg);

        _buttonBox = new QDialogButtonBox;
        _buttonBox->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);

        connect(
            _buttonBox, &QDialogButtonBox::accepted,
            this, &PropertiesDialog::parseSelections
        );
        connect(
            _buttonBox, &QDialogButtonBox::rejected,
            this, &PropertiesDialog::reject
        );
        footerLayout->addWidget(_buttonBox);
        layout->addLayout(footerLayout);
    }
}

QString PropertiesDialog::createOneLineSummary(Profile::Property p) {
    QString summary = QString::fromStdString(p.name);
    summary += " = ";
    summary += QString::fromStdString(p.value);
    summary += " (SetPropertyValue";
    if (p.setType == Profile::Property::SetType::SetPropertyValueSingle) {
        summary += "Single";
    }
    summary += ")";
    return summary;
}

void PropertiesDialog::listItemSelected() {
    QListWidgetItem* item = _list->currentItem();
    int index = _list->row(item);

    if (_propertyData.size() > 0) {
        Profile::Property& p = _propertyData[index];
        if (p.setType == Profile::Property::SetType::SetPropertyValueSingle) {
            _commandCombo->setCurrentIndex(0);
        }
        else {
            _commandCombo->setCurrentIndex(1);
        }
        _propertyEdit->setText(QString::fromStdString(p.name));
        _valueEdit->setText(QString::fromStdString(p.value));
    }
    transitionToEditMode();
}

bool PropertiesDialog::isLineEmpty(int index) {
    bool isEmpty = true;
    if (!_list->item(index)->text().isEmpty()) {
        isEmpty = false;
    }
    if (!_propertyData.empty() && !_propertyData.at(0).name.empty()) {
        isEmpty = false;
    }
    return isEmpty;
}

void PropertiesDialog::listItemAdded() {
    int currentListSize = _list->count();

     if ((currentListSize == 1) && (isLineEmpty(0))) {
         // Special case where list is "empty" but really has one line that is blank.
         // This is done because QListWidget does not seem to like having its sole
         // remaining item being removed.
         _propertyData.at(0) = Blank;
         _list->item(0)->setText("  (Enter details below & click 'Save')");
         _list->setCurrentRow(0);
         transitionToEditMode();
     }
     else {
         _propertyData.push_back(Blank);
         _list->addItem(new QListWidgetItem("  (Enter details below & click 'Save')"));
         //Scroll down to that blank line highlighted
         _list->setCurrentRow(_list->count() - 1);
     }

    // Blank-out the 2 text fields, set combo box to index 0
    _commandCombo->setCurrentIndex(0);
    _propertyEdit->setText(QString::fromStdString(_propertyData.back().name));
    _valueEdit->setText(QString::fromStdString(_propertyData.back().value));
    _commandCombo->setFocus(Qt::OtherFocusReason);
    _editModeNewItem = true;
}

void PropertiesDialog::listItemSave() {
    if (!areRequiredFormsFilled()) {
        return;
    }

    QListWidgetItem* item = _list->currentItem();
    int index = _list->row(item);

    if (_propertyData.size() > 0) {
        if (_commandCombo->currentIndex() == 0) {
            _propertyData[index].setType =
                Profile::Property::SetType::SetPropertyValueSingle;
        }
        else {
            _propertyData[index].setType = Profile::Property::SetType::SetPropertyValue;
        }
        _propertyData[index].name = _propertyEdit->text().toStdString();
        _propertyData[index].value = _valueEdit->text().toStdString();
        _list->item(index)->setText(createOneLineSummary(_propertyData[index]));
    }
    transitionFromEditMode();
    _editModeNewItem = false;
}

bool PropertiesDialog::areRequiredFormsFilled() {
    bool requiredFormsFilled = true;
    QString errors;
    if (_propertyEdit->text().length() == 0) {
        errors += "Missing property name";
        requiredFormsFilled = false;
    }
    if (_valueEdit->text().length() == 0) {
        if (errors.length() > 0) {
            errors += ", ";
        }
        errors += "Missing value";
        requiredFormsFilled = false;
    }
    _errorMsg->setText("<font color='red'>" + errors + "</font>");
    return requiredFormsFilled;
}

void PropertiesDialog::listItemCancelSave() {
    listItemSelected();
    transitionFromEditMode();
    if (_editModeNewItem) {
        if (_propertyData.size() > 0) {
            if (_propertyData.back().name.length() == 0 ||
                _propertyData.back().value.length() == 0)
            {
                listItemRemove();
            }
        }
    }
    _editModeNewItem = false;
}

void PropertiesDialog::listItemRemove() {
    if (_list->count() > 0) {
        if (_list->currentRow() >= 0 && _list->currentRow() < _list->count()) {
            if (_list->count() == 1) {
                //Special case where last remaining item is being removed (QListWidget
                // doesn't like the final item being removed so instead clear it)
                _propertyData.at(0) = Blank;
                _list->item(0)->setText("");
            }
            else {
                int index = _list->currentRow();
                if (index >= 0 && index < _list->count()) {
                    delete _list->takeItem(index);
                    if (_propertyData.size() > 0) {
                        _propertyData.erase(_propertyData.begin() + index);
                    }
                }
            }
        }
    }
    transitionFromEditMode();
}

void PropertiesDialog::transitionToEditMode() {
    _saveButton->setDisabled(true);
    _cancelButton->setDisabled(true);
    _buttonBox->setDisabled(true);

    _commandLabel->setText("<font color='black'>Property Set Command</font>");
    _propertyLabel->setText("<font color='black'>Property</font>");
    _valueLabel->setText("<font color='black'>Value to set</font>");
    editBoxDisabled(false);
    _errorMsg->setText("");
}

void PropertiesDialog::transitionFromEditMode() {
    _list->setDisabled(false);
    _addButton->setDisabled(false);
    _removeButton->setDisabled(false);
    _saveButton->setDisabled(false);
    _cancelButton->setDisabled(false);
    _buttonBox->setDisabled(false);

    _commandLabel->setText("<font color='light gray'>Property Set Command</font>");
    _propertyLabel->setText("<font color='light gray'>Property</font>");
    _valueLabel->setText("<font color='light gray'>Value to set</font>");
    editBoxDisabled(true);
    _errorMsg->setText("");
}

void PropertiesDialog::editBoxDisabled(bool disabled) {
    _fillFromScriptLog->setDisabled(disabled);
    _commandLabel->setDisabled(disabled);
    _commandCombo->setDisabled(disabled);
    _propertyLabel->setDisabled(disabled);
    _propertyEdit->setDisabled(disabled);
    _valueLabel->setDisabled(disabled);
    _valueEdit->setDisabled(disabled);
    _saveButton->setDisabled(disabled);
    _cancelButton->setDisabled(disabled);
}

void PropertiesDialog::parseSelections() {
    // Handle case with only one remaining but empty line
    if ((_propertyData.size() == 1) && (_propertyData.at(0).name.compare("") == 0)) {
        _propertyData.clear();
    }
    *_properties = std::move(_propertyData);
    accept();
}

void PropertiesDialog::keyPressEvent(QKeyEvent* evt) {
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

void PropertiesDialog::selectLineFromScriptLog() {
    QComboBox* comboBox = new QComboBox;

    QFile file(QString::fromStdString(absPath("${LOGS}/scriptLog.txt").string()));
    if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        QTextStream in(&file);
        while (!in.atEnd()) {
            QString line = in.readLine();
            // removing return from a few statments
            // these are usually generated by gui panels
            line.remove(QRegularExpression("^return "));

            if (line.isEmpty()) {
                continue;
            }

            if (!line.startsWith("openspace.setPropertyValue")) {
                continue;
            }

            comboBox->addItem(line);
        }
    }

    QDialog dialog;

    connect(&dialog, &QDialog::finished, [this, comboBox](int result) {
        if (result == QDialog::Rejected) {
            return;
        }

        QString text = comboBox->currentText();
        if (!text.startsWith("openspace.setPropertyValue")) {
            return;
        }

        // We have a string that is of the form:
        // openspace.setPropertyValue('prop', value);

        if (text.startsWith("openspace.setPropertyValueSingle")) {
            using namespace std::string_view_literals;
            _commandCombo->setCurrentIndex(0);
            text = text.mid("openspace.setPropertyValueSingle"sv.size() + 1); // +1 for (
        }
        else {
            // command == "openspace.setPropertyValue"
            using namespace std::string_view_literals;
            _commandCombo->setCurrentIndex(1);
            text = text.mid("openspace.setPropertyValue"sv.size() + 1); // +1 for (
        }

        // Remove everything past the closing brace
        text = text.left(text.indexOf(")"));
        QStringList textList = text.split(",");

        if (textList.size() < 2) {
            return;
        }

        // Remove the string markers around the property
        QString property = textList[0].mid(1, textList[0].size() - 2);

        textList.removeFirst();
        QString value = textList.join(",");


        _propertyEdit->setText(property.trimmed());
        _valueEdit->setText(value.trimmed());
    });

    QLayout* layout = new QVBoxLayout;
    QLabel* label = new QLabel("Select a line from the Script Log to add");
    layout->addWidget(label);

    layout->addWidget(comboBox);

    QDialogButtonBox* bb = new QDialogButtonBox(
        QDialogButtonBox::Ok | QDialogButtonBox::Cancel
    );
    connect(bb, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    connect(bb, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);
    layout->addWidget(bb);
    
    dialog.setLayout(layout);
    dialog.exec();
}
