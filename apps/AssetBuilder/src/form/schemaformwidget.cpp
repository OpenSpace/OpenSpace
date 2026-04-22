/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include "form/schemaformwidget.h"

#include "form/matrixwidget.h"
#include "utils.h"

#include <QCheckBox>
#include <QComboBox>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>

#include <algorithm>
#include <optional>
#include <string>
#include <vector>

namespace {

constexpr int LabelColumnMinWidth = 130;
constexpr int ColumnSpacing = 8;
constexpr int RowSpacing = 4;
constexpr int InfoButtonSize = 16;

bool hasWidgetContent(QWidget* widget) {
    if (auto* checkBox = qobject_cast<QCheckBox*>(widget)) {
        return checkBox->isChecked();
    }
    if (auto* matrix = qobject_cast<MatrixWidget*>(widget)) {
        return matrix->hasContent();
    }
    if (auto* lineEdit = qobject_cast<QLineEdit*>(widget)) {
        return !lineEdit->text().isEmpty();
    }
    if (widget->objectName() == FileContainerName) {
        auto* lineEdit = widget->findChild<QLineEdit*>();
        return lineEdit && !lineEdit->text().isEmpty();
    }
    if (auto* combo = widget->findChild<QComboBox*>()) {
        return !combo->currentText().isEmpty();
    }
    return false;
}

void clearWidget(QWidget* widget) {
    if (auto* checkBox = qobject_cast<QCheckBox*>(widget)) {
        checkBox->blockSignals(true);
        checkBox->setChecked(false);
        checkBox->blockSignals(false);
    }
    else if (auto* matrix = qobject_cast<MatrixWidget*>(widget)) {
        matrix->blockSignals(true);
        matrix->clear();
        matrix->blockSignals(false);
    }
    else if (auto* lineEdit = qobject_cast<QLineEdit*>(widget)) {
        lineEdit->blockSignals(true);
        lineEdit->clear();
        lineEdit->blockSignals(false);
    }
    else if (widget->objectName() == FileContainerName) {
        auto* lineEdit = widget->findChild<QLineEdit*>();
        if (lineEdit) {
            lineEdit->blockSignals(true);
            lineEdit->clear();
            lineEdit->blockSignals(false);
        }
    }
    else if (auto* combo = widget->findChild<QComboBox*>()) {
        combo->blockSignals(true);
        combo->setCurrentText("");
        combo->blockSignals(false);
    }
}

// Reads the current value from a field widget and returns it as a PropertyValue.
// The member type is needed to distinguish numeric/date/string line edits.
// Returns std::nullopt if the widget is empty or unrecognized.
std::optional<PropertyValue> readWidgetValue(QWidget* widget, const std::string& type) {
    if (auto* checkBox = qobject_cast<QCheckBox*>(widget)) {
        return PropertyValue{ checkBox->isChecked() };
    }
    if (auto* matrix = qobject_cast<MatrixWidget*>(widget)) {
        return PropertyValue{ matrix->values() };
    }
    if (auto* lineEdit = qobject_cast<QLineEdit*>(widget)) {
        const QString text = lineEdit->text();
        if (text.isEmpty()) {
            return std::nullopt;
        }
        if (type == "Integer" || type == "Double") {
            bool ok = false;
            // Internally integers are stored as double
            const double value = text.toDouble(&ok);
            return ok ? std::optional(PropertyValue{ value }) : std::nullopt;
        }
        if (type == "Date and time") {
            const QDateTime dateTime =
                QDateTime::fromString(text, DateDisplayFormat);
            if (!dateTime.isValid()) {
                return std::nullopt;
            }
            return PropertyValue{
                dateTime.toUTC().toString(Qt::ISODate).toStdString()
            };
        }
        return PropertyValue{ text.toStdString() };
    }
    if (widget->objectName() == FileContainerName) {
        auto* fileEdit = widget->findChild<QLineEdit*>();
        if (fileEdit && !fileEdit->text().isEmpty()) {
            return PropertyValue{ fileEdit->text().toStdString() };
        }
        return std::nullopt;
    }
    if (auto* combo = widget->findChild<QComboBox*>()) {
        const QString text = combo->currentText();
        if (!text.isEmpty()) {
            return PropertyValue{ text.toStdString() };
        }
    }
    return std::nullopt;
}

void populateWidget(QWidget* widget, const PropertyValue& propertyValue) {
    if (auto* checkBox = qobject_cast<QCheckBox*>(widget)) {
        checkBox->blockSignals(true);
        checkBox->setChecked(
            propertyValue.isBool() ? propertyValue.toBool() : false
        );
        checkBox->blockSignals(false);
    }
    else if (auto* matrix = qobject_cast<MatrixWidget*>(widget)) {
        matrix->blockSignals(true);
        if (propertyValue.isList()) {
            matrix->setValues(propertyValue.toList());
        }
        matrix->blockSignals(false);
    }
    else if (auto* lineEdit = qobject_cast<QLineEdit*>(widget)) {
        lineEdit->blockSignals(true);
        if (propertyValue.isDouble()) {
            lineEdit->setText(
                QString::number(propertyValue.toDouble(), 'g', 10)
            );
        }
        else if (propertyValue.isString()) {
            QString value =
                QString::fromStdString(propertyValue.toString());
            // Convert ISO date to display format for date fields only
            // (avoid mangling plain strings)
            if (lineEdit->objectName() == DateEditName) {
                const QDateTime dateTime =
                    QDateTime::fromString(value, Qt::ISODate);
                if (dateTime.isValid()) {
                    value = dateTime.toLocalTime()
                        .toString(DateDisplayFormat);
                }
            }
            lineEdit->setText(value);
        }
        lineEdit->blockSignals(false);
    }
    else if (widget->objectName() == FileContainerName) {
        auto* fileEdit = widget->findChild<QLineEdit*>();
        if (fileEdit) {
            fileEdit->blockSignals(true);
            if (propertyValue.isString()) {
                fileEdit->setText(
                    QString::fromStdString(propertyValue.toString())
                );
            }
            fileEdit->blockSignals(false);
        }
    }
    else if (auto* combo = widget->findChild<QComboBox*>()) {
        combo->blockSignals(true);
        if (propertyValue.isString()) {
            combo->setCurrentText(
                QString::fromStdString(propertyValue.toString())
            );
        }
        combo->blockSignals(false);
    }
}

// Resolves the editable QLineEdit from a member widget. The widget may be
// a QLineEdit directly, or a QComboBox wrapping one (e.g. Identifier).
QLineEdit* resolveLineEdit(QWidget* widget) {
    if (!widget) {
        return nullptr;
    }
    if (auto* lineEdit = qobject_cast<QLineEdit*>(widget)) {
        return lineEdit;
    }
    if (auto* combo = widget->findChild<QComboBox*>()) {
        return combo->lineEdit();
    }
    return nullptr;
}

// A union field stores only the raw property value (no type tag), so we need
// to deduce which type it is by inspecting the value and matching it against
// the entries in the union's type combo box.
//
// The combo entries correspond to schema types like "Double", "String",
// "Vector3<double>", etc. We check the value's runtime type (isDouble,
// isString, isList, isBool) and search the combo for a matching entry.
// Some runtime types map to multiple possible schema types (e.g. a string
// could be "String", "File", or "Date and time"), so we try candidates in
// order and use the first one present in the combo.
//
// matchIndex tracks the result: the combo index to activate. If no match is
// found (matchIndex stays < 0), we fall back to index 0 (the first entry).
// Setting the combo index triggers the show/hide lambda from
// createUnionWidget, which makes the correct field widget visible.
void populateUnionWidget(QWidget* container, const PropertyValue& value) {
    auto* typeCombo = container->findChild<QComboBox*>(UnionTypeComboName);
    if (!typeCombo) {
        return;
    }

    int matchIndex = 0;
    if (value.isDouble()) {
        for (const char* name : { "Double", "Integer" }) {
            matchIndex = typeCombo->findText(name);
            if (matchIndex >= 0) {
                break;
            }
        }
    }
    else if (value.isBool()) {
        matchIndex = typeCombo->findText("Boolean");
    }
    else if (value.isList()) {
        for (int i = 0; i < typeCombo->count(); ++i) {
            const QString text = typeCombo->itemText(i);
            if (text.startsWith("Vector") || text.startsWith("Color") ||
                text.startsWith("Matrix"))
            {
                matchIndex = i;
                break;
            }
        }
    }
    else if (value.isString()) {
        for (const char* name : { "String", "Date and time", "File",
                                   "Directory", "Identifier" })
        {
            matchIndex = typeCombo->findText(name);
            if (matchIndex >= 0) {
                break;
            }
        }
    }
    if (matchIndex < 0) {
        matchIndex = 0;
    }

    // setCurrentIndex triggers the show/hide lambda
    typeCombo->setCurrentIndex(matchIndex);

    // Populate the now-visible field widget
    const auto children = container->findChildren<QWidget*>(
        QString(), Qt::FindDirectChildrenOnly
    );
    for (QWidget* fieldWidget : children) {
        if (fieldWidget->isVisible() &&
            fieldWidget->objectName() != UnionTypeComboName)
        {
            populateWidget(fieldWidget, value);
            break;
        }
    }
}

} // namespace

SchemaFormWidget::SchemaFormWidget(
    const std::vector<SchemaMember>& members,
    PropertyMap& properties,
    QWidget* parent,
    bool subSectionsExpanded,
    bool sortMembers,
    IdentifierRegistry* registry
)
    : QWidget(parent)
    , _members(members)
    , _properties(properties)
    , _subSectionsExpanded(subSectionsExpanded)
    , _sortMembers(sortMembers)
    , _registry(registry)
{
    _fieldWidgets.resize(_members.size(), nullptr);
    _fieldAddButtons.resize(_members.size(), nullptr);
    _fieldActiveRows.resize(_members.size(), nullptr);
    _optionalFieldActive.resize(_members.size(), true);

    buildUi();
}

void SchemaFormWidget::addMemberToGrid(
    QGridLayout* grid, int row, int memberIndex,
    const SchemaMember& member)
{
    if (member.type == "Table") {
        QWidget* section = createTableSection(member);
        _fieldWidgets[memberIndex] = section;
        grid->addWidget(section, row, 0, 1, 2);
        return;
    }

    // Label container: info button + name label + optional asterisk
    {
        QWidget* labelContainer = new QWidget(this);
        labelContainer->setSizePolicy(
            QSizePolicy::Preferred, QSizePolicy::Fixed
        );

        QHBoxLayout* labelLayout = new QHBoxLayout(labelContainer);
        labelLayout->setContentsMargins(0, 0, 0, 0);
        labelLayout->setSpacing(4);

        labelLayout->addWidget(createInfoButton(member, labelContainer));

        QLabel* nameLabel = new QLabel(
            splitPascalCase(member.name), labelContainer
        );
        nameLabel->setObjectName("field-label");
        labelLayout->addWidget(nameLabel);

        if (!member.isOptional) {
            QLabel* asterisk = new QLabel("*", labelContainer);
            asterisk->setObjectName("field-required-asterisk");
            labelLayout->addWidget(asterisk);
        }
        labelLayout->addStretch();
        grid->addWidget(
            labelContainer, row, 0,
            Qt::AlignVCenter | Qt::AlignLeft
        );
    }

    QWidget* field = createFlatWidget(member);
    _fieldWidgets[memberIndex] = field;

    if (member.isOptional) {
        grid->addWidget(createOptionalWrapper(memberIndex, field), row, 1);
    }
    else {
        grid->addWidget(field, row, 1);
    }
}

QPushButton* SchemaFormWidget::createInfoButton(
    const SchemaMember& member, QWidget* parent)
{
    Documentation info;
    info.name = QString::fromStdString(member.name);
    info.type = QString::fromStdString(member.type);
    info.isOptional = member.isOptional;
    info.description = QString::fromStdString(member.description);
    info.documentation = QString::fromStdString(member.documentation);

    QPushButton* button = new QPushButton("i", parent);
    button->setObjectName("field-info-button");
    button->setFixedSize(InfoButtonSize, InfoButtonSize);

    connect(button, &QPushButton::clicked, this,
        [this, info]() { emit documentationRequested(info); }
    );
    return button;
}

QWidget* SchemaFormWidget::createOptionalWrapper(int memberIndex, QWidget* field) {
    const std::string& name = _members[memberIndex].name;
    const bool alreadySet = _properties.count(name) > 0;

    // Container holds two mutually-exclusive rows: a "+" button (when the
    // field is inactive) and the field + remove button (when active).
    // Both are created upfront and toggled via setOptionalFieldActive.
    QWidget* optionalContainer = new QWidget(this);
    QVBoxLayout* containerLayout = new QVBoxLayout(optionalContainer);
    containerLayout->setContentsMargins(0, 0, 0, 0);
    containerLayout->setSpacing(0);

    // "+" button shown when the optional field is not yet added
    QPushButton* addButton = new QPushButton("+", optionalContainer);
    addButton->setObjectName("field-add-button");
    addButton->setVisible(!alreadySet);
    _fieldAddButtons[memberIndex] = addButton;
    containerLayout->addWidget(addButton);

    // Active row: the actual field widget + a remove button beside it.
    // Visible when the field is active, hidden otherwise.
    QWidget* activeRow = new QWidget(optionalContainer);
    QHBoxLayout* activeRowLayout = new QHBoxLayout(activeRow);
    activeRowLayout->setContentsMargins(0, 0, 0, 0);
    activeRowLayout->setSpacing(4);
    activeRowLayout->addWidget(field, 1);

    QPushButton* removeButton = new QPushButton(RemoveGlyph, activeRow);
    removeButton->setObjectName("field-remove-button");
    removeButton->setFixedSize(RemoveButtonSize, RemoveButtonSize);
    activeRowLayout->addWidget(removeButton);

    activeRow->setVisible(alreadySet);
    _fieldActiveRows[memberIndex] = activeRow;
    _optionalFieldActive[memberIndex] = alreadySet;
    containerLayout->addWidget(activeRow);

    // Clicking "+" activates the field and focuses it
    connect(addButton, &QPushButton::clicked, this,
        [this, memberIndex]()
    {
        setOptionalFieldActive(memberIndex, true);
        if (_fieldWidgets[memberIndex]) {
            _fieldWidgets[memberIndex]->setFocus();
        }
        emit optionalFieldToggled(
            QString::fromStdString(_members[memberIndex].name), true
        );
        emit fieldChanged();
    });

    // Clicking the remove button deactivates the field after confirmation
    connect(removeButton, &QPushButton::clicked, this,
        [this, memberIndex]()
    {
        if (hasWidgetContent(_fieldWidgets[memberIndex])) {
            const int result = QMessageBox::question(
                this, "Remove Field",
                QString("The field \"%1\" has content. Remove it?")
                    .arg(splitPascalCase(_members[memberIndex].name)),
                QMessageBox::Yes | QMessageBox::No
            );
            if (result != QMessageBox::Yes) {
                return;
            }
        }
        _properties.erase(_members[memberIndex].name);
        setOptionalFieldActive(memberIndex, false);
        emit optionalFieldToggled(
            QString::fromStdString(_members[memberIndex].name), false
        );
        emit fieldChanged();
    });

    return optionalContainer;
}

QWidget* SchemaFormWidget::widgetForMember(const std::string& name) const {
    auto it = std::find_if(_members.begin(), _members.end(),
        [&](const SchemaMember& member) { return member.name == name; }
    );
    if (it == _members.end() || it->type == "Table") {
        return nullptr;
    }
    const int index = static_cast<int>(std::distance(_members.begin(), it));
    return _fieldWidgets[index];
}

void SchemaFormWidget::applyToProperties() {
    for (size_t i = 0; i < _members.size(); ++i) {
        QWidget* widget = _fieldWidgets[i];
        if (!widget || !_optionalFieldActive[i]) {
            continue;
        }
        const SchemaMember& member = _members[i];

        // Union pages maintain _properties via their own signal handlers;
        // skip to avoid type-detection issues (member.type is the union
        // string, not the active type)
        if (widget->objectName() == UnionContainerName) {
            continue;
        }

        std::optional<PropertyValue> value = readWidgetValue(widget, member.type);
        if (value) {
            _properties[member.name] = std::move(*value);
        }
    }
}

void SchemaFormWidget::setOptionalFieldActive(int memberIndex, bool active) {
    if (!_fieldAddButtons[memberIndex]) {
        return;
    }
    _optionalFieldActive[memberIndex] = active;
    _fieldAddButtons[memberIndex]->setVisible(!active);
    _fieldActiveRows[memberIndex]->setVisible(active);
    if (!active) {
        clearWidget(_fieldWidgets[memberIndex]);
    }
}

void SchemaFormWidget::setFieldActive(const std::string& memberName, bool active) {
    auto it = std::find_if(_members.begin(), _members.end(),
        [&](const SchemaMember& member) { return member.name == memberName; }
    );
    if (it == _members.end()) {
        return;
    }

    const int index = static_cast<int>(std::distance(_members.begin(), it));

    // Only optional fields have add/remove buttons
    if (!_fieldAddButtons[index]) {
        return;
    }

    // Already in the requested state — nothing to do
    if (active == _optionalFieldActive[index]) {
        return;
    }

    // When deactivating, remove the property so it won't be serialized
    if (!active) {
        _properties.erase(memberName);
    }
    setOptionalFieldActive(index, active);
}

void SchemaFormWidget::syncFieldWith(
    const std::string& memberName, SchemaFormWidget* other)
{
    // Cross-connect text edits so typing in one form updates the other.
    // setText() emits textChanged but NOT textEdited, so the cross-connections
    // cannot loop. Signals are left unblocked so that other textChanged
    // listeners (e.g. identifier auto-generation) still fire.
    QLineEdit* thisEdit = resolveLineEdit(widgetForMember(memberName));
    QLineEdit* otherEdit = resolveLineEdit(other->widgetForMember(memberName));

    if (thisEdit && otherEdit) {
        connect(thisEdit, &QLineEdit::textEdited, otherEdit, &QLineEdit::setText);
        connect(otherEdit, &QLineEdit::textEdited, thisEdit, &QLineEdit::setText);
    }

    // Cross-connect optional field toggle state so adding/removing the field
    // in one form mirrors the change in the other
    const QString targetName = QString::fromStdString(memberName);

    auto syncToggle = [memberName, targetName](
        SchemaFormWidget* sender, SchemaFormWidget* receiver)
    {
        connect(sender, &SchemaFormWidget::optionalFieldToggled, receiver,
            [receiver, memberName, targetName](const QString& name, bool active)
        {
            if (name == targetName) {
                receiver->setFieldActive(memberName, active);
            }
        });
    };
    syncToggle(this, other);
    syncToggle(other, this);
}

void SchemaFormWidget::populateFromProperties() {
    for (size_t i = 0; i < _members.size(); ++i) {
        QWidget* widget = _fieldWidgets[i];
        if (!widget) {
            continue;
        }
        const SchemaMember& member = _members[i];

        // For optional fields, show or hide the active row based on whether
        // the property exists. Skip if the property is absent or null.
        if (_fieldAddButtons[i]) {
            const bool exists =
                _properties.count(member.name) > 0 &&
                !_properties.at(member.name).isNull();
            setOptionalFieldActive(static_cast<int>(i), exists);
            if (!exists) {
                continue;
            }
        }

        // Look up the stored value; skip if there is no property for this member
        const auto it = _properties.find(member.name);
        if (it == _properties.end()) {
            continue;
        }
        const PropertyValue& value = it->second;

        // Union widgets need special handling to detect the stored type
        // and switch the combo to the matching page before populating
        if (widget->objectName() == UnionContainerName) {
            populateUnionWidget(widget, value);
            continue;
        }

        // Regular flat widget — set the display value directly
        populateWidget(widget, value);
    }
}

void SchemaFormWidget::buildUi() {
    QVBoxLayout* outer = new QVBoxLayout(this);
    outer->setContentsMargins(0, 0, 0, 0);
    outer->setSpacing(4);

    QWidget* gridWidget = new QWidget(this);
    QGridLayout* grid = new QGridLayout(gridWidget);
    grid->setContentsMargins(0, 0, 0, 0);
    grid->setColumnMinimumWidth(0, LabelColumnMinWidth);
    grid->setColumnStretch(1, 1);
    grid->setHorizontalSpacing(ColumnSpacing);
    grid->setVerticalSpacing(RowSpacing);

    // Build display order: sorted or original insertion order
    const int nMembers = static_cast<int>(_members.size());
    std::vector<int> displayOrder(nMembers);
    for (int i = 0; i < nMembers; ++i) {
        displayOrder[i] = i;
    }

    if (_sortMembers) {
        // Order: 1) Flat before Table 2) required before optional 3) alphabetical
        std::sort(displayOrder.begin(), displayOrder.end(),
            [this](int a, int b) {
                const bool aTable = (_members[a].type == "Table");
                const bool bTable = (_members[b].type == "Table");

                if (aTable != bTable) {
                    return !aTable;
                }

                const bool aOpt = _members[a].isOptional;
                const bool bOpt = _members[b].isOptional;

                if (aOpt != bOpt) {
                    return !aOpt;
                }

                return _members[a].name < _members[b].name;
            }
        );
    }

    for (int row = 0; row < nMembers; ++row) {
        addMemberToGrid(
            grid, row, displayOrder[row], _members[displayOrder[row]]
        );
    }

    outer->addWidget(gridWidget);
}
