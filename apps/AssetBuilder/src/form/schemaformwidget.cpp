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

#include "documentation.h"
#include "form/collapsiblesection.h"
#include "form/colorwidget.h"
#include "form/matrixwidget.h"
#include "form/searchdropdown.h"
#include "identifierregistry.h"
#include "schema/assetschema.h"
#include "utils.h"
#include <QCheckBox>
#include <QComboBox>
#include <QDateTime>
#include <QDoubleValidator>
#include <QFile>
#include <QFileDialog>
#include <QFrame>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QIntValidator>
#include <QJsonDocument>
#include <QJsonObject>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QRegularExpression>
#include <QRegularExpressionValidator>
#include <QVBoxLayout>
#include <algorithm>
#include <functional>
#include <limits>
#include <optional>
#include <string>
#include <vector>

namespace {
    constexpr int LabelColumnMinWidth = 130;
    constexpr int ColumnSpacing = 8;
    constexpr int RowSpacing = 4;
    constexpr int InfoButtonSize = 16;
    constexpr int CardMarginHorizontal = 8;
    constexpr int CardMarginVertical = 6;
    constexpr const char* DateDisplayFormat = "yyyy-MM-dd HH:mm:ss";

    // Object names used as type tags for widget identification.
    constexpr const char* FileContainerName = "file-container";
    constexpr const char* UnionContainerName = "union-container";
    constexpr const char* UnionTypeComboName = "union-type-combo";
    constexpr const char* DateEditName = "date-edit";

    enum class TableKind {
        RefArray,     // e.g. Layers = [{ Type: "...", ... }, ...]
        Ref,          // e.g. Renderable = { Type: "...", ... }
        ScalarArray,  // e.g. Tags = ["tag1", "tag2"]
        InlineArray,  // e.g. Instruments = [{ Name: "...", ... }, ...]
        Table         // e.g. GUI = { Name: "...", Path: "..." }
    };

    struct TableClassification {
        TableKind kind;
        const SchemaReference* reference = nullptr;
    };

    constexpr MatrixTypeEntry MatrixTypeLookup[] = {
    { "Vector2<double>",    2,  2, false, false },
    { "Vector2<int>",       2,  2, true,  false },
    { "Vector3<double>",    3,  3, false, false },
    { "Vector3<int>",       3,  3, true,  false },
    { "Vector4<double>",    4,  4, false, false },
    { "Vector4<int>",       4,  4, true,  false },
    { "Color3",             3,  3, false, true  },
    { "Color4",             4,  4, false, true  },
    { "Matrix3x3<double>",  9,  3, false, false },
    { "Matrix4x4<double>", 16,  4, false, false }
    };

    struct NumericRange {
        double min = -std::numeric_limits<double>::max();
        double max = std::numeric_limits<double>::max();
        bool hasMin = false;
        bool hasMax = false;
    };

    bool hasWidgetContent(QWidget* widget) {
        if (auto* checkBox = qobject_cast<QCheckBox*>(widget);  checkBox) {
            return checkBox->isChecked();
        }
        if (auto* matrix = qobject_cast<MatrixWidget*>(widget);  matrix) {
            return matrix->hasContent();
        }
        if (auto* lineEdit = qobject_cast<QLineEdit*>(widget);  lineEdit) {
            return !lineEdit->text().isEmpty();
        }
        if (widget->objectName() == FileContainerName) {
            auto* lineEdit = widget->findChild<QLineEdit*>();
            return lineEdit && !lineEdit->text().isEmpty();
        }
        if (auto* combo = widget->findChild<QComboBox*>();  combo) {
            return !combo->currentText().isEmpty();
        }
        return false;
    }

    void clearWidget(QWidget* widget) {
        if (auto* checkBox = qobject_cast<QCheckBox*>(widget);  checkBox) {
            checkBox->blockSignals(true);
            checkBox->setChecked(false);
            checkBox->blockSignals(false);
        }
        else if (auto* matrix = qobject_cast<MatrixWidget*>(widget);  matrix) {
            matrix->blockSignals(true);
            matrix->clear();
            matrix->blockSignals(false);
        }
        else if (auto* lineEdit = qobject_cast<QLineEdit*>(widget);  lineEdit) {
            lineEdit->blockSignals(true);
            lineEdit->clear();
            lineEdit->blockSignals(false);
        }
        else if (widget->objectName() == FileContainerName) {
            auto* lineEditFile = widget->findChild<QLineEdit*>();
            if (lineEditFile) {
                lineEditFile->blockSignals(true);
                lineEditFile->clear();
                lineEditFile->blockSignals(false);
            }
        }
        else if (auto* combo = widget->findChild<QComboBox*>();  combo) {
            combo->blockSignals(true);
            combo->setCurrentText("");
            combo->blockSignals(false);
        }
    }

    // Reads the current value from a field widget and returns it as a PropertyValue. The
    // member type is needed to distinguish numeric/date/string line edits. Returns
    // std::nullopt if the widget is empty or unrecognized
    std::optional<PropertyValue> readWidgetValue(QWidget* widget, const std::string& type)
    {
        if (auto* checkBox = qobject_cast<QCheckBox*>(widget);  checkBox) {
            return PropertyValue{ checkBox->isChecked() };
        }
        if (auto* matrix = qobject_cast<MatrixWidget*>(widget);  matrix) {
            return PropertyValue{ matrix->values() };
        }
        if (auto* lineEdit = qobject_cast<QLineEdit*>(widget);  lineEdit) {
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
                const QDateTime dateTime = QDateTime::fromString(text, DateDisplayFormat);
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
        if (auto* combo = widget->findChild<QComboBox*>();  combo) {
            const QString text = combo->currentText();
            if (!text.isEmpty()) {
                return PropertyValue{ text.toStdString() };
            }
        }
        return std::nullopt;
    }

    void populateWidget(QWidget* widget, const PropertyValue& propertyValue) {
        if (auto* checkBox = qobject_cast<QCheckBox*>(widget);  checkBox) {
            checkBox->blockSignals(true);
            checkBox->setChecked(
                propertyValue.isBool() ? propertyValue.toBool() : false
            );
            checkBox->blockSignals(false);
        }
        else if (auto* matrix = qobject_cast<MatrixWidget*>(widget);  matrix) {
            matrix->blockSignals(true);
            if (propertyValue.isList()) {
                matrix->setValues(propertyValue.toList());
            }
            matrix->blockSignals(false);
        }
        else if (auto* lineEdit = qobject_cast<QLineEdit*>(widget);  lineEdit) {
            lineEdit->blockSignals(true);
            if (propertyValue.isDouble()) {
                lineEdit->setText(
                    QString::number(propertyValue.toDouble(), 'g', 10)
                );
            }
            else if (propertyValue.isString()) {
                QString value = QString::fromStdString(propertyValue.toString());
                // Convert ISO date to display format for date fields only (avoid mangling
                // plain strings)
                if (lineEdit->objectName() == DateEditName) {
                    const QDateTime dateTime = QDateTime::fromString(value, Qt::ISODate);
                    if (dateTime.isValid()) {
                        value = dateTime.toLocalTime().toString(DateDisplayFormat);
                    }
                }
                lineEdit->setText(value);
            }
            lineEdit->blockSignals(false);
        }
        else if (widget->objectName() == FileContainerName) {
            if (auto* fileEdit = widget->findChild<QLineEdit*>();  fileEdit) {
                if (propertyValue.isString()) {
                    fileEdit->blockSignals(true);
                    fileEdit->setText(QString::fromStdString(propertyValue.toString()));
                    fileEdit->blockSignals(false);
                }
            }
        }
        else if (auto* combo = widget->findChild<QComboBox*>()) {
            if (propertyValue.isString()) {
                combo->blockSignals(true);
                combo->setCurrentText(QString::fromStdString(propertyValue.toString()));
                combo->blockSignals(false);
            }
        }
    }

    // Resolves the editable QLineEdit from a member widget. The widget may be a QLineEdit
    // directly, or a QComboBox wrapping one (e.g. Identifier)
    QLineEdit* resolveLineEdit(QWidget* widget) {
        if (!widget) {
            return nullptr;
        }

        if (auto* lineEdit = qobject_cast<QLineEdit*>(widget);  lineEdit) {
            return lineEdit;
        }
        if (auto* combo = widget->findChild<QComboBox*>();  combo) {
            return combo->lineEdit();
        }
        return nullptr;
    }

    // A union field stores only the raw property value (no type tag), so we need to
    // deduce which type it is by inspecting the value and matching it against the entries
    // in the union's type combo box.
    //
    // The combo entries correspond to schema types like "Double", "String",
    // "Vector3<double>", etc. We check the value's runtime type (isDouble, isString,
    // isList, isBool) and search the combo for a matching entry. Some runtime types map
    // to multiple possible schema types (e.g. a string could be "String", "File", or
    // "Date and time"), so we try candidates in order and use the first one present in
    // the combo.
    //
    // matchIndex tracks the result: the combo index to activate. If no match is found
    // (matchIndex stays < 0), we fall back to index 0 (the first entry). Setting the
    // combo index triggers the show/hide lambda from createUnionWidget, which makes the
    // correct field widget visible
    void populateUnionWidget(QWidget* container, const PropertyValue& value) {
        auto* typeCombo = container->findChild<QComboBox*>(UnionTypeComboName);
        if (!typeCombo) {
            return;
        }

        int matchIndex = 0;
        if (value.isDouble()) {
            for (const QString& name : { "Double", "Integer" }) {
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
            for (int i = 0; i < typeCombo->count(); i++) {
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
            for (const QString& name : { "String", "Date and time", "File",
                                         "Directory", "Identifier" })
            {
                matchIndex = typeCombo->findText(name);
                if (matchIndex != -1) {
                    break;
                }
            }
        }
        matchIndex = std::max(matchIndex, 0);

        // setCurrentIndex triggers the show/hide lambda
        typeCombo->setCurrentIndex(matchIndex);

        // Populate the now-visible field widget
        const auto children = container->findChildren<QWidget*>(
            QString(),
            Qt::FindDirectChildrenOnly
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

    // Extracts numeric bounds from a schema description string. Supports "In range",
    // "Greater or equal to", "Greater than", "Less or equal to", and "Less than" patterns
    NumericRange parseRange(const std::string& description) {
        NumericRange range;
        const QString descriptionText = QString::fromStdString(description);

        // "In range: ( min, max )"
        static const QRegularExpression reRange(
            R"(In range:\s*\(\s*([^,]+),\s*([^)]+)\))"
        );
        QRegularExpressionMatch match = reRange.match(descriptionText);
        if (match.hasMatch()) {
            bool okMin = false;
            bool okMax = false;
            const double lower = match.captured(1).trimmed().toDouble(&okMin);
            const double upper = match.captured(2).trimmed().toDouble(&okMax);
            if (okMin) {
                range.min = lower;
                range.hasMin = true;
            }
            if (okMax) {
                range.max = upper;
                range.hasMax = true;
            }
            return range;
        }

        // "Greater or equal to: N"
        static const QRegularExpression reGe(R"(Greater or equal to:\s*([-\d.eE+]+))");
        match = reGe.match(descriptionText);
        if (match.hasMatch()) {
            bool ok = false;
            const double value = match.captured(1).toDouble(&ok);
            if (ok) {
                range.min = value;
                range.hasMin = true;
            }
        }

        // "Greater than: N"
        static const QRegularExpression reGt(R"(Greater than:\s*([-\d.eE+]+))");
        match = reGt.match(descriptionText);
        if (match.hasMatch()) {
            bool ok = false;
            const double value = match.captured(1).toDouble(&ok);
            if (ok) {
                range.min = value;
                range.hasMin = true;
            }
        }

        // "Less or equal to: N"
        static const QRegularExpression reLe(R"(Less or equal to:\s*([-\d.eE+]+))");
        match = reLe.match(descriptionText);
        if (match.hasMatch()) {
            bool ok = false;
            const double value = match.captured(1).toDouble(&ok);
            if (ok) {
                range.max = value;
                range.hasMax = true;
            }
        }

        // "Less than: N"
        static const QRegularExpression reLt(R"(Less than:\s*([-\d.eE+]+))");
        match = reLt.match(descriptionText);
        if (match.hasMatch()) {
            bool ok = false;
            const double value = match.captured(1).toDouble(&ok);
            if (ok) {
                range.max = value;
                range.hasMax = true;
            }
        }

        return range;
    }

    // Builds a placeholder string describing the valid range for a numeric field. Returns
    // e.g. "1 to 10", ">= 0", "<= 100", or a default "0"/"0.0"
    QString rangePlaceholder(const NumericRange& range, bool isInteger) {
        auto format = [isInteger](double value) {
            if (isInteger) {
                return QString::number(static_cast<int>(value));
            }
            QString formatted = QString::number(value, 'g', 10);
            if (!formatted.contains('.')) {
                formatted += ".0";
            }
            return formatted;
            };

        if (range.hasMin && range.hasMax) {
            return format(range.min) + " to " + format(range.max);
        }
        if (range.hasMin) {
            return ">= " + format(range.min);
        }
        if (range.hasMax) {
            return "<= " + format(range.max);
        }
        return isInteger ? "0" : "0.0";
    }

    // Extracts the allowed values from an "In list { ... }" description pattern. Returns
    // the values as a trimmed string list, or empty if no match
    QStringList parseInList(const std::string& description) {
        const QString descriptionText = QString::fromStdString(description);
        static const QRegularExpression re(R"(In list \{\s*(.+?)\s*\})");
        QRegularExpressionMatch match = re.match(descriptionText);
        if (!match.hasMatch()) {
            return QStringList();
        }
        QStringList items = match.captured(1).split(",");
        for (QString& item : items) {
            item = item.trimmed();
        }
        items.removeAll("");
        return items;
    }

    // Splits a union type string like "Integer, or Double" into individual types. Handles
    // both ", or " and " or " separators
    QStringList splitUnionTypes(const QString& type) {
        QString cleaned = type;
        cleaned.replace(", or ", ", ");
        cleaned.replace(" or ", ", ");
        QStringList parts = cleaned.split(", ", Qt::SkipEmptyParts);
        for (QString& part : parts) {
            part = part.trimmed();
        }
        return parts;
    }

    // Determines the table kind (ref array, ref, scalar array, inline array, or plain
    // table) based on the member's structure and references
    TableClassification classifyTableMember(const SchemaMember& member) {
        // An array member has exactly one child named "*", which serves as the schema
        // template for each element in the array
        const bool isArray = !member.reference.has_value() &&
            member.members.size() == 1 && member.members[0].name == "*";
        const std::optional<SchemaReference>& optionalReference =
            isArray ? member.members[0].reference : member.reference;

        if (optionalReference.has_value() && optionalReference->isFound) {
            return {
                isArray ? TableKind::RefArray : TableKind::Ref,
                &optionalReference.value()
            };
        }
        if (isArray) {
            return {
                member.members[0].members.empty()
                    ? TableKind::ScalarArray : TableKind::InlineArray,
                nullptr
            };
        }
        return { TableKind::Table, nullptr };
    }

    // Removes and deletes all widgets and layout items from a layout
    void clearLayout(QLayout* layout) {
        while (layout->count() > 0) {
            QLayoutItem* layoutItem = layout->takeAt(0);
            if (layoutItem->widget()) {
                delete layoutItem->widget();
            }
            delete layoutItem;
        }
    }

    // Looks up a schema type by display name within a category
    const SchemaType* findTypeByName(const SchemaCategory* category,
        const QString& typeName)
    {
        if (!category) {
            return nullptr;
        }
        for (const SchemaType& schemaType : category->types) {
            if (QString::fromStdString(schemaType.name) == typeName) {
                return &schemaType;
            }
        }
        return nullptr;
    }

    // Extracts the "Type" value from a property map, or empty if absent
    QString storedTypeFromMap(const PropertyMap& properties) {
        const auto it = properties.find("Type");
        if (it != properties.end() && it->second.isString()) {
            return QString::fromStdString(it->second.toString());
        }
        return {};
    }

    struct ArrayScaffold {
        QVBoxLayout* itemsLayout;
        QPushButton* addButton;
    };

    // Creates the shared container layout with an items area and "+ Add" button for
    // array sections
    ArrayScaffold createArrayScaffold(CollapsibleSection* section) {
        // Outer container for the whole array section
        QWidget* container = new QWidget();
        QBoxLayout* containerLayout = new QVBoxLayout(container);
        containerLayout->setContentsMargins(0, 4, 0, 4);
        containerLayout->setSpacing(4);

        // Inner container where individual item cards are added
        QWidget* itemsContainer = new QWidget();
        QVBoxLayout* itemsLayout = new QVBoxLayout(itemsContainer);
        itemsLayout->setContentsMargins(0, 0, 0, 0);
        itemsLayout->setSpacing(4);

        // Button to append a new item to the array
        QPushButton* addButton = new QPushButton("+ Add");
        addButton->setObjectName("array-add-button");

        // Assemble and set as the section content
        containerLayout->addWidget(itemsContainer);
        containerLayout->addWidget(addButton);
        section->setContentWidget(container);

        return { itemsLayout, addButton };
    }

    struct CardFrame {
        QFrame* frame;
        QBoxLayout* layout;
    };

    // Creates a styled card frame with vertical layout for an array item
    CardFrame createCardFrame() {
        QFrame* card = new QFrame();
        card->setObjectName("array-item-frame");
        card->setFrameShape(QFrame::StyledPanel);

        QBoxLayout* cardLayout = new QVBoxLayout(card);

        cardLayout->setContentsMargins(
            CardMarginHorizontal,
            CardMarginVertical,
            CardMarginHorizontal,
            CardMarginVertical
        );
        cardLayout->setSpacing(4);
        return { card, cardLayout };
    }

    // The ensure* functions guarantee that a PropertyMap or PropertyList exists at the
    // given key before a nested form or array binds to it. Without them, accessing an
    // unset key would produce a null PropertyValue, and calling toMap()/toList() on it
    // would fail

    // Ensures the item at the given index is a map, replacing it if needed
    PropertyMap& ensureListItemMap(PropertyList& items, size_t index) {
        if (index >= items.size()) {
            qWarning("ensureListItemMap: index %llu out of range (size %llu)",
                     index, items.size());
            static PropertyMap empty;
            empty = PropertyMap{};
            return empty;
        }
        if (!items[index].isMap()) {
            items[index] = PropertyValue{ PropertyMap{} };
        }
        return items[index].toMap();
    }

    // Ensures a property map exists at the given key, creating one if absent
    PropertyMap& ensurePropertyMap(PropertyMap& properties, const std::string& key) {
        if (properties.count(key) == 0 || properties.at(key).isNull()) {
            properties[key] = PropertyValue{ PropertyMap{} };
        }
        if (!properties[key].isMap()) {
            qWarning("ensurePropertyMap: key '%s' is not a map, replacing",
                     key.c_str());
            properties[key] = PropertyValue{ PropertyMap{} };
        }
        return properties[key].toMap();
    }

    // Ensures a property list exists at the given key. Creates one if absent, or wraps a
    // single map value in a one-element list for uniformity
    void ensurePropertyList(PropertyMap& properties, const std::string& key) {
        if (properties.count(key) == 0 || properties.at(key).isNull()) {
            properties[key] = PropertyValue{ PropertyList{} };
        }
        else if (properties.at(key).isMap()) {
            // A single object was stored instead of a list - wrap it in a one-element
            // list so downstream code can treat it uniformly as an array
            PropertyMap existing = std::move(properties[key].toMap());
            PropertyList wrapped;
            wrapped.push_back(PropertyValue{ std::move(existing) });
            properties[key] = PropertyValue{ std::move(wrapped) };
        }
        else {
            // Should already be a list if it exists and isn't null or a map
            if (!properties.at(key).isList()) {
                qWarning("ensurePropertyList: key '%s' is not a list, replacing",
                         key.c_str());
                properties[key] = PropertyValue{ PropertyList{} };
            }
        }
    }
    QPushButton* createInfoButton(const SchemaMember& member, QWidget* parent,
                                  const std::function<void(const Documentation&)>& onInfo)
    {
        const Documentation info = {
            .name = QString::fromStdString(member.name),
            .type = QString::fromStdString(member.type),
            .isOptional = member.isOptional,
            .description = QString::fromStdString(member.description),
            .documentation = QString::fromStdString(member.documentation)
        };

        QPushButton* button = new QPushButton("i", parent);
        button->setObjectName("field-info-button");
        button->setFixedSize(InfoButtonSize, InfoButtonSize);

        QObject::connect(
            button, &QPushButton::clicked,
            button, [info, onInfo]() { onInfo(info); }
        );
        return button;
    }

    QWidget* createBooleanWidget(const std::string& name, PropertyMap& properties,
                                 const std::function<void()>& onChange)
    {
        PropertyMap* props = &properties;
        QCheckBox* checkBox = new QCheckBox();

        QObject::connect(
            checkBox,
            &QCheckBox::toggled,
            checkBox,
            [props, name, onChange](bool checked) {
                (*props)[name] = PropertyValue{ checked };
                onChange();
            }
        );
        return checkBox;
    }

    QWidget* createIntegerWidget(const std::string& name, const std::string& description,
                                 PropertyMap& properties,
                                 const std::function<void()>& onChange)
    {
        PropertyMap* props = &properties;
        QLineEdit* lineEdit = new QLineEdit();
        const NumericRange range = parseRange(description);
        // Cannot cast the NumericRange double defaults to int (overflow / UB), so fall
        // back to int limits when no bound was parsed
        const int lowerBound =
            range.hasMin ? static_cast<int>(range.min) : std::numeric_limits<int>::min();
        const int upperBound =
            range.hasMax ? static_cast<int>(range.max) : std::numeric_limits<int>::max();
        lineEdit->setValidator(new QIntValidator(lowerBound, upperBound, lineEdit));
        lineEdit->setPlaceholderText(rangePlaceholder(range, true));

        QObject::connect(
            lineEdit,
            &QLineEdit::textEdited,
            lineEdit,
            [props, name, onChange](const QString& text) {
                bool ok = false;
                const int value = text.toInt(&ok);
                (*props)[name] = PropertyValue{
                    static_cast<double>(ok ? value : 0)
                };
                onChange();
            }
        );
        return lineEdit;
    }

    QWidget* createDoubleWidget(const std::string& name, const std::string& description,
                                PropertyMap& properties,
                                const std::function<void()>& onChange)
    {
        PropertyMap* props = &properties;
        QLineEdit* lineEdit = new QLineEdit();
        const NumericRange range = parseRange(description);
        // QDoubleValidator misbehaves with extreme double values, so use +/- 1e15 as a
        // practical fallback when no bound was parsed
        const double lowerBound = range.hasMin ? range.min : -1e15;
        const double upperBound = range.hasMax ? range.max : 1e15;
        lineEdit->setValidator(
            new QDoubleValidator(lowerBound, upperBound, -1, lineEdit)
        );
        lineEdit->setPlaceholderText(rangePlaceholder(range, false));
        QObject::connect(
            lineEdit,
            &QLineEdit::textEdited,
            lineEdit,
            [props, name, onChange](const QString& text) {
                bool ok = false;
                const double value = text.toDouble(&ok);
                (*props)[name] = PropertyValue{ ok ? value : 0.0 };
                onChange();
            }
        );
        return lineEdit;
    }

    QWidget* createMatrixWidget(const std::string& name, const MatrixTypeEntry& entry,
                                PropertyMap& properties,
                                const std::function<void()>& onChange)
    {
        PropertyMap* props = &properties;
        MatrixWidget* matrixWidget = entry.isColor ?
            static_cast<MatrixWidget*>(new ColorWidget(entry.nComponents)) :
            new MatrixWidget(entry.nComponents, entry.nColumns, entry.isInteger);
        QObject::connect(
            matrixWidget,
            &MatrixWidget::valueChanged,
            matrixWidget,
            [props, name, matrixWidget, onChange]() {
                (*props)[name] = PropertyValue{ matrixWidget->values() };
                onChange();
            }
        );
        return matrixWidget;
    }

    QWidget* createFileWidget(const std::string& name, const std::string& description,
                              bool isDirectory, PropertyMap& properties,
                              const std::function<void()>& onChange)
    {
        PropertyMap* props = &properties;
        QWidget* container = new QWidget();
        container->setObjectName(FileContainerName);
        QBoxLayout* horizontalLayout = new QHBoxLayout(container);
        horizontalLayout->setContentsMargins(0, 0, 0, 0);
        horizontalLayout->setSpacing(4);

        QLineEdit* lineEdit = new QLineEdit(container);
        if (!description.empty()) {
            lineEdit->setPlaceholderText(QString::fromStdString(description));
        }
        else {
            lineEdit->setPlaceholderText(
                isDirectory ? "Path to directory..." : "Path to file..."
            );
        }
        QObject::connect(
            lineEdit,
            &QLineEdit::textEdited,
            lineEdit,
            [props, name, onChange](const QString& text) {
                (*props)[name] = PropertyValue{ text.toStdString() };
                onChange();
            }
        );

        QPushButton* browseButton = new QPushButton("Browse...", container);
        browseButton->setObjectName("file-browse-button");
        QObject::connect(
            browseButton,
            &QPushButton::clicked,
            container,
            [container, lineEdit, props, name, isDirectory, onChange]() {
                const QString path = isDirectory ?
                    QFileDialog::getExistingDirectory(container, "Select Directory") :
                    QFileDialog::getOpenFileName(container, "Select File");
                if (!path.isEmpty()) {
                    lineEdit->setText(path);
                    (*props)[name] = PropertyValue{ path.toStdString() };
                    onChange();
                }
            }
        );

        horizontalLayout->addWidget(lineEdit, 1);
        horizontalLayout->addWidget(browseButton);
        return container;
    }

    QWidget* createDateTimeWidget(const std::string& name, PropertyMap& properties,
                                  const std::function<void()>& onChange)
    {
        PropertyMap* props = &properties;
        QLineEdit* lineEdit = new QLineEdit();
        lineEdit->setObjectName(DateEditName);
        // Human-readable hint; the actual Qt format string is DateDisplayFormat
        lineEdit->setPlaceholderText("YYYY-MM-DD hh:mm:ss");
        lineEdit->setValidator(new QRegularExpressionValidator(
            QRegularExpression(R"(^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}$)"),
            lineEdit
        ));
        QObject::connect(
            lineEdit,
            &QLineEdit::textEdited,
            lineEdit,
            [props, name, onChange](const QString& text) {
                const QDateTime dateTime = QDateTime::fromString(text, DateDisplayFormat);
                if (dateTime.isValid()) {
                    (*props)[name] = PropertyValue{
                        dateTime.toUTC().toString(Qt::ISODate).toStdString()
                    };
                }
                onChange();
            }
        );
        return lineEdit;
    }

    QStringList extractJassetIdentifiers(const QString& filePath) {
        QFile file(filePath);
        if (!file.open(QFile::ReadOnly)) {
            return {};
        }
        QJsonParseError err;
        const QJsonDocument doc = QJsonDocument::fromJson(file.readAll(), &err);
        if (doc.isNull() || !doc.isObject()) {
            return {};
        }
        const JAsset asset = jassetFromJson(doc.object());
        QStringList ids;
        for (const ContentItem& item : asset.contents) {
            const auto it = item.properties.find("Identifier");
            if (it != item.properties.end() && it->second.isString()) {
                ids.append(QString::fromStdString(it->second.toString()));
            }
        }
        return ids;
    }

    QWidget* createIdentifierComboWidget(const SchemaMember& member,
                                         IdentifierRegistry* registry,
                                         PropertyMap& properties,
                                         const std::function<void()>& onChange,
                                    const std::function<void(const QString&)>& onSelect)
    {
        PropertyMap* props = &properties;
        QWidget* container = new QWidget();
        QBoxLayout* horizontalLayout = new QHBoxLayout(container);
        horizontalLayout->setContentsMargins(0, 0, 0, 0);
        horizontalLayout->setSpacing(4);

        // Returns identifiers excluding this node's own
        auto filteredIdentifiers = [registry, props]() {
            QStringList identifiers = registry->knownIdentifiers();
            const auto it = props->find("Identifier");
            if (it != props->end() && it->second.isString()) {
                identifiers.removeAll(QString::fromStdString(it->second.toString()));
            }
            return identifiers;
            };

        QComboBox* combo = new QComboBox(container);
        combo->setEditable(true);
        combo->addItems(filteredIdentifiers());
        if (!member.description.empty()) {
            combo->lineEdit()->setPlaceholderText(
                QString::fromStdString(member.description)
            );
        }

        QObject::connect(
            registry,
            &IdentifierRegistry::registryChanged,
            combo,
            [combo, filteredIdentifiers]() {
                const QString current = combo->currentText();
                combo->blockSignals(true);
                combo->clear();
                combo->addItems(filteredIdentifiers());
                combo->setCurrentText(current);
                combo->blockSignals(false);
            }
        );
        QObject::connect(
            combo,
            &QComboBox::currentTextChanged,
            combo,
            [props, name = member.name, onChange](const QString& text) {
                (*props)[name] = PropertyValue{ text.toStdString() };
                onChange();
            }
        );

        QPushButton* browseButton = new QPushButton("Browse .jasset", container);
        browseButton->setObjectName("identifier-browse-button");
        QObject::connect(
            browseButton, &QPushButton::clicked,
            container,
            [combo, container, onSelect]() {
                const QString selected = QFileDialog::getOpenFileName(
                    container, "Browse .jasset", QDir::homePath(),
                    "Asset files (*.jasset);"
                );
                if (selected.isEmpty()) {
                    return;
                }

                QStringList identifiers = extractJassetIdentifiers(selected);

                if (identifiers.isEmpty()) {
                    QMessageBox::information(
                        container, "No Identifiers",
                        "No identifiers were found in the selected file."
                    );
                    return;
                }

                bool ok = false;
                const QString picked = QInputDialog::getItem(
                    container, "Select Identifier",
                    "Select an identifier from the file:",
                    identifiers, 0, false, &ok
                );
                if (!ok) {
                    return;
                }

                combo->setCurrentText(picked);
                onSelect(selected);
            }
        );

        horizontalLayout->addWidget(combo, 1);
        horizontalLayout->addWidget(browseButton);
        return container;
    }

    QWidget* createInListWidget(const std::string& name, const QStringList& listOptions,
                                PropertyMap& properties,
                                const std::function<void()>& onChange)
    {
        PropertyMap* props = &properties;
        QComboBox* combo = new QComboBox();
        combo->addItem("Select...");
        combo->addItems(listOptions);
        combo->setCurrentIndex(0);
        QObject::connect(
            combo,
            &QComboBox::currentIndexChanged,
            combo,
            [props, name, combo, onChange](int index) {
                // Index 0 is the "Select..." placeholder - erase the property so the
                // placeholder choice is not serialized into the asset
                if (index <= 0) {
                    props->erase(name);
                }
                else {
                    (*props)[name] = PropertyValue{ combo->currentText().toStdString() };
                }
                onChange();
            }
        );
        return combo;
    }

    QWidget* createStringWidget(const std::string& name, const std::string& description,
                                PropertyMap& properties,
                                const std::function<void()>& onChange)
    {
        PropertyMap* props = &properties;
        QLineEdit* lineEdit = new QLineEdit();

        if (!description.empty()) {
            lineEdit->setPlaceholderText(QString::fromStdString(description));
        }

        QObject::connect(
            lineEdit,
            &QLineEdit::textEdited,
            lineEdit,
            [props, name, onChange](const QString& text) {
                (*props)[name] = PropertyValue{ text.toStdString() };
                onChange();
            }
        );

        return lineEdit;
    }
} // namespace

SchemaFormWidget::SchemaFormWidget(std::vector<SchemaMember> members,
                                   PropertyMap& properties, QWidget* parent,
                                   bool subSectionsExpanded, bool sortMembers,
                                   IdentifierRegistry* registry)
    : QWidget(parent)
    , _members(std::move(members))
    , _properties(properties)
    , _areSubSectionsExpanded(subSectionsExpanded)
    , _shouldSortMembers(sortMembers)
    , _registry(registry)
{
    _fieldWidgets.resize(_members.size(), nullptr);
    _fieldAddButtons.resize(_members.size(), nullptr);
    _fieldActiveRows.resize(_members.size(), nullptr);
    _optionalFieldActive.resize(_members.size(), true);

    buildUi();
}

void SchemaFormWidget::addMemberToGrid(QGridLayout* grid, int row, int memberIndex,
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
        labelContainer->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);

        QBoxLayout* labelLayout = new QHBoxLayout(labelContainer);
        labelLayout->setContentsMargins(0, 0, 0, 0);
        labelLayout->setSpacing(4);

        auto onInfo = [this](const Documentation& info) {
            emit documentationRequested(info);
        };
        labelLayout->addWidget(createInfoButton(member, labelContainer, onInfo));

        QLabel* nameLabel = new QLabel(splitPascalCase(member.name), labelContainer);
        nameLabel->setObjectName("field-label");
        labelLayout->addWidget(nameLabel);

        if (!member.isOptional) {
            QLabel* asterisk = new QLabel("*", labelContainer);
            asterisk->setObjectName("field-required-asterisk");
            labelLayout->addWidget(asterisk);
        }
        labelLayout->addStretch();
        grid->addWidget(labelContainer, row, 0, Qt::AlignVCenter | Qt::AlignLeft);
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

QWidget* SchemaFormWidget::createOptionalWrapper(int memberIndex, QWidget* field) {
    const std::string& name = _members[memberIndex].name;
    const bool alreadySet = _properties.count(name) > 0;

    // Container holds two mutually-exclusive rows: a "+" button (when the
    // field is inactive) and the field + remove button (when active).
    // Both are created upfront and toggled via setOptionalFieldActive.
    QWidget* optionalContainer = new QWidget(this);
    QBoxLayout* containerLayout = new QVBoxLayout(optionalContainer);
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
    QBoxLayout* activeRowLayout = new QHBoxLayout(activeRow);
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
    connect(
        addButton,
        &QPushButton::clicked,
        this,
        [this, memberIndex]() {
            setOptionalFieldActive(memberIndex, true);
            if (_fieldWidgets[memberIndex]) {
                _fieldWidgets[memberIndex]->setFocus();
            }
            emit optionalFieldToggled(
                QString::fromStdString(_members[memberIndex].name), true
            );
            emit fieldChanged();
        }
    );

    // Clicking the remove button deactivates the field after confirmation
    connect(
        removeButton,
        &QPushButton::clicked,
        this,
        [this, memberIndex]() {
            if (hasWidgetContent(_fieldWidgets[memberIndex])) {
                const int result = QMessageBox::question(
                    this,
                    "Remove Field",
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
                QString::fromStdString(_members[memberIndex].name),
                false
            );
            emit fieldChanged();
        }
    );

    return optionalContainer;
}

QWidget* SchemaFormWidget::widgetForMember(const std::string& name) const {
    auto it = std::find_if(
        _members.begin(),
        _members.end(),
        [&](const SchemaMember& member) { return member.name == name; }
    );
    if (it == _members.end() || it->type == "Table") {
        return nullptr;
    }
    const int index = static_cast<int>(std::distance(_members.begin(), it));
    return _fieldWidgets[index];
}

void SchemaFormWidget::applyToProperties() {
    for (size_t i = 0; i < _members.size(); i++) {
        QWidget* widget = _fieldWidgets[i];
        if (!widget || !_optionalFieldActive[i]) {
            continue;
        }
        const SchemaMember& member = _members[i];

        // Union pages maintain _properties via their own signal handlers; skip to avoid
        // type-detection issues (member.type is the union string, not the active type)
        if (widget->objectName() == UnionContainerName) {
            continue;
        }

        std::optional<PropertyValue> value = readWidgetValue(widget, member.type);
        if (value.has_value()) {
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
    auto it = std::find_if(
        _members.begin(),
        _members.end(),
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

    // Already in the requested state - nothing to do
    if (active == _optionalFieldActive[index]) {
        return;
    }

    // When deactivating, remove the property so it won't be serialized
    if (!active) {
        _properties.erase(memberName);
    }
    setOptionalFieldActive(index, active);
}

void SchemaFormWidget::syncFieldWith(const std::string& memberName,
                                     SchemaFormWidget* other)
{
    // Cross-connect text edits so typing in one form updates the other. setText() emits
    // textChanged but NOT textEdited, so the cross-connections cannot loop. Signals are
    // left unblocked so that other textChanged listeners (e.g. identifier
    // auto-generation) still fire
    QLineEdit* thisEdit = resolveLineEdit(widgetForMember(memberName));
    QLineEdit* otherEdit = resolveLineEdit(other->widgetForMember(memberName));

    if (thisEdit && otherEdit) {
        connect(thisEdit, &QLineEdit::textEdited, otherEdit, &QLineEdit::setText);
        connect(otherEdit, &QLineEdit::textEdited, thisEdit, &QLineEdit::setText);
    }

    // Cross-connect optional field toggle state so adding/removing the field in one form
    // mirrors the change in the other
    auto syncToggle = [memberName](SchemaFormWidget* sender, SchemaFormWidget* receiver) {
        connect(
            sender,
            &SchemaFormWidget::optionalFieldToggled,
            receiver,
            [receiver, memberName](const QString& name, bool active) {
                if (name == memberName) {
                    receiver->setFieldActive(memberName, active);
                }
            }
        );
    };
    syncToggle(this, other);
    syncToggle(other, this);
}

void SchemaFormWidget::populateFromProperties() {
    for (size_t i = 0; i < _members.size(); i++) {
        QWidget* widget = _fieldWidgets[i];
        if (!widget) {
            continue;
        }
        const SchemaMember& member = _members[i];

        // For optional fields, show or hide the active row based on whether the property
        // exists. Skip if the property is absent or null
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

        // Union widgets need special handling to detect the stored type and switch the
        // combo to the matching page before populating
        if (widget->objectName() == UnionContainerName) {
            populateUnionWidget(widget, value);
            continue;
        }

        // Regular flat widget - set the display value directly
        populateWidget(widget, value);
    }
}

void SchemaFormWidget::buildUi() {
    QBoxLayout* outer = new QVBoxLayout(this);
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

    if (_shouldSortMembers) {
        // Order: 1) Flat before Table 2) required before optional 3) alphabetical
        std::sort(
            displayOrder.begin(),
            displayOrder.end(),
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

    for (int row = 0; row < nMembers; row++) {
        addMemberToGrid(grid, row, displayOrder[row], _members[displayOrder[row]]);
    }

    outer->addWidget(gridWidget);
}

QWidget* SchemaFormWidget::createTableSection(const SchemaMember& member) {
    // Create the collapsible section header
    CollapsibleSection* section =
        new CollapsibleSection(splitPascalCase(member.name), this);

    section->setExpanded(_areSubSectionsExpanded);
    section->setMandatory(!member.isOptional);

    if (!member.documentation.empty()) {
        section->setDocumentation(
            QString::fromStdString(member.name),
            QString::fromStdString(member.documentation)
        );
    }

    // Property key for right-click copy/paste context menu
    section->setSectionKey(QString::fromStdString(member.name));

    // Forward section signals to the parent form
    connect(
        section, &CollapsibleSection::documentationRequested,
        this, &SchemaFormWidget::documentationRequested
    );
    connect(
        section, &CollapsibleSection::copyRequested,
        this, &SchemaFormWidget::sectionCopyRequested
    );
    connect(
        section, &CollapsibleSection::pasteRequested,
        this, &SchemaFormWidget::sectionPasteRequested
    );

    // Populate the section content based on the table kind
    const TableClassification classification = classifyTableMember(member);
    switch (classification.kind) {
    case TableKind::RefArray:
        buildRefArrayContent(
            section,
            member,
            classification.reference->identifier,
            _properties
        );
        break;
    case TableKind::Ref:
        buildRefContent(section, member, *classification.reference, _properties);
        break;
    case TableKind::ScalarArray: {
        // Scalar arrays are rendered as a single flat widget (e.g. a text field)
        // rather than a list with add/remove buttons. member.members[0] is the "*"
        // array template child — it has the right type but a placeholder name. Copy
        // it and replace the name with the parent's so the flat widget displays and
        // stores the value under the correct key (e.g. "Tags")
        SchemaMember scalar = member.members[0];
        scalar.name = member.name;
        section->setContentWidget(createFlatWidget(scalar));
        break;
    }
    case TableKind::InlineArray:
        buildInlineArrayContent(section, member, _properties);
        break;
    case TableKind::Table:
        buildInlineTableContent(section, member, _properties);
        break;
    }

    return section;
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

SchemaFormWidget* SchemaFormWidget::createNestedForm(
    const std::vector<SchemaMember>& members,
    PropertyMap& properties)
{
    SchemaFormWidget* inner = new SchemaFormWidget(
        members,
        properties,
        nullptr,
        false,
        true,
        _registry
    );
    connect(
        inner, &SchemaFormWidget::fieldChanged,
        this, &SchemaFormWidget::fieldChanged
    );
    connect(
        inner, &SchemaFormWidget::documentationRequested,
        this, &SchemaFormWidget::documentationRequested
    );
    connect(
        inner, &SchemaFormWidget::addDependency,
        this, &SchemaFormWidget::addDependency
    );
    return inner;
}

void SchemaFormWidget::emitTypeDocumentation(const SchemaCategory* category,
    const QString& typeName)
{
    const SchemaType* schemaType = findTypeByName(category, typeName);
    if (!schemaType) {
        return;
    }
    const Documentation info = {
        .name = splitPascalCase(schemaType->name),
        .type = "Table",
        .isOptional = false,
        .documentation = QString::fromStdString(schemaType->description)
    };
    emit documentationRequested(info);
}

void SchemaFormWidget::rebuildItemForm(QBoxLayout* layout, PropertyMap& properties,
    const QString& typeName,
    const SchemaCategory* category)
{
    clearLayout(layout);
    const SchemaType* type = findTypeByName(category, typeName);
    if (!type) {
        return;
    }

    properties["Type"] = PropertyValue{ typeName.toStdString() };
    SchemaFormWidget* inner = createNestedForm(type->members, properties);
    layout->addWidget(inner);
    inner->populateFromProperties();
}

// ---------------------------------------------------------------------------
// Array card builders
// build*Card and rebuild* call each other: removing an item triggers a
// rebuild, which clears all cards and re-creates them with updated indices.
// ---------------------------------------------------------------------------
void SchemaFormWidget::buildRefArrayCard(QBoxLayout* itemsLayout,
    const std::string& memberName,
    const SchemaCategory* category,
    const std::string& referenceId, size_t index,
    PropertyMap& properties)
{
    // Pointer alias - lambdas cannot capture references
    PropertyMap* propertiesPtr = &properties;
    PropertyList& items = properties[memberName].toList();
    PropertyMap& itemProperties = ensureListItemMap(items, index);

    // Card frame and header row
    auto [card, cardLayout] = createCardFrame();

    QWidget* headerRow = new QWidget();
    QBoxLayout* headerLayout = new QHBoxLayout(headerRow);
    headerLayout->setContentsMargins(0, 0, 0, 0);
    headerLayout->setSpacing(4);

    // Type dropdown - populated with concrete types, skipping the base type
    SearchDropdown* dropdown = new SearchDropdown(card);
    dropdown->setPlaceholderText("Select type...");

    const QString storedType = storedTypeFromMap(itemProperties);

    int preSelectedIndex = -1;
    int dropdownIndex = 0;
    if (category) {
        for (const SchemaType& schemaType : category->types) {
            // Skip the abstract base type of the category
            if (schemaType.identifier == referenceId) {
                continue;
            }
            const QString typeName = QString::fromStdString(schemaType.name);
            dropdown->addItem(splitPascalCase(schemaType.name), typeName);
            if (!storedType.isEmpty() && typeName == storedType) {
                preSelectedIndex = dropdownIndex;
            }
            ++dropdownIndex;
        }
    }
    dropdown->setCurrentIndex(preSelectedIndex);

    // Remove button for this array item
    QPushButton* removeButton = new QPushButton(RemoveGlyph, card);
    removeButton->setObjectName("combo-clear-button");
    removeButton->setFixedSize(RemoveButtonSize, RemoveButtonSize);
    removeButton->setToolTip("Remove item");

    // Assemble header: dropdown + remove button
    headerLayout->addWidget(dropdown, 1);
    headerLayout->addWidget(removeButton);
    cardLayout->addWidget(headerRow);

    // Inner form holder - populated when a type is selected
    QWidget* innerHolder = new QWidget(card);
    QBoxLayout* innerLayout = new QVBoxLayout(innerHolder);
    innerLayout->setContentsMargins(0, 0, 0, 0);
    innerLayout->setSpacing(0);
    cardLayout->addWidget(innerHolder);

    // Restore the inner form for a previously saved type selection
    if (preSelectedIndex >= 0) {
        rebuildItemForm(innerLayout, itemProperties, storedType, category);
    }

    // Type selected: reset the item's property map and rebuild the form
    connect(
        dropdown,
        &SearchDropdown::activated,
        this,
        [this, propertiesPtr, memberName, index, innerLayout, category, dropdown]() {
            PropertyMap& properties = *propertiesPtr;
            PropertyList& list = properties[memberName].toList();
            if (index >= list.size()) {
                return;
            }
            list[index] = PropertyValue{ PropertyMap{} };
            PropertyMap& itemProperties = list[index].toMap();
            const QString typeName = dropdown->currentData().toString();
            rebuildItemForm(innerLayout, itemProperties, typeName, category);
            emit fieldChanged();
        }
    );

    // Hover: show documentation for the highlighted type
    connect(
        dropdown,
        &SearchDropdown::highlighted,
        this,
        [this, category](const QVariant& itemData) {
            emitTypeDocumentation(category, itemData.toString());
        }
    );

    // Remove button: erase this item and rebuild the entire array
    connect(
        removeButton,
        &QPushButton::clicked,
        this,
        [this, propertiesPtr, memberName, index, itemsLayout, category, referenceId]() {
            PropertyMap& properties = *propertiesPtr;
            PropertyList& list = properties[memberName].toList();
            if (index < list.size()) {
                list.erase(list.begin() + static_cast<int>(index));
            }
            rebuildRefArray(itemsLayout, memberName, category, referenceId, properties);
            emit fieldChanged();
        }
    );

    itemsLayout->addWidget(card);
}

void SchemaFormWidget::rebuildRefArray(QBoxLayout* itemsLayout,
    const std::string& memberName,
    const SchemaCategory* category,
    const std::string& referenceId,
    PropertyMap& properties)
{
    clearLayout(itemsLayout);
    PropertyList& items = properties[memberName].toList();
    for (size_t i = 0; i < items.size(); i++) {
        buildRefArrayCard(itemsLayout, memberName, category, referenceId, i, properties);
    }
}

void SchemaFormWidget::buildInlineArrayCard(QBoxLayout* itemsLayout,
    const std::string& memberName,
    const std::vector<SchemaMember>& itemMembers,
    size_t index, PropertyMap& properties)
{
    PropertyMap* propertiesPtr = &properties;
    PropertyList& items = properties[memberName].toList();
    PropertyMap& itemProperties = ensureListItemMap(items, index);

    auto [card, cardLayout] = createCardFrame();

    QWidget* headerRow = new QWidget();
    QBoxLayout* headerLayout = new QHBoxLayout(headerRow);
    headerLayout->setContentsMargins(0, 0, 0, 0);
    headerLayout->setSpacing(4);
    headerLayout->addStretch();

    QPushButton* removeButton = new QPushButton(RemoveGlyph, card);
    removeButton->setObjectName("combo-clear-button");
    removeButton->setFixedSize(RemoveButtonSize, RemoveButtonSize);
    removeButton->setToolTip("Remove item");
    headerLayout->addWidget(removeButton);
    cardLayout->addWidget(headerRow);

    SchemaFormWidget* inner = createNestedForm(itemMembers, itemProperties);
    cardLayout->addWidget(inner);
    inner->populateFromProperties();

    // Capture pointer - underlying vector lives in _members
    const std::vector<SchemaMember>* membersPtr = &itemMembers;
    // Remove button: erase this item and rebuild the array
    connect(
        removeButton,
        &QPushButton::clicked,
        this,
        [this, propertiesPtr, memberName, index, itemsLayout, membersPtr]() {
            PropertyMap& properties = *propertiesPtr;
            PropertyList& list = properties[memberName].toList();
            if (index < list.size()) {
                list.erase(list.begin() + static_cast<int>(index));
            }
            rebuildInlineArray(itemsLayout, memberName, *membersPtr, properties);
            emit fieldChanged();
        }
    );

    itemsLayout->addWidget(card);
}

void SchemaFormWidget::rebuildInlineArray(QBoxLayout* itemsLayout,
    const std::string& memberName,
    const std::vector<SchemaMember>& itemMembers,
    PropertyMap& properties)
{
    clearLayout(itemsLayout);
    PropertyList& items = properties[memberName].toList();
    for (size_t i = 0; i < items.size(); i++) {
        buildInlineArrayCard(itemsLayout, memberName, itemMembers, i, properties);
    }
}

// ---------------------------------------------------------------------------
// Table builders
// ---------------------------------------------------------------------------
void SchemaFormWidget::buildRefArrayContent(CollapsibleSection* section,
    const SchemaMember& member,
    const std::string& referenceId,
    PropertyMap& properties)
{
    // Look up the category for the reference
    const SchemaCategory* category =
        AssetSchema::instance().findCategoryByTypeId(referenceId);
    const std::string memberName = member.name;
    PropertyMap* propertiesPtr = &properties;

    auto [itemsLayout, addButton] = createArrayScaffold(section);

    ensurePropertyList(properties, memberName);

    // Build a card for each existing item in the array
    PropertyList& items = properties[memberName].toList();
    for (size_t i = 0; i < items.size(); i++) {
        buildRefArrayCard(itemsLayout, memberName, category, referenceId, i, properties);
    }

    // "+ Add" button: appends a new item and builds its card
    connect(
        addButton,
        &QPushButton::clicked,
        this,
        [this, propertiesPtr, memberName, itemsLayout, category, referenceId]() {
            PropertyMap& properties = *propertiesPtr;
            properties[memberName].toList().push_back(PropertyValue{ PropertyMap{} });
            const size_t newIndex = properties[memberName].toList().size() - 1;
            buildRefArrayCard(
                itemsLayout,
                memberName,
                category,
                referenceId,
                newIndex,
                properties
            );
            emit fieldChanged();
        }
    );
}

void SchemaFormWidget::buildRefContent(CollapsibleSection* section,
    const SchemaMember& member,
    const SchemaReference& reference,
    PropertyMap& properties)
{
    // Look up the category and concrete type for the reference
    const SchemaCategory* category =
        AssetSchema::instance().findCategoryByTypeId(reference.identifier);
    const SchemaType* targetType =
        AssetSchema::instance().findType(reference.identifier);

    // Both lookups should succeed if the schema is well-formed
    if (!targetType || !category) {
        qWarning("buildRefContent: schema lookup failed for '%s'",
                 reference.identifier.c_str());
    }

    // Polymorphic when the target is the base type of its category
    // (e.g. "Renderable"), or as a fallback when lookup fails
    const bool isPolymorphic = !targetType || !category ||
        targetType->name == category->name;

    if (isPolymorphic) {
        buildPolymorphicRefContent(section, member, reference, category, properties);
        return;
    }

    PropertyMap& subProperties = ensurePropertyMap(properties, member.name);
    SchemaFormWidget* nested = createNestedForm(targetType->members, subProperties);

    section->setContentWidget(nested);
    nested->populateFromProperties();
}

void SchemaFormWidget::buildPolymorphicRefContent(CollapsibleSection* section,
    const SchemaMember& member,
    const SchemaReference& reference,
    const SchemaCategory* category,
    PropertyMap& properties)
{
    // Pointer alias - lambdas cannot capture references
    PropertyMap* propertiesPtr = &properties;

    // Outer container for the type selector and inner form
    QWidget* container = new QWidget();
    QBoxLayout* containerLayout = new QVBoxLayout(container);
    containerLayout->setContentsMargins(0, 4, 0, 4);
    containerLayout->setSpacing(4);

    // Check if the property map already has a saved type selection
    const bool hasStoredProperties =
        properties.count(member.name) > 0
        && properties.at(member.name).isMap();
    const QString storedTypeName = hasStoredProperties ?
        storedTypeFromMap(properties.at(member.name).toMap()) :
        QString();

    // Type dropdown - populated with all concrete types in the category, skipping the
    // abstract base type and the member's own name
    SearchDropdown* dropdown = new SearchDropdown(container);
    dropdown->setPlaceholderText(
        QString("No %1 selected").arg(splitPascalCase(member.name).toLower())
    );
    int preSelectedIndex = -1;
    int dropdownIndex = 0;

    // Add each concrete subtype to the dropdown
    if (category) {
        for (const SchemaType& schemaType : category->types) {
            // Skip self-referential entry (e.g. "Renderable" member)
            if (schemaType.name == member.name) {
                continue;
            }
            // Skip the abstract base type of the category
            if (schemaType.identifier == reference.identifier) {
                continue;
            }
            const QString typeName = QString::fromStdString(schemaType.name);
            dropdown->addItem(splitPascalCase(schemaType.name), typeName);

            if (!storedTypeName.isEmpty() && typeName == storedTypeName) {
                preSelectedIndex = dropdownIndex;
            }
            ++dropdownIndex;
        }
    }
    dropdown->setCurrentIndex(preSelectedIndex);

    // Inner form holder - populated when a type is selected
    QWidget* innerHolder = new QWidget();
    QBoxLayout* innerLayout = new QVBoxLayout(innerHolder);
    innerLayout->setContentsMargins(0, 0, 0, 0);
    innerLayout->setSpacing(0);

    // Selector row: dropdown & clear button
    QWidget* selectorRow = new QWidget();
    QBoxLayout* selectorLayout = new QHBoxLayout(selectorRow);
    selectorLayout->setContentsMargins(0, 0, 0, 0);
    selectorLayout->setSpacing(4);
    selectorLayout->addWidget(dropdown, 1);

    QPushButton* clearButton = new QPushButton(RemoveGlyph);
    clearButton->setObjectName("combo-clear-button");
    clearButton->setFixedSize(RemoveButtonSize, RemoveButtonSize);
    clearButton->setToolTip("Clear selection");
    selectorLayout->addWidget(clearButton);
    clearButton->setVisible(preSelectedIndex >= 0);

    // Assemble the container and set it as the section content
    containerLayout->addWidget(selectorRow);
    containerLayout->addWidget(innerHolder);
    section->setContentWidget(container);

    const std::string memberName = member.name;

    // Restore the inner form for a previously saved type selection
    if (preSelectedIndex >= 0) {
        PropertyMap& subProperties = ensurePropertyMap(properties, memberName);
        rebuildItemForm(innerLayout, subProperties, storedTypeName, category);
    }

    // Track the selected index so we can revert on cancelled type changes.
    // It needs to be a pointer because we mutate it in the lambda
    auto previousIndex = std::make_shared<int>(preSelectedIndex);

    // Type selected: reset the property map and rebuild the inner form
    connect(
        dropdown,
        &SearchDropdown::activated,
        this,
        [this, propertiesPtr, memberName, innerLayout, dropdown, clearButton, category,
         previousIndex]()
        {
            PropertyMap& properties = *propertiesPtr;

            // Check if the user has filled in any fields beyond the "Type" key
            const auto it = properties.find(memberName);
            bool hasEntry = it != properties.end() && it->second.isMap();
            bool hasExistingData = hasEntry && it->second.toMap().size() > 1;

            if (hasExistingData) {
                const int result = QMessageBox::question(
                    this,
                    "Change Type",
                    QString("Changing the type will discard all current settings."
                            " Continue?"),
                    QMessageBox::Yes | QMessageBox::No
                );
                if (result != QMessageBox::Yes) {
                    dropdown->setCurrentIndex(*previousIndex);
                    return;
                }
            }

            properties[memberName] = PropertyValue{ PropertyMap{} };
            PropertyMap& subProperties = properties[memberName].toMap();

            rebuildItemForm(
                innerLayout,
                subProperties,
                dropdown->currentData().toString(),
                category
            );
            emit fieldChanged();
            clearButton->setVisible(true);
            *previousIndex = dropdown->currentIndex();
        }
    );

    // Hover: show documentation for the highlighted type
    connect(
        dropdown,
        &SearchDropdown::highlighted,
        this,
        [this, category](const QVariant& itemData) {
            emitTypeDocumentation(category, itemData.toString());
        }
    );

    // Clear button: remove the selection and erase the property
    connect(
        clearButton,
        &QPushButton::clicked,
        this,
        [this, propertiesPtr, memberName, dropdown, innerLayout, clearButton,
         previousIndex]()
        {
            PropertyMap& properties = *propertiesPtr;

            // Check if the user has filled in any fields beyond the "Type" key
            const auto it = properties.find(memberName);
            bool hasEntry = it != properties.end() && it->second.isMap();
            bool hasExistingData = hasEntry && it->second.toMap().size() > 1;

            if (hasExistingData) {
                const int result = QMessageBox::question(
                    this,
                    "Clear Type",
                    QString("Clearing the type will discard all current settings."
                            " Continue?"),
                    QMessageBox::Yes | QMessageBox::No
                );
                if (result != QMessageBox::Yes) {
                    return;
                }
            }

            dropdown->setCurrentIndex(-1);
            clearButton->setVisible(false);
            clearLayout(innerLayout);
            properties.erase(memberName);
            emit fieldChanged();
            *previousIndex = -1;
        }
    );
}

void SchemaFormWidget::buildInlineArrayContent(CollapsibleSection* section,
    const SchemaMember& member,
    PropertyMap& properties)
{
    // members[0] is the "*" array template child; its sub-members define the schema for
    // each element in the array
    const std::vector<SchemaMember>& itemMembers = member.members[0].members;
    const std::string memberName = member.name;
    const std::vector<SchemaMember>* membersPtr = &itemMembers;
    PropertyMap* propertiesPtr = &properties;

    auto [itemsLayout, addButton] = createArrayScaffold(section);

    ensurePropertyList(properties, memberName);

    PropertyList& items = properties[memberName].toList();
    for (size_t i = 0; i < items.size(); i++) {
        buildInlineArrayCard(itemsLayout, memberName, *membersPtr, i, properties);
    }

    // "+ Add" button: appends a new item to the array and builds its card
    connect(
        addButton,
        &QPushButton::clicked,
        this,
        [this, propertiesPtr, memberName, itemsLayout, membersPtr]() {
            PropertyMap& properties = *propertiesPtr;
            properties[memberName].toList().push_back(PropertyValue{ PropertyMap{} });
            const size_t newIndex = properties[memberName].toList().size() - 1;
            buildInlineArrayCard(
                itemsLayout,
                memberName,
                *membersPtr,
                newIndex,
                properties
            );
            emit fieldChanged();
        }
    );
}

void SchemaFormWidget::buildInlineTableContent(CollapsibleSection* section,
    const SchemaMember& member,
    PropertyMap& properties)
{
    if (member.members.empty()) {
        return;
    }

    PropertyMap& subProperties = ensurePropertyMap(properties, member.name);
    SchemaFormWidget* nested = createNestedForm(member.members, subProperties);
    section->setContentWidget(nested);
    nested->populateFromProperties();
}

QWidget* SchemaFormWidget::createFlatWidget(const SchemaMember& member) {
    const QString typeString = QString::fromStdString(member.type);
    auto onChange = [this]() {
        emit fieldChanged();
    };

    // Union types (contain ", " or " or ")
    if (typeString.contains(", ") || typeString.contains(" or ")) {
        return createUnionWidget(member);
    }

    if (member.type == "Boolean") {
        return createBooleanWidget(member.name, _properties, onChange);
    }
    if (member.type == "Integer") {
        return createIntegerWidget(member.name, member.description, _properties, onChange);
    }
    if (member.type == "Double") {
        return createDoubleWidget(member.name, member.description, _properties, onChange);
    }
    for (const MatrixTypeEntry& entry : MatrixTypeLookup) {
        if (member.type == entry.typeName) {
            return createMatrixWidget(member.name, entry, _properties, onChange);
        }
    }
    if (member.type == "File") {
        return createFileWidget(
            member.name,
            member.description,
            false,
            _properties,
            onChange
        );
    }
    if (member.type == "Directory") {
        return createFileWidget(
            member.name,
            member.description,
            true,
            _properties,
            onChange
        );
    }
    if (member.type == "Date and time") {
        return createDateTimeWidget(member.name, _properties, onChange);
    }
    // Identifier fields that reference other nodes (not the node's own Identifier
    // definition) get a registry combo + browse button
    if (member.type == "Identifier" && member.name != "Identifier" && _registry) {
        auto onSelect = [this](const QString& filePath) {
            emit addDependency(filePath);
        };
        return createIdentifierComboWidget(
            member,
            _registry,
            _properties,
            onChange,
            onSelect
        );
    }
    const QStringList options = parseInList(member.description);
    if (!options.isEmpty()) {
        return createInListWidget(member.name, options, _properties, onChange);
    }

    // Fallback to string
    return createStringWidget(member.name, member.description, _properties, onChange);
}

QWidget* SchemaFormWidget::createUnionWidget(const SchemaMember& member) {
    const QString typeString = QString::fromStdString(member.type);
    QStringList types = splitUnionTypes(typeString);

    // Combo selector + show/hide pages. Hidden widgets take zero layout
    // space, so the container fits the active page.
    QWidget* container = new QWidget(this);
    container->setObjectName(UnionContainerName);
    QBoxLayout* verticalLayout = new QVBoxLayout(container);
    verticalLayout->setContentsMargins(0, 0, 0, 0);
    verticalLayout->setSpacing(4);

    QComboBox* typeCombo = new QComboBox(container);
    typeCombo->setObjectName(UnionTypeComboName);
    verticalLayout->addWidget(typeCombo);

    QList<QWidget*> typeWidgets;
    for (int j = 0; j < types.size(); j++) {
        typeCombo->addItem(types[j]);
        // Empty description — the union description applies to the union
        // as a whole, not the individual type
        SchemaMember typeMember = { member.name, types[j].toStdString(), {} };
        QWidget* widget = (types[j] == "Table") ?
            createTableSection(typeMember) :
            createFlatWidget(typeMember);

        // Parent first, then set visibility — a parentless widget with
        // setVisible(true) becomes a top-level window on Windows
        verticalLayout->addWidget(widget);
        widget->setVisible(j == 0);
        typeWidgets.append(widget);
    }

    connect(
        typeCombo,
        &QComboBox::currentIndexChanged,
        this, [typeWidgets](int index) {
            for (int i = 0; i < typeWidgets.size(); ++i) {
                typeWidgets[i]->setVisible(i == index);
            }
        }
    );

    return container;
}
