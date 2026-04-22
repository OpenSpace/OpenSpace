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

#include "form/collapsiblesection.h"
#include "form/searchdropdown.h"
#include "documentation.h"
#include "schema/assetschema.h"
#include "utils.h"

#include <QFrame>
#include <QHBoxLayout>
#include <QPushButton>
#include <QVBoxLayout>

#include <string>
#include <vector>

namespace {

constexpr int CardMarginHorizontal = 8;
constexpr int CardMarginVertical = 6;

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

// Determines the table kind (ref array, ref, scalar array, inline array,
// or plain table) based on the member's structure and references.
TableClassification classifyTableMember(const SchemaMember& member) {
    // An array member has exactly one child named "*", which serves as the
    // schema template for each element in the array.
    const bool isArray = !member.reference.has_value() &&
        member.members.size() == 1 && member.members[0].name == "*";
    const auto& optionalReference = isArray
        ? member.members[0].reference : member.reference;

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

// Removes and deletes all widgets and layout items from a layout.
void clearLayout(QLayout* layout) {
    while (layout->count() > 0) {
        QLayoutItem* layoutItem = layout->takeAt(0);
        if (layoutItem->widget()) {
            delete layoutItem->widget();
        }
        delete layoutItem;
    }
}

// Looks up a schema type by display name within a category.
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

// Extracts the "Type" value from a property map, or empty if absent.
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

// Creates the shared container layout with an items area and "+ Add" button
// for array sections.
ArrayScaffold createArrayScaffold(CollapsibleSection* section) {
    // Outer container for the whole array section
    QWidget* container = new QWidget();
    QVBoxLayout* containerLayout = new QVBoxLayout(container);
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
    QVBoxLayout* layout;
};

// Creates a styled card frame with vertical layout for an array item.
CardFrame createCardFrame() {
    QFrame* card = new QFrame();
    card->setObjectName("array-item-frame");
    card->setFrameShape(QFrame::StyledPanel);

    QVBoxLayout* cardLayout = new QVBoxLayout(card);

    cardLayout->setContentsMargins(
        CardMarginHorizontal, CardMarginVertical,
        CardMarginHorizontal, CardMarginVertical
    );
    cardLayout->setSpacing(4);
    return { card, cardLayout };
}

// The ensure* functions guarantee that a PropertyMap or PropertyList exists
// at the given key before a nested form or array binds to it. Without them,
// accessing an unset key would produce a null PropertyValue, and calling
// toMap()/toList() on it would fail.

// Ensures the item at the given index is a map, replacing it if needed.
PropertyMap& ensureListItemMap(PropertyList& items, size_t index) {
    Q_ASSERT(index < items.size());
    if (!items[index].isMap()) {
        items[index] = PropertyValue{ PropertyMap{} };
    }
    return items[index].toMap();
}

// Ensures a property map exists at the given key, creating one if absent.
PropertyMap& ensurePropertyMap(PropertyMap& properties, const std::string& key) {
    if (properties.count(key) == 0 || properties.at(key).isNull()) {
        properties[key] = PropertyValue{ PropertyMap{} };
    }
    Q_ASSERT(properties[key].isMap());
    return properties[key].toMap();
}

// Ensures a property list exists at the given key. Creates one if absent,
// or wraps a single map value in a one-element list for uniformity.
void ensurePropertyList(PropertyMap& properties, const std::string& key) {
    if (properties.count(key) == 0 || properties.at(key).isNull()) {
        properties[key] = PropertyValue{ PropertyList{} };
    }
    else if (properties.at(key).isMap()) {
        // A single object was stored instead of a list — wrap it in a one-element list
        // so downstream code can treat it uniformly as an array.
        PropertyMap existing = std::move(properties[key].toMap());
        PropertyList wrapped;
        wrapped.push_back(PropertyValue{ std::move(existing) });
        properties[key] = PropertyValue{ std::move(wrapped) };
    }
    else {
        // Should already be a list if it exists and isn't null or a map
        Q_ASSERT(properties.at(key).isList());
    }
}

} // namespace

// ---------------------------------------------------------------------------
// Table section entry point
// ---------------------------------------------------------------------------

QWidget* SchemaFormWidget::createTableSection(const SchemaMember& member) {
    // Create the collapsible section header
    CollapsibleSection* section =
        new CollapsibleSection(splitPascalCase(member.name), this);

    section->setExpanded(_subSectionsExpanded);
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
    connect(section, &CollapsibleSection::documentationRequested,
        this, &SchemaFormWidget::documentationRequested);
    connect(section, &CollapsibleSection::copyRequested,
        this, &SchemaFormWidget::sectionCopyRequested);
    connect(section, &CollapsibleSection::pasteRequested,
        this, &SchemaFormWidget::sectionPasteRequested);

    // Populate the section content based on the table kind
    const TableClassification classification = classifyTableMember(member);
    switch (classification.kind) {
        case TableKind::RefArray:
            buildRefArrayContent(
                section, member, classification.reference->identifier, _properties
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
            // stores the value under the correct key (e.g. "Tags").
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
    const std::vector<SchemaMember>& members, PropertyMap& properties)
{
    auto* inner = new SchemaFormWidget(
        members, properties, nullptr, false, true, _registry);
    connect(inner, &SchemaFormWidget::fieldChanged,
        this, &SchemaFormWidget::fieldChanged);
    connect(inner, &SchemaFormWidget::documentationRequested,
        this, &SchemaFormWidget::documentationRequested);
    connect(inner, &SchemaFormWidget::browseJassetRequested,
        this, &SchemaFormWidget::browseJassetRequested);
    return inner;
}

void SchemaFormWidget::emitTypeDocumentation(
    const SchemaCategory* category, const QString& typeName)
{
    const SchemaType* schemaType = findTypeByName(category, typeName);
    if (!schemaType) {
        return;
    }
    Documentation info;
    info.name = splitPascalCase(schemaType->name);
    info.type = "Table";
    info.isOptional = false;
    info.documentation = QString::fromStdString(schemaType->description);
    emit documentationRequested(info);
}

void SchemaFormWidget::rebuildItemForm(
    QVBoxLayout* layout, PropertyMap& properties,
    const QString& typeName, const SchemaCategory* category)
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

void SchemaFormWidget::buildRefArrayCard(
    QVBoxLayout* itemsLayout, const std::string& memberName,
    const SchemaCategory* category, const std::string& referenceId,
    size_t index, PropertyMap& properties)
{
    // Pointer alias — lambdas cannot capture references
    PropertyMap* propertiesPtr = &properties;
    PropertyList& items = properties[memberName].toList();
    PropertyMap& itemProperties = ensureListItemMap(items, index);

    // Card frame and header row
    auto [card, cardLayout] = createCardFrame();

    QWidget* headerRow = new QWidget();
    QHBoxLayout* headerLayout = new QHBoxLayout(headerRow);
    headerLayout->setContentsMargins(0, 0, 0, 0);
    headerLayout->setSpacing(4);

    // Type dropdown — populated with concrete types, skipping the base type
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

    // Inner form holder — populated when a type is selected
    QWidget* innerHolder = new QWidget(card);
    QVBoxLayout* innerLayout = new QVBoxLayout(innerHolder);
    innerLayout->setContentsMargins(0, 0, 0, 0);
    innerLayout->setSpacing(0);
    cardLayout->addWidget(innerHolder);

    // Restore the inner form for a previously saved type selection
    if (preSelectedIndex >= 0) {
        rebuildItemForm(innerLayout, itemProperties, storedType, category);
    }

    // Type selected: reset the item's property map and rebuild the form
    connect(dropdown, &SearchDropdown::activated, this,
        [this, propertiesPtr, memberName, index, innerLayout, category, dropdown](int)
    {
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
    });

    // Hover: show documentation for the highlighted type
    connect(dropdown, &SearchDropdown::highlighted, this,
        [this, category](const QVariant& itemData)
    {
        emitTypeDocumentation(category, itemData.toString());
    });

    // Remove button: erase this item and rebuild the entire array
    connect(removeButton, &QPushButton::clicked, this,
        [this, propertiesPtr, memberName, index, itemsLayout, category, referenceId]()
    {
        PropertyMap& properties = *propertiesPtr;
        PropertyList& list = properties[memberName].toList();
        if (index < list.size()) {
            list.erase(list.begin() + static_cast<int>(index));
        }
        rebuildRefArray(itemsLayout, memberName, category, referenceId, properties);
        emit fieldChanged();
    });

    itemsLayout->addWidget(card);
}

void SchemaFormWidget::rebuildRefArray(
    QVBoxLayout* itemsLayout, const std::string& memberName,
    const SchemaCategory* category, const std::string& referenceId,
    PropertyMap& properties)
{
    clearLayout(itemsLayout);
    PropertyList& items = properties[memberName].toList();
    for (size_t i = 0; i < items.size(); ++i) {
        buildRefArrayCard(itemsLayout, memberName, category, referenceId, i, properties);
    }
}

void SchemaFormWidget::buildInlineArrayCard(
    QVBoxLayout* itemsLayout, const std::string& memberName,
    const std::vector<SchemaMember>& itemMembers, size_t index,
    PropertyMap& properties)
{
    PropertyMap* propertiesPtr = &properties;
    PropertyList& items = properties[memberName].toList();
    PropertyMap& itemProperties = ensureListItemMap(items, index);

    auto [card, cardLayout] = createCardFrame();

    QWidget* headerRow = new QWidget();
    QHBoxLayout* headerLayout = new QHBoxLayout(headerRow);
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

    // Capture pointer — underlying vector lives in _members
    const std::vector<SchemaMember>* membersPtr = &itemMembers;
    // Remove button: erase this item and rebuild the array
    connect(removeButton, &QPushButton::clicked, this,
        [this, propertiesPtr, memberName, index, itemsLayout, membersPtr]()
    {
        PropertyMap& properties = *propertiesPtr;
        PropertyList& list = properties[memberName].toList();
        if (index < list.size()) {
            list.erase(list.begin() + static_cast<int>(index));
        }
        rebuildInlineArray(itemsLayout, memberName, *membersPtr, properties);
        emit fieldChanged();
    });

    itemsLayout->addWidget(card);
}

void SchemaFormWidget::rebuildInlineArray(
    QVBoxLayout* itemsLayout, const std::string& memberName,
    const std::vector<SchemaMember>& itemMembers, PropertyMap& properties)
{
    clearLayout(itemsLayout);
    PropertyList& items = properties[memberName].toList();
    for (size_t i = 0; i < items.size(); ++i) {
        buildInlineArrayCard(itemsLayout, memberName, itemMembers, i, properties);
    }
}

// ---------------------------------------------------------------------------
// Public content builders
// ---------------------------------------------------------------------------

void SchemaFormWidget::buildRefArrayContent(
    CollapsibleSection* section, const SchemaMember& member,
    const std::string& referenceId, PropertyMap& properties)
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
    for (size_t i = 0; i < items.size(); ++i) {
        buildRefArrayCard(itemsLayout, memberName, category, referenceId, i, properties);
    }

    // "+ Add" button: appends a new item and builds its card
    connect(addButton, &QPushButton::clicked, this,
        [this, propertiesPtr, memberName, itemsLayout, category, referenceId]()
    {
        PropertyMap& properties = *propertiesPtr;
        properties[memberName].toList().push_back(PropertyValue{ PropertyMap{} });
        const size_t newIndex = properties[memberName].toList().size() - 1;
        buildRefArrayCard(
            itemsLayout, memberName, category, referenceId,
            newIndex, properties);
        emit fieldChanged();
    });
}

void SchemaFormWidget::buildRefContent(
    CollapsibleSection* section, const SchemaMember& member,
    const SchemaReference& reference, PropertyMap& properties)
{
    // Look up the category and concrete type for the reference
    const SchemaCategory* category =
        AssetSchema::instance().findCategoryByTypeId(reference.identifier);
    const SchemaType* targetType =
        AssetSchema::instance().findType(reference.identifier);

    // Both lookups should succeed if the schema is well-formed
    Q_ASSERT(targetType);
    Q_ASSERT(category);

    // Polymorphic when the target is the base type of its category
    // (e.g. "Renderable"), or as a fallback when lookup fails.
    const bool isPolymorphic = !targetType || !category ||
        targetType->name == category->name;

    if (isPolymorphic) {
        buildPolymorphicRefContent(
            section, member, reference, category, properties
        );
        return;
    }

    PropertyMap& subProperties = ensurePropertyMap(properties, member.name);
    SchemaFormWidget* nested = createNestedForm(targetType->members, subProperties);

    section->setContentWidget(nested);
    nested->populateFromProperties();
}

void SchemaFormWidget::buildPolymorphicRefContent(
    CollapsibleSection* section, const SchemaMember& member,
    const SchemaReference& reference, const SchemaCategory* category,
    PropertyMap& properties)
{
    // Pointer alias — lambdas cannot capture references
    PropertyMap* propertiesPtr = &properties;

    // Outer container for the type selector and inner form
    QWidget* container = new QWidget();
    QVBoxLayout* containerLayout = new QVBoxLayout(container);
    containerLayout->setContentsMargins(0, 4, 0, 4);
    containerLayout->setSpacing(4);

    // Check if the property map already has a saved type selection
    const bool hasStoredProperties =
        properties.count(member.name) > 0
        && properties.at(member.name).isMap();
    const QString storedTypeName = hasStoredProperties
        ? storedTypeFromMap(properties.at(member.name).toMap())
        : QString();

    // Type dropdown — populated with all concrete types in the category,
    // skipping the abstract base type and the member's own name
    SearchDropdown* dropdown = new SearchDropdown(container);
    dropdown->setPlaceholderText(
        QString("No %1 selected")
            .arg(splitPascalCase(member.name).toLower())
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

    // Inner form holder — populated when a type is selected
    QWidget* innerHolder = new QWidget();
    QVBoxLayout* innerLayout = new QVBoxLayout(innerHolder);
    innerLayout->setContentsMargins(0, 0, 0, 0);
    innerLayout->setSpacing(0);

    // Selector row: dropdown & clear button
    QWidget* selectorRow = new QWidget();
    QHBoxLayout* selectorLayout = new QHBoxLayout(selectorRow);
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

    // Type selected: reset the property map and rebuild the inner form
    connect(dropdown, &SearchDropdown::activated, this,
        [this, propertiesPtr, memberName, innerLayout, dropdown,
            clearButton, category](int)
    {
        PropertyMap& properties = *propertiesPtr;
        properties[memberName] = PropertyValue{ PropertyMap{} };
        PropertyMap& subProperties = properties[memberName].toMap();

        rebuildItemForm(innerLayout, subProperties,
            dropdown->currentData().toString(), category);
        emit fieldChanged();
        clearButton->setVisible(true);
    });

    // Hover: show documentation for the highlighted type
    connect(dropdown, &SearchDropdown::highlighted, this,
        [this, category](const QVariant& itemData)
    {
        emitTypeDocumentation(category, itemData.toString());
    });

    // Clear button: remove the selection and erase the property
    connect(clearButton, &QPushButton::clicked, this,
        [this, propertiesPtr, memberName, dropdown,
            innerLayout, clearButton]()
    {
        PropertyMap& properties = *propertiesPtr;
        dropdown->setCurrentIndex(-1);
        clearButton->setVisible(false);
        clearLayout(innerLayout);
        properties.erase(memberName);
        emit fieldChanged();
    });
}

void SchemaFormWidget::buildInlineArrayContent(
    CollapsibleSection* section, const SchemaMember& member,
    PropertyMap& properties)
{
    // members[0] is the "*" array template child; its sub-members define
    // the schema for each element in the array
    const std::vector<SchemaMember>& itemMembers = member.members[0].members;
    const std::string memberName = member.name;
    const std::vector<SchemaMember>* membersPtr = &itemMembers;
    PropertyMap* propertiesPtr = &properties;

    auto [itemsLayout, addButton] = createArrayScaffold(section);

    ensurePropertyList(properties, memberName);

    PropertyList& items = properties[memberName].toList();
    for (size_t i = 0; i < items.size(); ++i) {
        buildInlineArrayCard(itemsLayout, memberName, *membersPtr, i, properties);
    }

    // "+ Add" button: appends a new item to the array and builds its card
    connect(addButton, &QPushButton::clicked, this,
        [this, propertiesPtr, memberName, itemsLayout, membersPtr]()
    {
        PropertyMap& properties = *propertiesPtr;
        properties[memberName].toList().push_back(PropertyValue{ PropertyMap{} });
        const size_t newIndex = properties[memberName].toList().size() - 1;
        buildInlineArrayCard(
            itemsLayout, memberName, *membersPtr,
            newIndex, properties);
        emit fieldChanged();
    });
}

void SchemaFormWidget::buildInlineTableContent(
    CollapsibleSection* section, const SchemaMember& member,
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
