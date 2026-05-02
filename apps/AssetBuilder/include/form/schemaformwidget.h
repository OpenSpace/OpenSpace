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

#ifndef __OPENSPACE_ASSETBUILDER___FORM_SCHEMAFORMWIDGET___H__
#define __OPENSPACE_ASSETBUILDER___FORM_SCHEMAFORMWIDGET___H__

#include <QWidget>

#include "documentation.h"
#include "jasset.h"
#include "schema/assetschema.h"
#include <memory>
#include <vector>

class CollapsibleSection;
class IdentifierRegistry;
class QBoxLayout;
class QGridLayout;

/**
 * Generates a form from a list of SchemaMember descriptors bound to a PropertyMap. Flat
 * members (string, number, boolean, etc.) are rendered as labeled fields in a grid. Table
 * members become nested CollapsibleSection widgets with type-appropriate content.
 */
class SchemaFormWidget final : public QWidget {
Q_OBJECT
public:
    /**
     * Constructs a form for the given schema members, bound to \p properties.
     *
     * \param members Schema member descriptors to render
     * \param properties PropertyMap to read from and write to
     * \param parent Parent widget
     * \param subSectionsExpanded Whether Table sub-sections start expanded
     * \param sortMembers Whether to sort members (required before optional, flat before
     *        Table, then alphabetical)
     * \param registry Optional identifier registry for Identifier-type combos
     */
    SchemaFormWidget(std::vector<SchemaMember> members,
        std::weak_ptr<PropertyMap> properties, QWidget* parent = nullptr,
        bool subSectionsExpanded = false, bool sortMembers = true,
        const IdentifierRegistry* registry = nullptr);

    /**
     * Writes current widget values into the bound PropertyMap.
     */
    void applyToProperties();

    /**
     * Reads the bound PropertyMap and updates field widgets to match.
     */
    void populateFromProperties();

    /**
     * Returns the leaf field widget (e.g. QLineEdit, QCheckBox) for the member with the
     * given name, or nullptr if the name is unknown or the member is a Table type.
     *
     * \param name The member for which the widget should be returned
     */
    QWidget* widgetForMember(const std::string& name) const;

    /**
     * Programmatically activates or deactivates an optional field by name. When
     * deactivating, the property is erased and the widget is cleared. Does NOT emit
     * optionalFieldToggled (to prevent infinite signal loops).
     *
     * \param memberName The schema member name (e.g. "Parent", "Name")
     * \param active `true` to show the field, `false` to hide and clear it
     */
    void setFieldActive(const std::string& memberName, bool active);

    /**
     * Cross-connects the field identified by \p memberName between this form and \p other
     * so that text edits and optional-field toggles stay in sync.
     *
     * \param memberName The schema member name present in both forms
     * \param other The other SchemaFormWidget to sync with
     */
    void syncFieldWith(const std::string& memberName, SchemaFormWidget* other);

signals:
    /**
     * Emitted whenever a field value changes (user edit or programmatic).
     */
    void fieldChanged();

    /**
     * Emitted when the user clicks a field's info button.
     */
    void documentationRequested(const Documentation& info);

    /**
     * Emitted when the user selects Copy from a CollapsibleSection context menu.
     */
    void sectionCopyRequested(const QString& key);

    /**
     * Emitted when the user selects Paste from a CollapsibleSection context menu.
     */
    void sectionPasteRequested(const QString& key);

    /**
     * Emitted when the user clicks the Add or Remove button on an optional field. NOT
     * emitted when setFieldActive() is called programmatically.
     */
    void optionalFieldToggled(const QString& name, bool active);

    /**
     * Emitted when the user selects a .jasset file via the Browse button on an
     * Identifier field. Carries the absolute file path to add as a dependency.
     *
     * \param filePath Absolute path to the selected .jasset file
     */
    void addDependency(const QString& filePath);

private:
    /**
     * Adds a single member's label and widget to the grid at \p row.
     */
    void addMemberToGrid(QGridLayout* grid, int row, int memberIndex,
        const SchemaMember& member);

    /**
     * Wraps a field widget with Add (+) / Remove (x) buttons for optional members.
     */
    QWidget* createOptionalWrapper(int memberIndex, QWidget* field);

    /**
     * Shows or hides the active row for an optional field by vector index.
     */
    void setOptionalFieldActive(int memberIndex, bool active);

    /**
     * Dispatches to a type-specific creator based on the member's schema type.
     */
    QWidget* createFlatWidget(const SchemaMember& member);

    /**
     * Creates a CollapsibleSection for a Table member, dispatching by table kind.
     */
    QWidget* createTableSection(const SchemaMember& member);

    /**
     * Creates a nested SchemaFormWidget and wires its signals to this form.
     */
    SchemaFormWidget* createNestedForm(const std::vector<SchemaMember>& members,
        PropertyMap& subMap);

    /**
     * Emits documentationRequested for a named type within a category.
     */
    void emitTypeDocumentation(const SchemaCategory* category, const QString& typeName);

    /**
     * Replaces the form contents when the user picks a new type from a dropdown. Clears
     * the existing layout, looks up the selected type's schema, stores the type name in
     * the property map, and creates a new nested form populated with any existing
     * property values.
     */
    void rebuildItemForm(QBoxLayout* layout, PropertyMap& properties,
        const QString& typeName, const SchemaCategory* category);

    /**
     * Populates a CollapsibleSection with an array of ref-typed items.
     */
    void buildRefArrayContent(CollapsibleSection* section, const SchemaMember& member,
        const std::string& referenceId, std::weak_ptr<PropertyMap> properties);

    /**
     * Builds a single card (type dropdown + form) for one ref-array item.
     */
    void buildRefArrayCard(QBoxLayout* itemsLayout, const std::string& memberName,
        const SchemaCategory* category, const std::string& referenceId, size_t index,
        std::weak_ptr<PropertyMap> properties);

    /**
     * Clears and rebuilds all ref-array cards from the current property list.
     */
    void rebuildRefArray(QBoxLayout* itemsLayout, const std::string& memberName,
        const SchemaCategory* category, const std::string& referenceId,
        std::weak_ptr<PropertyMap> properties);

    /**
     * Populates a CollapsibleSection with a concrete or polymorphic ref.
     */
    void buildRefContent(CollapsibleSection* section, const SchemaMember& member,
        const SchemaReference& reference, PropertyMap& properties);

    /**
     * Builds a type-selector dropdown + form for a polymorphic ref.
     */
    void buildPolymorphicRefContent(CollapsibleSection* section,
        const SchemaMember& member, const SchemaReference& reference,
        const SchemaCategory* category, std::weak_ptr<PropertyMap> properties);

    /**
     * Populates a CollapsibleSection with an array of inline-member items.
     */
    void buildInlineArrayContent(CollapsibleSection* section, const SchemaMember& member,
        std::weak_ptr<PropertyMap> properties);

    /**
     * Builds a single card (nested form + remove button) for one inline array item.
     */
    void buildInlineArrayCard(QBoxLayout* itemsLayout, const std::string& memberName,
        const std::vector<SchemaMember>& itemMembers, size_t index,
        std::weak_ptr<PropertyMap> properties);

    /**
     * Clears and rebuilds all inline-array cards from the current property list.
     */
    void rebuildInlineArray(QBoxLayout* itemsLayout, const std::string& memberName,
        const std::vector<SchemaMember>& itemMembers,
        std::weak_ptr<PropertyMap> properties);

    /**
     * Populates a CollapsibleSection with a nested form for a plain table member.
     */
    void buildInlineTableContent(CollapsibleSection* section, const SchemaMember& member,
        PropertyMap& properties);

    /// Weak reference to the bound property map (root or sub-map)
    std::weak_ptr<PropertyMap> _properties;

    struct MemberInfo {
        /// Schema member descriptors for this form
        SchemaMember member;
        /// Per-member leaf widget (flat) or CollapsibleSection (Table)
        QWidget* fieldWidget = nullptr;
        /// Per-member "+" button; nullptr for required and Table members
        QWidget* addButton = nullptr;
        /// Per-member active row container; nullptr for required and Table members
        QWidget* activeRows = nullptr;
        /// Whether each optional field is currently active. Always `true` for
        /// required/Table
        bool optionalActive = true;
    };
    std::vector<MemberInfo> _members;

    /// Whether new sub-sections start expanded
    const bool _areSubSectionsExpanded = false;
    /// Whether members are sorted in the grid
    const bool _shouldSortMembers = true;
    /// Registry for Identifier-type combo boxes; `nullptr` if unavailable
    const IdentifierRegistry* _registry = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___FORM_SCHEMAFORMWIDGET___H__
