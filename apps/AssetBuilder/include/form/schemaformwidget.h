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

#ifndef __OPENSPACE_ASSET_BUILDER___FORM_SCHEMAFORMWIDGET___H__
#define __OPENSPACE_ASSET_BUILDER___FORM_SCHEMAFORMWIDGET___H__

#include "documentation.h"
#include "jasset.h"
#include "schema/assetschema.h"

#include <QStringList>
#include <QWidget>

#include <vector>

/// Describes a matrix/vector/color type for widget creation.
struct MatrixTypeEntry {
    /// Schema type name (e.g. "Vector3<double>", "Color4").
    const char* typeName;
    /// Total number of numeric fields.
    int nComponents;
    /// Columns in the grid layout.
    int nColumns;
    /// true for integer validation, false for double.
    bool isInteger;
    /// true to use ColorWidget instead of plain MatrixWidget.
    bool isColor;
};

// Shared constants used across the schemaformwidget .cpp files.
constexpr const char* DateDisplayFormat = "yyyy-MM-dd HH:mm:ss";

// Object names used as type tags for widget identification.
// Set in schemaformwidget_flat.cpp, checked in schemaformwidget.cpp.
constexpr const char* FileContainerName = "file-container";
constexpr const char* UnionContainerName = "union-container";
constexpr const char* UnionTypeComboName = "union-type-combo";
constexpr const char* DateEditName = "date-edit";

class CollapsibleSection;
class IdentifierRegistry;
class QGridLayout;
class QPushButton;
class QVBoxLayout;

/**
 * Generates a form from a list of SchemaMember descriptors bound to a PropertyMap.
 * Flat members (string, number, boolean, etc.) are rendered as labeled fields in a grid.
 * Table members become nested CollapsibleSection widgets with type-appropriate content.
 */
class SchemaFormWidget final : public QWidget {
Q_OBJECT
public:
    /**
     * Constructs a form for the given schema members, bound to \p properties.
     *
     * \param members             Schema member descriptors to render
     * \param properties          PropertyMap to read from and write to
     * \param parent              Parent widget
     * \param subSectionsExpanded Whether Table sub-sections start expanded
     * \param sortMembers         Whether to sort members (required before optional,
     *                            flat before Table, then alphabetical)
     * \param registry            Optional identifier registry for Identifier-type combos
     */
    explicit SchemaFormWidget(
        const std::vector<SchemaMember>& members,
        PropertyMap& properties,
        QWidget* parent = nullptr,
        bool subSectionsExpanded = false,
        bool sortMembers = true,
        IdentifierRegistry* registry = nullptr
    );

    /** Writes current widget values into the bound PropertyMap. */
    void applyToProperties();

    /** Reads the bound PropertyMap and updates field widgets to match. */
    void populateFromProperties();

    /**
     * Returns the leaf field widget (e.g. QLineEdit, QCheckBox) for the member with the
     * given name, or nullptr if the name is unknown or the member is a Table type.
     */
    QWidget* widgetForMember(const std::string& name) const;

    /**
     * Programmatically activates or deactivates an optional field by name.
     * When deactivating, the property is erased and the widget is cleared.
     * Does NOT emit optionalFieldToggled (to prevent infinite signal loops).
     *
     * \param memberName The schema member name (e.g. "Parent", "Name")
     * \param active     true to show the field, false to hide and clear it
     */
    void setFieldActive(const std::string& memberName, bool active);

    /**
     * Cross-connects the field identified by \p memberName between this form
     * and \p other so that text edits and optional-field toggles stay in sync.
     *
     * \param memberName The schema member name present in both forms
     * \param other      The other SchemaFormWidget to sync with
     */
    void syncFieldWith(const std::string& memberName, SchemaFormWidget* other);

signals:
    /** Emitted whenever a field value changes (user edit or programmatic). */
    void fieldChanged();

    /** Emitted when the user clicks a field's info button. */
    void documentationRequested(const Documentation& info);

    /** Emitted when the user selects Copy from a CollapsibleSection context menu. */
    void sectionCopyRequested(const QString& key);

    /** Emitted when the user selects Paste from a CollapsibleSection context menu. */
    void sectionPasteRequested(const QString& key);

    /**
     * Emitted when the user clicks the Add or Remove button on an optional field.
     * NOT emitted when setFieldActive() is called programmatically.
     */
    void optionalFieldToggled(const QString& name, bool active);

    /** Emitted when the user clicks a Browse .jasset button on an Identifier field. */
    void browseJassetRequested();

private:
    // -- Constants -----------------------------------------------------------

    /// Size of remove/clear buttons.
    static constexpr int RemoveButtonSize = 22;
    /// UTF-8 multiply sign (x) used as remove/clear glyph.
    static constexpr const char* RemoveGlyph = "\xc3\x97";

    // -- Main infrastructure (schemaformwidget.cpp) -------------------------

    /** Creates the grid layout and populates it with member rows. */
    void buildUi();

    /** Adds a single member's label and widget to the grid at \p row. */
    void addMemberToGrid(
        QGridLayout* grid, int row, int memberIndex,
        const SchemaMember& member
    );

    /** Creates the small (i) documentation button for a member's label column. */
    QPushButton* createInfoButton(
        const SchemaMember& member, QWidget* parent
    );

    /** Wraps a field widget with Add (+) / Remove (x) buttons for optional members. */
    QWidget* createOptionalWrapper(int memberIndex, QWidget* field);

    /** Shows or hides the active row for an optional field by vector index. */
    void setOptionalFieldActive(int memberIndex, bool active);

    // -- Flat widget creators (schemaformwidget_flat.cpp) -------------------

    /** Dispatches to a type-specific creator based on the member's schema type. */
    QWidget* createFlatWidget(const SchemaMember& member);

    /** Creates a QCheckBox for a Boolean member. */
    QWidget* createBooleanWidget(const std::string& name);

    /** Creates a validated QLineEdit for an Integer member. */
    QWidget* createIntegerWidget(const std::string& name,
        const std::string& description);

    /** Creates a validated QLineEdit for a Double member. */
    QWidget* createDoubleWidget(const std::string& name,
        const std::string& description);

    /** Creates a MatrixWidget (or ColorWidget) for vector/matrix/color members. */
    QWidget* createMatrixWidget(const std::string& name,
        const MatrixTypeEntry& entry);

    /** Creates a QLineEdit + Browse button for File or Directory members. */
    QWidget* createFileWidget(const std::string& name,
        const std::string& description, bool isDirectory);

    /** Creates a validated QLineEdit for a "Date and time" member. */
    QWidget* createDateTimeWidget(const std::string& name);

    /** Creates an editable QComboBox + Browse .jasset button for Identifier refs. */
    QWidget* createIdentifierComboWidget(const SchemaMember& member);

    /** Creates a QComboBox for members whose description contains "In list {...}". */
    QWidget* createInListWidget(const std::string& name,
        const QStringList& listOptions);

    /** Creates a type-selector combo + stacked pages for union-typed members. */
    QWidget* createUnionWidget(const SchemaMember& member);

    /** Creates a plain QLineEdit as a fallback for unrecognised types. */
    QWidget* createStringWidget(const SchemaMember& member);

    // -- Table / section builders (schemaformwidget_table.cpp) --------------

    /** Creates a CollapsibleSection for a Table member, dispatching by table kind. */
    QWidget* createTableSection(const SchemaMember& member);

    /** Creates a nested SchemaFormWidget and wires its signals to this form. */
    SchemaFormWidget* createNestedForm(
        const std::vector<SchemaMember>& members, PropertyMap& props);

    /** Emits documentationRequested for a named type within a category. */
    void emitTypeDocumentation(
        const SchemaCategory* category, const QString& typeName);

    /**
     * Replaces the form contents when the user picks a new type from a dropdown.
     * Clears the existing layout, looks up the selected type's schema, stores the
     * type name in the property map, and creates a new nested form populated with
     * any existing property values.
     */
    void rebuildItemForm(
        QVBoxLayout* layout, PropertyMap& props,
        const QString& typeName, const SchemaCategory* category);

    // Ref-array (polymorphic type-dropdown arrays)

    /** Populates a CollapsibleSection with an array of ref-typed items. */
    void buildRefArrayContent(
        CollapsibleSection* section, const SchemaMember& member,
        const std::string& referenceId, PropertyMap& properties);

    /** Builds a single card (type dropdown + form) for one ref-array item. */
    void buildRefArrayCard(
        QVBoxLayout* itemsLayout, const std::string& memberName,
        const SchemaCategory* category, const std::string& referenceId,
        size_t index, PropertyMap& properties);

    /** Clears and rebuilds all ref-array cards from the current property list. */
    void rebuildRefArray(
        QVBoxLayout* itemsLayout, const std::string& memberName,
        const SchemaCategory* category, const std::string& referenceId,
        PropertyMap& properties);

    // Single ref (polymorphic or concrete)

    /** Populates a CollapsibleSection with a concrete or polymorphic ref. */
    void buildRefContent(
        CollapsibleSection* section, const SchemaMember& member,
        const SchemaReference& reference, PropertyMap& properties);

    /** Builds a type-selector dropdown + form for a polymorphic ref. */
    void buildPolymorphicRefContent(
        CollapsibleSection* section, const SchemaMember& member,
        const SchemaReference& reference, const SchemaCategory* category,
        PropertyMap& properties);

    // Inline-member arrays (no type dropdown)

    /** Populates a CollapsibleSection with an array of inline-member items. */
    void buildInlineArrayContent(
        CollapsibleSection* section, const SchemaMember& member,
        PropertyMap& properties);

    /** Builds a single card (nested form + remove button) for one inline array item. */
    void buildInlineArrayCard(
        QVBoxLayout* itemsLayout, const std::string& memberName,
        const std::vector<SchemaMember>& itemMembers, size_t index,
        PropertyMap& properties);

    /** Clears and rebuilds all inline-array cards from the current property list. */
    void rebuildInlineArray(
        QVBoxLayout* itemsLayout, const std::string& memberName,
        const std::vector<SchemaMember>& itemMembers,
        PropertyMap& properties);

    // Inline table (plain nested form)

    /** Populates a CollapsibleSection with a nested form for a plain table member. */
    void buildInlineTableContent(
        CollapsibleSection* section, const SchemaMember& member,
        PropertyMap& properties);

    // -- Data ---------------------------------------------------------------

    /// Schema member descriptors for this form.
    std::vector<SchemaMember> _members;
    /// Bound property map (owned by caller).
    PropertyMap& _properties;

    /// Per-member leaf widget (flat) or CollapsibleSection (Table).
    std::vector<QWidget*> _fieldWidgets;
    /// Per-member "+" button; nullptr for required and Table members.
    std::vector<QWidget*> _fieldAddButtons;
    /// Per-member active row container; nullptr for required and Table members.
    std::vector<QWidget*> _fieldActiveRows;
    /// Whether each optional field is currently active. Always true for required/Table.
    std::vector<bool> _optionalFieldActive;

    /// Whether new sub-sections start expanded.
    bool _subSectionsExpanded = false;
    /// Whether members are sorted in the grid.
    bool _sortMembers = true;
    /// Registry for Identifier-type combo boxes; nullptr if unavailable.
    IdentifierRegistry* _registry = nullptr;
};

#endif // __OPENSPACE_ASSET_BUILDER___FORM_SCHEMAFORMWIDGET___H__
