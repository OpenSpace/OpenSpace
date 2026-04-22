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

#ifndef __OPENSPACE_ASSET_BUILDER___SCHEMA_ASSETSCHEMA___H__
#define __OPENSPACE_ASSET_BUILDER___SCHEMA_ASSETSCHEMA___H__

#include <optional>
#include <string>
#include <vector>

/**
 * Points to another SchemaType in a different SchemaCategory, used when a member's
 * type is "Table" and its contents are defined by a separate named class.
 */
struct SchemaReference {
    /// The identifier of the referenced SchemaType (e.g. "renderable")
    std::string identifier;

    /// The human-readable name of the referenced class (e.g. "Renderable")
    std::string name;

    /// Whether the referenced class was actually found in the schema
    bool isFound = false;
};

/**
 * A single configurable member of a SchemaType. May be a leaf value (Boolean, String,
 * Double, Identifier) or a table (can contain members or be a reference to another
 * SchemaCategory).
 */
struct SchemaMember {
    /// Display name of this member (e.g. "Identifier", "Color")
    std::string name;

    /// Type string from the schema JSON (e.g. "Boolean", "String", "Double",
    /// "Identifier", "Table", "Color3")
    std::string type;

    /// `false` = required (must be set), `true` = optional
    bool isOptional = false;

    /// Full documentation text shown in the documentation panel
    std::string documentation;

    /// Short description, used to describe type or ranges
    std::string description;

    /// Set when type == "Table" and this member references a type in another category
    /// e.g. Renderable, LabelsComponent
    std::optional<SchemaReference> reference;

    /// Set when type == "Table" and the sub-members are defined inline in the schema
    /// (e.g. the GUI sub-table on SceneGraphNode, Transform)
    std::vector<SchemaMember> members;
};

/**
 * A single type within a SchemaCategory (e.g. "RenderableGlobe" within "Renderable").
 * Defines all members (required and optional) for that type.
 */
struct SchemaType {
    /// Display name of this type (e.g. "SceneGraphNode", "RenderableGlobe")
    std::string name;

    /// Unique identifier used for lookup (e.g. "core_scene_node", "renderable_globe")
    std::string identifier;

    /// Human-readable description of what this class represents
    std::string description;

    /// All members of this class, both required and optional
    std::vector<SchemaMember> members;
};

/**
 * A top-level category grouping related types (e.g. "Renderable" groups all 55
 * renderable types). Corresponds to one top-level entry in assetComponents.json.
 */
struct SchemaCategory {
    /// Display name of this category (e.g. "Renderable", "Rotation")
    std::string name;

    /// Identifier used for lookup (e.g. "categoryRenderable", "categoryRotation")
    std::string identifier;

    /// All types within this category
    std::vector<SchemaType> types;
};

/**
 * Singleton that loads and holds the full OpenSpace asset component schema from the
 * bundled Qt resource assetComponents.json. All schema data is read-only
 * after loading and shared across the entire application.
 */
class AssetSchema {
public:
    /**
     * Returns the singleton instance.
     */
    static AssetSchema& instance();

    /**
     * Parses the schema resource into the in-memory schema representation.
     * Safe to call multiple times; subsequent calls return immediately.
     *
     * \throw std::runtime_error if the resource cannot be opened or parsed
     */
    void loadFromResource();

    /**
     * Returns the category with the given identifier, or `nullptr` if not found.
     *
     * \param identifier The category identifier (e.g. "categoryRenderable")
     * \return Pointer to the matching SchemaCategory, or nullptr
     */
    const SchemaCategory* findCategory(const std::string& identifier) const;

    /**
     * Searches all categories for a type with the given identifier.
     *
     * \param identifier The type identifier (e.g. "core_scene_node")
     * \return Pointer to the matching SchemaType, or nullptr
     */
    const SchemaType* findType(const std::string& identifier) const;

    /**
     * Searches all categories for a type with the given identifier and returns
     * the category containing it, or `nullptr` if not found.
     *
     * \param typeId The type identifier (e.g. "renderable", "core_time_frame")
     * \return Pointer to the SchemaCategory containing the type, or nullptr
     */
    const SchemaCategory* findCategoryByTypeId(const std::string& typeId) const;

    /**
     * Returns all loaded schema categories.
     *
     * \return Reference to the list of all SchemaCategories
     */
    const std::vector<SchemaCategory>& categories() const;

private:
    AssetSchema() = default;
    AssetSchema(const AssetSchema&) = delete;
    AssetSchema& operator=(const AssetSchema&) = delete;

    std::vector<SchemaCategory> _categories;
    bool _isLoaded = false;
};

#endif // __OPENSPACE_ASSET_BUILDER___SCHEMA_ASSETSCHEMA___H__
