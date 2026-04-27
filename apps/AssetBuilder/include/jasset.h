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

#ifndef __OPENSPACE_ASSETBUILDER___JASSET___H__
#define __OPENSPACE_ASSETBUILDER___JASSET___H__

#include <map>
#include <string>
#include <variant>
#include <vector>

struct PropertyValue;
class QJsonObject;

/// A map of named property values, used for Table-typed members
using PropertyMap = std::map<std::string, PropertyValue>;

/// A list of property values, used for list-typed members (e.g. Tags)
using PropertyList = std::vector<PropertyValue>;

/**
 * A recursive property value that can hold any type representable in the schema.
 * Corresponds directly to values that will be serialised into the .jasset JSON format.
 */
struct PropertyValue {
    /// The held value; monostate represents null / unset
    std::variant<
        std::monostate,
        std::string,
        double,
        bool,
        PropertyMap,
        PropertyList
    > value;

    bool isNull() const;
    bool isString() const;
    bool isDouble() const;
    bool isBool() const;
    bool isMap() const;
    bool isList() const;

    const std::string& toString() const;
    double toDouble() const;
    bool toBool() const;
    const PropertyMap& toMap() const;
    const PropertyList& toList() const;

    PropertyMap& toMap();
    PropertyList& toList();
};

/**
 * A single item within an asset's contents list. In v1 the type is always
 * "SceneGraphNode". Properties mirror the schema member names exactly, so that
 * serialisation to .jasset JSON requires no key translation.
 */
struct ContentItem {
    /// Schema type name (e.g. "SceneGraphNode")
    std::string type;

    /// All set properties, keyed by their schema member name (e.g. "Identifier", "GUI")
    PropertyMap properties;

    /// Whether this item has unsaved changes (drives the white dot in the contents list)
    bool isDirty = false;
};

/**
 * Top-level metadata stored in the .jasset file's "metadata" block.
 */
struct AssetMetadata {
    /// Human-readable asset name
    std::string name    = "Untitled Asset";
    /// Semantic version string (e.g. "1.0.0")
    std::string version = "1.0.0";
    /// Asset author name
    std::string author;
    /// Short description of the asset
    std::string description;
    /// License identifier (e.g. "MIT", "None")
    std::string license = "None";
};

/**
 * The full in-memory representation of an open .jasset file. Owned by AssetEditorWidget
 * so that each editor tab (future implementation) has its own independent state.
 */
struct JAsset {
    /// Top-level metadata block (name, version, author, etc.)
    AssetMetadata metadata;

    /// Paths to dependency .jasset files. The path type is encoded in the string:
    /// data-relative (e.g. "textures/earth.png"),
    /// jasset-relative (starts with ./ or ../, e.g. "./other.jasset"),
    /// or absolute (e.g. "C:/full/path.jasset").
    std::vector<std::string> dependencies;

    /// The content items in the order they appear in the file
    std::vector<ContentItem> contents;
};

/**
 * Deserializes a JAsset from a parsed JSON object. Pure data transform.
 *
 * \param root Top-level JSON object from a .jasset file
 * \return Populated JAsset with metadata, dependencies, and contents
 */
JAsset jassetFromJson(const QJsonObject& root);

/**
 * Serializes a JAsset to a JSON object. Pure data transform.
 *
 * \param asset The in-memory asset to serialize
 * \return JSON object ready to be written to a .jasset file
 */
QJsonObject jassetToJson(const JAsset& asset);

#endif // __OPENSPACE_ASSETBUILDER___JASSET___H__
