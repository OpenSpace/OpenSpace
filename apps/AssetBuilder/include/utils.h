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

#ifndef __OPENSPACE_ASSETBUILDER___UTILS___H__
#define __OPENSPACE_ASSETBUILDER___UTILS___H__

#include <jasset.h>

#include <filesystem>
#include <string>

/// QSettings key for the persistent OpenSpace data root directory
constexpr const char* SettingsKeyDataRoot = "dataRoot";

class QJsonValue;
class QString;
class QWidget;

struct SchemaType;
struct SchemaMember;

/** Classification of dependency path strings. */
enum class PathType {
    /// Data-relative path (e.g. "textures/earth.png")
    Data,
    /// Jasset-relative path (starts with ./ or ../)
    Relative,
    /// Absolute filesystem path (e.g. "C:/full/path.jasset")
    Absolute
};

/**
 * Detects whether a dependency string is data-relative, jasset-relative, or absolute.
 *
 * \param path The dependency path string
 * \return Classified PathType for the given string
 */
PathType detectPathType(const std::string& path);

/**
 * Resolves a dependency string to an absolute filesystem path.
 * Data-relative paths resolve against \p dataRoot, jasset-relative paths resolve
 * against \p assetDir, and absolute paths are returned as-is (after canonicalization).
 *
 * \param dependency The dependency path string
 * \param dataRoot The OpenSpace data root directory
 * \param assetDirectory The parent directory of the current .jasset file
 * \return Canonicalized absolute path, or the raw resolved path on error
 */
std::filesystem::path resolvePath(
    const std::string& dependency,
    const std::filesystem::path& dataRoot,
    const std::filesystem::path& assetDirectory
);

/**
 * Returns the persisted data root from QSettings.
 *
 * \return Stored data root path, or an empty path if not set
 */
std::filesystem::path dataRoot();

/**
 * Opens a directory picker to choose the data root, persists it in QSettings,
 * and returns the chosen path.
 *
 * \param parent Parent widget for the dialog
 * \return Chosen data root path, or an empty path if the user cancels
 */
std::filesystem::path pickDataRootDialog(QWidget* parent);

/**
 * Converts a display name to PascalCase suitable for use as an identifier.
 * Splits on spaces, hyphens and underscores; removes non-alphanumeric characters.
 *
 * \param name The display name to convert
 * \return PascalCase string (e.g. "My Planet" -> "MyPlanet")
 */
QString toPascalCase(const QString& name);

/**
 * Inserts spaces before uppercase letters to turn PascalCase into readable text.
 * "TimeFrame" -> "Time Frame",  "IsOptional" -> "Is Optional",  "GUI" -> "GUI".
 *
 * \param name The PascalCase identifier to split
 * \return Human-readable string with spaces inserted
 */
QString splitPascalCase(const std::string& name);

/**
 * Converts a QJsonValue to the corresponding PropertyValue tree.
 *
 * \param value The JSON value to convert
 * \return Recursively converted PropertyValue
 */
PropertyValue jsonValueToProperty(const QJsonValue& value);

/**
 * Converts a PropertyValue tree to the corresponding QJsonValue.
 *
 * \param propertyValue The property value to convert
 * \return Recursively converted QJsonValue
 */
QJsonValue propertyToJsonValue(const PropertyValue& propertyValue);

/**
 * Collects members from a SchemaType by name.
 * If \p parentName is empty, searches top-level members.
 * Otherwise, finds the parent member first, then searches its children.
 *
 * \param schemaType  The schema type to search within
 * \param parentName  Parent member name, or empty for top-level lookup
 * \param names       Member names to collect, in desired order
 * \return Vector of matching members in the order of \p names, empty if parent not found
 */
std::vector<SchemaMember> collectMembers(
    const SchemaType& schemaType,
    const std::string& parentName,
    const std::vector<std::string>& names);

#endif // __OPENSPACE_ASSETBUILDER___UTILS___H__
