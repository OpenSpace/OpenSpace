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

#include "utils.h"

#include <schema/assetschema.h>

#include <QDir>
#include <QFileDialog>
#include <QJsonArray>
#include <QJsonObject>
#include <QJsonValue>
#include <QSettings>
#include <QString>

PathType detectPathType(const std::string& path) {
    // Jasset-relative: ./foo, ../foo
    if (path.starts_with("./") || path.starts_with("../")) {
        return PathType::Relative;
    }
    // Absolute: C:/ on Windows, / on Unix
    if (std::filesystem::path(path).is_absolute()) {
        return PathType::Absolute;
    }
    return PathType::Data;
}

std::filesystem::path resolvePath(
    const std::string& dependency,
    const std::filesystem::path& dataRoot,
    const std::filesystem::path& assetDirectory)
{
    const PathType type = detectPathType(dependency);
    std::filesystem::path resolved(dependency);

    // Resolve relative paths against the appropriate root directory
    if (type == PathType::Data && !dataRoot.empty()) {
        resolved = dataRoot / resolved;
    }
    else if (type == PathType::Relative && !assetDirectory.empty()) {
        resolved = assetDirectory / resolved;
    }

    // Normalize the path (resolve .. and . segments)
    std::error_code error;
    std::filesystem::path canonical =
        std::filesystem::weakly_canonical(resolved, error);
    return error ? resolved : canonical;
}

std::filesystem::path dataRoot() {
    const QString value = QSettings().value(SettingsKeyDataRoot).toString();
    return value.isEmpty()
        ? std::filesystem::path{}
        : std::filesystem::path(value.toStdWString());
}

std::filesystem::path pickDataRootDialog(QWidget* parent) {
    const std::filesystem::path current = dataRoot();
    const QString startDirectory = current.empty()
        ? QDir::homePath()
        : QString::fromStdWString(current.wstring());

    const QString selected = QFileDialog::getExistingDirectory(
        parent, "Set Data Directory", startDirectory
    );
    if (selected.isEmpty()) {
        return {};
    }

    QSettings().setValue(SettingsKeyDataRoot, selected);
    return std::filesystem::path(selected.toStdWString());
}

QString splitPascalCase(const std::string& name) {
    const QString text = QString::fromStdString(name);
    QString result;
    for (int i = 0; i < text.size(); ++i) {
        const QChar character = text[i];
        // Insert a space before an uppercase letter when it marks a word boundary:
        if (i > 0 && character.isUpper()) {
            const bool previousLower = text[i - 1].isLower();
            const bool previousUpper = text[i - 1].isUpper();
            const bool nextLower = (i + 1 < text.size()) && text[i + 1].isLower();
            // Current character is upper; previous character is lower.
            // This is a camelCase space (e.g. "Time|Frame" -> "Time Frame")
            const bool camelCaseBoundary = previousLower;
            // previousUpper && nextLower: end of acronym run, split before the
            //   new word (e.g. "GUI|Name" -> "GUI Name"), but keep consecutive
            //   uppercase together (e.g. "GUI" stays as "GUI")
            const bool endOfAcronym = previousUpper && nextLower;
            if (camelCaseBoundary|| endOfAcronym) {
                result += ' ';
            }
        }
        result += character;
    }
    return result;
}

QString toPascalCase(const QString& name) {
    QString result;
    bool capitalizeNext = true;
    for (const QChar character : name) {
        if (character == ' ' || character == '-' || character == '_') {
            capitalizeNext = true;
        }
        else if (character.isLetterOrNumber()) {
            result += capitalizeNext ? character.toUpper() : character;
            capitalizeNext = false;
        }
    }
    return result;
}

PropertyValue jsonValueToProperty(const QJsonValue& value) {
    if (value.isBool()) {
        return PropertyValue{ value.toBool() };
    }
    if (value.isDouble()) {
        return PropertyValue{ value.toDouble() };
    }
    if (value.isString()) {
        return PropertyValue{ value.toString().toStdString() };
    }
    if (value.isObject()) {
        PropertyMap map;
        const QJsonObject object = value.toObject();
        for (auto it = object.begin(); it != object.end(); ++it) {
            map[it.key().toStdString()] =
                jsonValueToProperty(it.value());
        }
        return PropertyValue{ std::move(map) };
    }
    if (value.isArray()) {
        PropertyList list;
        const QJsonArray array = value.toArray();
        for (const QJsonValue& item : array) {
            list.push_back(jsonValueToProperty(item));
        }
        return PropertyValue{ std::move(list) };
    }
    return PropertyValue{};
}

QJsonValue propertyToJsonValue(const PropertyValue& propertyValue) {
    if (propertyValue.isNull())   { return QJsonValue::Null; }
    if (propertyValue.isBool())   { return QJsonValue(propertyValue.toBool()); }
    if (propertyValue.isDouble()) { return QJsonValue(propertyValue.toDouble()); }
    if (propertyValue.isString()) {
        return QJsonValue(
            QString::fromStdString(propertyValue.toString())
        );
    }
    if (propertyValue.isMap()) {
        QJsonObject object;
        for (const auto& [key, value] : propertyValue.toMap()) {
            object[QString::fromStdString(key)] =
                propertyToJsonValue(value);
        }
        return object;
    }
    if (propertyValue.isList()) {
        QJsonArray array;
        for (const PropertyValue& item : propertyValue.toList()) {
            array.append(propertyToJsonValue(item));
        }
        return array;
    }
    return QJsonValue::Null;
}

std::vector<SchemaMember> collectMembers(
    const SchemaType& schemaType,
    const std::string& parentName,
    const std::vector<std::string>& names)
{
    const std::vector<SchemaMember>* source = &schemaType.members;
    if (!parentName.empty()) {
        source = nullptr;
        for (const SchemaMember& member : schemaType.members) {
            if (member.name == parentName) {
                source = &member.members;
                break;
            }
        }
        // No top level member found; return empty
        if (!source) {
            return {};
        }
    }

    std::vector<SchemaMember> result;
    for (const std::string& name : names) {
        for (const SchemaMember& member : *source) {
            if (member.name == name) {
                result.push_back(member);
                break;
            }
        }
    }
    return result;
}
