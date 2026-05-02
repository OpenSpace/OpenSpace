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

#include "schema/assetschema.h"

#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <format>
#include <stdexcept>

namespace {
    constexpr const char* SchemaResourcePath = ":/schema/assetComponents.json";

    constexpr const char* KeyName = "name";
    constexpr const char* KeyIdentifier = "identifier";
    constexpr const char* KeyDescription = "description";
    constexpr const char* KeyDocumentation = "documentation";
    constexpr const char* KeyMembers = "members";
    constexpr const char* KeyClasses = "classes";
    constexpr const char* KeyType = "type";
    constexpr const char* KeyOptional = "optional";
    constexpr const char* KeyReference = "reference";
    constexpr const char* KeyFound = "found";

    SchemaMember parseMember(const QJsonObject& obj) {
        SchemaMember member = {
            .name = obj[KeyName].toString().toStdString(),
            .type = obj[KeyType].toString().toStdString(),
            .isOptional = obj[KeyOptional].toInt() != 0,
            .documentation = obj[KeyDocumentation].toString().toStdString(),
            .description = obj[KeyDescription].toString().toStdString()
        };

        if (obj.contains(KeyReference)) {
            const QJsonObject refObj = obj[KeyReference].toObject();
            SchemaReference ref = {
                .identifier = refObj[KeyIdentifier].toString().toStdString(),
                .name = refObj[KeyName].toString().toStdString(),
                .isFound = refObj[KeyFound].toBool()
            };
            member.reference = std::move(ref);
        }

        if (obj.contains(KeyMembers)) {
            const QJsonArray membersArray = obj[KeyMembers].toArray();
            for (const QJsonValue& memberVal : membersArray) {
                member.members.push_back(parseMember(memberVal.toObject()));
            }
        }

        return member;
    }

    SchemaType parseType(const QJsonObject& obj) {
        SchemaType type = {
            .name = obj[KeyName].toString().toStdString(),
            .identifier = obj[KeyIdentifier].toString().toStdString(),
            .description = obj[KeyDescription].toString().toStdString()
        };

        const QJsonArray members = obj[KeyMembers].toArray();
        for (const QJsonValue& memberVal : members) {
            type.members.push_back(parseMember(memberVal.toObject()));
        }

        return type;
    }

    std::vector<SchemaCategory> loadCategories() {
        std::vector<SchemaCategory> result;
        QFile file(SchemaResourcePath);
        if (!file.open(QFile::ReadOnly)) {
            throw std::runtime_error(std::format(
                "Could not open schema resource {}", SchemaResourcePath
            ));
        }
        const QByteArray data = file.readAll();
        QJsonParseError parseError;
        const QJsonDocument doc = QJsonDocument::fromJson(data, &parseError);
        if (doc.isNull()) {
            throw std::runtime_error(std::format(
                "Failed to parse {}: {}",
                SchemaResourcePath, parseError.errorString().toStdString()
            ));
        }
        if (!doc.isArray()) {
            throw std::runtime_error(std::format(
                "{} must be a JSON array at the top level", SchemaResourcePath
            ));
        }
        const QJsonArray categoriesArray = doc.array();
        result.reserve(categoriesArray.size());
        // Read the JSON into the structs
        for (const QJsonValue& categoryVal : categoriesArray) {
            const QJsonObject categoryObj = categoryVal.toObject();
            SchemaCategory category = {
                .name = categoryObj[KeyName].toString().toStdString(),
                .identifier = categoryObj[KeyIdentifier].toString().toStdString()
            };
            const QJsonArray types = categoryObj[KeyClasses].toArray();
            category.types.reserve(types.size());
            for (const QJsonValue& typeVal : types) {
                category.types.push_back(parseType(typeVal.toObject()));
            }
            result.push_back(std::move(category));
        }
        return result;
    }
} // namespace

const AssetSchema& AssetSchema::instance() {
    static AssetSchema schema;
    return schema;
}

AssetSchema::AssetSchema()
    : _categories(loadCategories())
{}

const SchemaCategory* AssetSchema::findCategory(const std::string& identifier) const {
    for (const SchemaCategory& category : _categories) {
        if (category.identifier == identifier) {
            return &category;
        }
    }
    return nullptr;
}

const SchemaType* AssetSchema::findType(const std::string& identifier) const {
    for (const SchemaCategory& category : _categories) {
        for (const SchemaType& type : category.types) {
            if (type.identifier == identifier) {
                return &type;
            }
        }
    }
    return nullptr;
}

const SchemaCategory* AssetSchema::findCategoryByTypeId(const std::string& id) const {
    for (const SchemaCategory& category : _categories) {
        for (const SchemaType& type : category.types) {
            if (type.identifier == id) {
                return &category;
            }
        }
    }
    return nullptr;
}

const std::vector<SchemaCategory>& AssetSchema::categories() const {
    return _categories;
}
