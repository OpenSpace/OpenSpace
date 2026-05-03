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

#include "jasset.h"

#include "utils.h"
#include <QJsonArray>
#include <QJsonObject>

namespace {
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
            for (auto it = object.begin(); it != object.end(); it++) {
                map[it.key().toStdString()] = jsonValueToProperty(it.value());
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
        if (propertyValue.isNull()) {
            return QJsonValue::Null;
        }
        if (propertyValue.isBool()) {
            return QJsonValue(propertyValue.toBool());
        }
        if (propertyValue.isDouble()) {
            return QJsonValue(propertyValue.toDouble());
        }
        if (propertyValue.isString()) {
            return QJsonValue(QString::fromStdString(propertyValue.toString()));
        }
        if (propertyValue.isMap()) {
            QJsonObject object;
            for (const auto& [key, value] : propertyValue.toMap()) {
                object[QString::fromStdString(key)] = propertyToJsonValue(value);
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
} // namespace

bool PropertyValue::isNull() const {
    return std::holds_alternative<std::monostate>(value);
}

bool PropertyValue::isString() const {
    return std::holds_alternative<std::string>(value);
}

bool PropertyValue::isDouble() const {
    return std::holds_alternative<double>(value);
}

bool PropertyValue::isBool() const {
    return std::holds_alternative<bool>(value);
}

bool PropertyValue::isMap() const {
    return std::holds_alternative<PropertyMap>(value);
}

bool PropertyValue::isList() const {
    return std::holds_alternative<PropertyList>(value);
}

const std::string& PropertyValue::toString() const {
    return std::get<std::string>(value);
}

double PropertyValue::toDouble() const {
    return std::get<double>(value);
}

bool PropertyValue::toBool() const {
    return std::get<bool>(value);
}

const PropertyMap& PropertyValue::toMap() const {
    return std::get<PropertyMap>(value);
}

const PropertyList& PropertyValue::toList() const {
    return std::get<PropertyList>(value);
}

PropertyMap& PropertyValue::toMap() {
    return std::get<PropertyMap>(value);
}

PropertyList& PropertyValue::toList() {
    return std::get<PropertyList>(value);
}

JAsset jassetFromJson(const QJsonObject& root) {
    JAsset asset;

    if (root.contains("metadata")) {
        const QJsonObject meta = root["metadata"].toObject();
        asset.metadata.name = meta["name"].toString("Untitled Asset").toStdString();
        asset.metadata.version = meta["asset_version"].toString("1.0.0").toStdString();
        asset.metadata.author = meta["author"].toString().toStdString();
        asset.metadata.description = meta["description"].toString().toStdString();
        asset.metadata.license = meta["license"].toString("None").toStdString();
    }

    if (root.contains("dependencies")) {
        for (const QJsonValue& dep : root["dependencies"].toArray()) {
            asset.dependencies.push_back(dep.toString().toStdString());
        }
    }

    if (root.contains("contents")) {
        for (const QJsonValue& itemVal : root["contents"].toArray()) {
            if (!itemVal.isObject()) {
                continue;
            }
            const QJsonObject itemObj = itemVal.toObject();
            ContentItem item;
            item.type = itemObj["type"].toString().toStdString();
            // Store all keys except "type" as properties — schema-agnostic,
            // so the parser doesn't need to know the type's members
            for (auto it = itemObj.begin(); it != itemObj.end(); it++) {
                if (it.key() == "type") { continue; }
                item.properties[it.key().toStdString()] = jsonValueToProperty(it.value());
            }
            asset.contents.push_back(std::move(item));
        }
    }

    return asset;
}

QJsonObject jassetToJson(const JAsset& asset) {
    QJsonObject root;
    root["jasset_version"] = "1.0";

    QJsonObject meta;
    meta["name"] = QString::fromStdString(asset.metadata.name);
    meta["asset_version"] = QString::fromStdString(asset.metadata.version);
    meta["author"] = QString::fromStdString(asset.metadata.author);
    meta["description"] = QString::fromStdString(asset.metadata.description);
    meta["license"] = QString::fromStdString(asset.metadata.license);
    root["metadata"] = meta;

    QJsonArray deps;
    for (const std::string& dep : asset.dependencies) {
        deps.append(QString::fromStdString(dep));
    }
    root["dependencies"] = deps;

    QJsonArray contents;
    for (const ContentItem& item : asset.contents) {
        QJsonObject itemObj;
        itemObj["type"] = QString::fromStdString(item.type);
        for (const auto& [key, value] : item.properties) {
            itemObj[QString::fromStdString(key)] =
                propertyToJsonValue(value);
        }
        contents.append(itemObj);
    }
    root["contents"] = contents;

    return root;
}
