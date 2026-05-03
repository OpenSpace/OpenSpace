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

#include "identifierregistry.h"

#include "jasset.h"
#include "path.h"
#include <ghoul/filesystem/filesystem.h>
#include <QFile>
#include <QJsonDocument>
#include <QJsonObject>
#include <QMap>

IdentifierRegistry::IdentifierRegistry(QObject* parent)
    : QObject(parent)
{}

void IdentifierRegistry::rebuildFromAsset(const JAsset& asset,
                                          const std::filesystem::path& assetPath)
{
    const std::filesystem::path assetDir = assetPath.parent_path();

    // Map each identifier to the list of sources where it was found
    QMap<QString, QStringList> sources;

    // Collect identifiers from local content items
    const QString localSource = "this asset";
    for (const ContentItem& item : asset.contents) {
        const auto it = item.properties.find("Identifier");
        if (it != item.properties.end() && it->second.isString()) {
            const QString id = QString::fromStdString(it->second.toString());
            sources[id].append(localSource);
        }
    }

    // Collect identifiers from dependency files
    for (const std::string& dep : asset.dependencies) {
        const std::filesystem::path depPath = resolvePath(
            dep,
            absPath("${USER_ASSETS}"),
            assetDir
        );

        if (depPath.extension() != ".jasset") {
            continue;
        }

        QFile file = QFile(QString::fromStdWString(depPath.wstring()));
        if (!file.open(QFile::ReadOnly)) {
            continue;
        }

        QJsonParseError err;
        const QJsonDocument doc = QJsonDocument::fromJson(file.readAll(), &err);
        if (doc.isNull() || !doc.isObject()) {
            continue;
        }

        const JAsset depAsset = jassetFromJson(doc.object());
        const QString depSource = QString::fromStdString(dep);
        for (const ContentItem& item : depAsset.contents) {
            const auto it = item.properties.find("Identifier");
            if (it != item.properties.end() && it->second.isString()) {
                const QString id = QString::fromStdString(it->second.toString());
                sources[id].append(depSource);
            }
        }
    }

    // Build the deduplicated identifier list
    QStringList ids = sources.keys();
    ids.sort();

    if (ids != _identifiers) {
        _identifiers = ids;
        emit registryChanged();
    }

    // Report any identifiers that appear in more than one source
    QStringList warnings;
    for (auto it = sources.constBegin(); it != sources.constEnd(); it++) {
        // If there are more than 1 of an identifier, add to the list
        if (it.value().size() > 1) {
            warnings.append(
                QString("\"%1\" is defined in:\n  - %2")
                    .arg(it.key()).arg(it.value().join("\n  - "))
            );
        }
    }
    const QString message = warnings.isEmpty() ?
        QString() :
        "Duplicate identifiers found:\n\n" + warnings.join("\n\n");
    if (message != _lastDuplicateWarning) {
        _lastDuplicateWarning = message;
        if (!message.isEmpty()) {
            emit duplicatesFound(message);
        }
    }
}

QStringList IdentifierRegistry::knownIdentifiers() const {
    return _identifiers;
}
