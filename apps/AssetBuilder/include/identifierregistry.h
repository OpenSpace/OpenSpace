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

#ifndef __OPENSPACE_ASSETBUILDER___IDENTIFIERREGISTRY___H__
#define __OPENSPACE_ASSETBUILDER___IDENTIFIERREGISTRY___H__

#include <QObject>

#include <QStringList>
#include <filesystem>

struct JAsset;

/**
 * Maintains a list of all known identifiers from the current asset's content items and
 * from parsed dependency files. Used to populate Identifier-type comboboxes in the schema
 * form.
 */
class IdentifierRegistry final : public QObject {
Q_OBJECT
public:
    /**
     * Constructs an empty registry. The parent defaults to nullptr to follow Qt
     * convention, though in practice a parent is always passed for lifetime management.
     *
     * \param parent Optional parent QObject for lifetime management
     */
    explicit IdentifierRegistry(QObject* parent);

    /**
     * Rebuilds the identifier list from the given asset. Collects identifiers from local
     * content items and from each dependency file (resolved relative to the parent
     * directory of assetPath). Emits registryChanged() only if the set actually changed.
     *
     * \param asset The current in-memory asset
     * \param assetPath Path to the .jasset file (empty if untitled)
     */
    void rebuildFromAsset(const JAsset& asset, const std::filesystem::path& assetPath);

    /**
     * Returns the sorted, deduplicated list of all known identifiers.
     */
    QStringList knownIdentifiers() const;

signals:
    /**
     * Emitted when the identifier set changes after a rebuild.
     */
    void registryChanged();

    /**
     * Emitted when duplicate identifiers are found across the asset's content items and
     * dependency files.
     *
     * \param message Human-readable description of each duplicate and its sources
     */
    void duplicatesFound(const QString& message);

private:
    /// Sorted, deduplicated list of all known identifiers
    QStringList _identifiers;

    /// Last emitted duplicate warning (to avoid repeated popups)
    QString _lastDuplicateWarning;
};

#endif // __OPENSPACE_ASSETBUILDER___IDENTIFIERREGISTRY___H__
