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

#ifndef __OPENSPACE_ASSETBUILDER___DEPENDENCIESWIDGET___H__
#define __OPENSPACE_ASSETBUILDER___DEPENDENCIESWIDGET___H__

#include <QWidget>

#include "path.h"
#include <filesystem>

struct JAsset;
class QListWidget;

/**
 * Widget showing the dependencies list with add/remove and path-conversion context menu
 * actions.
 */
class DependenciesWidget final : public QWidget {
Q_OBJECT
public:
    DependenciesWidget(QWidget* parent, JAsset& asset, std::filesystem::path& path);

    /**
     * Rebuilds the dependencies list from the current asset.
     */
    void refresh();

public slots:
    /**
     * Opens a file dialog and adds the chosen file as a dependency.
     */
    void addDependencyViaDialog();

    /**
     * Adds the file at \p filePath as a dependency. The path is stored in the first
     * format that applies:
     *   1. Data-relative (if a data root is set and the file is inside it)
     *   2. Jasset-relative (if the asset file has been saved)
     *   3. Absolute (last resort)
     * Checks for duplicates and emits assetModified on success.
     *
     * \param filePath Absolute path to the file to add
     */
    void addDependency(const QString& filePath);

signals:
    /**
     * Emitted whenever this widget mutates the asset.
     */
    void assetModified();

private:
    void showContextMenu(const QPoint& pos);

    /**
     * Removes the dependency at the given row index.
     *
     * \param row Index into asset dependencies
     */
    void removeDependency(size_t row);

    /**
     * Converts the dependency at the given row to a different path type.
     *
     * \param row Index into asset dependencies
     * \param target Target path type
     */
    void convertDependencyPath(size_t row, PathType target);

    JAsset& _asset;
    std::filesystem::path& _filePath;
    QListWidget* _dependenciesList = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___DEPENDENCIESWIDGET___H__
