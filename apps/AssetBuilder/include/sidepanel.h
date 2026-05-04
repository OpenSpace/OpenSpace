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

#ifndef __OPENSPACE_ASSETBUILDER___SIDEPANEL___H__
#define __OPENSPACE_ASSETBUILDER___SIDEPANEL___H__

#include <QWidget>

#include <filesystem>

class ContentsListWidget;
class DependenciesWidget;
struct JAsset;
class MetadataWidget;

/**
 * Thin container for the left-side panel. Owns a vertical QSplitter containing the
 * contents list, dependencies list, and metadata form.
 */
class SidePanel final : public QWidget {
Q_OBJECT
public:
    SidePanel(QWidget* parent, JAsset& asset, std::filesystem::path& path);

    /**
     * Refreshes all three child widgets.
     */
    void refreshAll();

public slots:
    /**
     * Adds the file at \p filePath as a dependency.
     *
     * \param filePath Absolute path to the file to add
     */
    void addDependency(const QString& filePath);

signals:
    /**
     * Emitted when the contents list selection changes.
     */
    void selectionChanged(size_t row);

    /**
     * Emitted whenever a child widget mutates the asset.
     */
    void assetModified();

private:
    ContentsListWidget* _contentsList = nullptr;
    DependenciesWidget* _dependencies = nullptr;
    MetadataWidget* _metadata = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___SIDEPANEL___H__
