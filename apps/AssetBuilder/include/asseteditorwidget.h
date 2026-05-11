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

#ifndef __OPENSPACE_ASSETBUILDER___ASSETEDITORWIDGET___H__
#define __OPENSPACE_ASSETBUILDER___ASSETEDITORWIDGET___H__

#include <QWidget>

#include <jasset.h>
#include <filesystem>

class DocumentationPanel;
class EditorPanel;
class IdentifierRegistry;
class SidePanel;

/**
 * Coordinator widget for the three-panel .jasset editor. Owns the asset data and wires
 ( signals between SidePanel, EditorPanel, and DocumentationPanel.
 *
 * Layout: left drawer (contents + deps) | center form | right drawer (docs)
 */
class AssetEditorWidget final : public QWidget {
Q_OBJECT
public:
    explicit AssetEditorWidget(QWidget* parent);

    /**
     * Resets the editor to an empty, untitled asset.
     */
    void newAsset();

    /**
     * Loads the given .jasset file into the editor.
     *
     * \param path Path to the .jasset file to load
     * \return `true` on success, `false` if the file could not be read or parsed
     */
    bool loadAsset(const std::filesystem::path& path);

    /**
     * Saves the current in-memory asset to disk.
     *
     * \param path Destination path for the .jasset file
     * \return `true` on success, `false` if serialisation or writing failed
     */
    bool saveAsset(const std::filesystem::path& path);

    /**
     * Returns true when the in-memory asset differs from the last saved state.
     */
    bool isDirty() const;

    /**
     * Returns the path of the currently open .jasset file (empty if untitled).
     */
    const std::filesystem::path& filePath() const;

    /**
     * Returns a short display name: "file.jasset" (+ " *" if having unsaved changes), or
     * "Untitled".
     */
    QString displayName() const;

    /**
     * Returns the full native-separator path (+ " *" if having unsaved changes), or
     * "Untitled".
     */
    QString displayPath() const;

    /**
     * Syncs all panels and the identifier registry to the current state.
     */
    void refreshPanels();

signals:
    /**
     * Emitted whenever a change is made to the in-memory asset.
     */
    void assetModified();

private slots:
    /**
     * Updates the center panel to reflect the current contents-list selection.
     *
     * \param row The selected row index
     */
    void onSelectionChanged(size_t row);

    /**
     * Called when either panel reports a change to the asset.
     */
    void onContentModified();

private:
    // Asset data and file path
    JAsset _asset;
    std::filesystem::path _filePath;

    bool _isDirty = false;

    SidePanel* _sidePanel = nullptr;
    EditorPanel* _editorPanel = nullptr;
    DocumentationPanel* _docsPanel = nullptr;
    IdentifierRegistry* _identifierRegistry = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___ASSETEDITORWIDGET___H__
