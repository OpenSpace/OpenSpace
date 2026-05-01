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

#ifndef __OPENSPACE_ASSETBUILDER___EDITORPANEL___H__
#define __OPENSPACE_ASSETBUILDER___EDITORPANEL___H__

#include <QWidget>
#include <documentation.h>

class IdentifierRegistry;
struct JAsset;
class QStackedWidget;

/**
 * Center panel containing the form editor for a single content item. Manages a
 * QStackedWidget with an empty-state page and a dynamically rebuilt editor page.
 * Delegates the actual form building to SceneGraphNodeEditor.
 */
class EditorPanel final : public QWidget {
Q_OBJECT
public:
    explicit EditorPanel(QWidget* parent = nullptr);

    /**
     * Sets the asset pointer used by showItemEditor. Must be called before
     * showItemEditor. The pointer must remain valid.
     *
     * \param asset Non-owning pointer to the JAsset
     */
    void setAsset(JAsset* asset);

    /**
     * Sets the identifier registry used for populating Identifier-type comboboxes.
     *
     * \param registry Non-owning pointer to the IdentifierRegistry
     */
    void setIdentifierRegistry(IdentifierRegistry* registry);

    /**
     * Replaces the editor page with a form for the given content item.
     *
     * \param index Index into _asset->contents
     */
    void showItemEditor(size_t index);

    /**
     * Switches the stack to the empty-state page.
     */
    void showEmptyCenter();

signals:
    /**
     * Emitted whenever a field change makes the asset dirty.
     */
    void contentModified();

    /**
     * Emitted when the user clicks a field info button to view its documentation.
     *
     * \param info Documentation bundle with name, type, description, and documentation
     */
    void documentationRequested(const Documentation& info);

    /**
     * Emitted when the user selects a .jasset file via the Browse button on an
     * Identifier field. Carries the absolute file path to add as a dependency.
     *
     * \param filePath Absolute path to the selected .jasset file
     */
    void addDependency(const QString& filePath);

private:
    void buildUi();
    QWidget* createContentEditor(size_t index, QWidget* parent);

    static constexpr size_t NoSelection = static_cast<size_t>(-1);

    JAsset* _asset = nullptr;
    IdentifierRegistry* _registry = nullptr;
    QStackedWidget* _centerStack = nullptr;
    size_t _currentIndex = NoSelection;
};

#endif // __OPENSPACE_ASSETBUILDER___EDITORPANEL___H__
