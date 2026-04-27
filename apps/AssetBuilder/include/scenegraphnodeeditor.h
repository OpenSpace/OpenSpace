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

#ifndef __OPENSPACE_ASSETBUILDER___SCENEGRAPHNODEEDITOR___H__
#define __OPENSPACE_ASSETBUILDER___SCENEGRAPHNODEEDITOR___H__

#include <QWidget>

#include <documentation.h>
#include <jasset.h>
#include <schema/assetschema.h>

class IdentifierRegistry;
class SchemaFormWidget;

/**
 * Builds the entire form for one SceneGraphNode content item. Created fresh each time a
 * content item is selected; the old instance is deleted by EditorPanel.
 */
class SceneGraphNodeEditor final : public QWidget {
Q_OBJECT
public:
    /**
     * Constructs the full SGN editor form for the content item at \p index.
     *
     * \param asset Non-owning pointer to the JAsset
     * \param registry Non-owning pointer to the IdentifierRegistry
     * \param index Index into asset->contents
     * \param parent Parent widget for ownership
     */
    SceneGraphNodeEditor(JAsset* asset, IdentifierRegistry* registry,
        size_t index, QWidget* parent = nullptr);

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
     * Emitted when the user clicks a Browse .jasset button on an Identifier field.
     */
    void browseJassetRequested();

    /**
     * Emitted after a section paste so EditorPanel can rebuild the editor.
     */
    void rebuildRequested();

private:
    void buildUi();

    /**
     * Creates a SchemaFormWidget, connects its signals, and populates it.
     */
    SchemaFormWidget* createForm(const std::vector<SchemaMember>& members,
        PropertyMap& properties, QWidget* parent, bool expanded, bool collapsible);

    /**
     * Connects the GUI Name field to auto-generate the Identifier field.
     */
    void wireIdentifierAutoGeneration(SchemaFormWidget* additionalForm,
        SchemaFormWidget* guiForm);

    // Section builders — create and return widgets without adding
    // to layout. buildUi handles layout order.
    QWidget* buildQuickAccessFields(const SchemaType& sgnType, SchemaFormWidget* guiForm,
        QWidget* additionalSection);
    SchemaFormWidget* buildMemberSection(const SchemaType& sgnType,
        const std::string& memberName, bool expanded);
    QWidget* buildAdditionalSection(const SchemaType& sgnType);

    /**
     * Copies the property data for the given key from the current content item.
     */
    void onSectionCopy(const QString& key);

    /**
     * Pastes stored clipboard data into the given key of the current content item.
     */
    void onSectionPaste(const QString& key);

    /**
     * Sets paste button visibility on all CollapsibleSections in the editor.
     */
    void updatePasteButtons();

    JAsset* _asset = nullptr;
    IdentifierRegistry* _registry = nullptr;
    size_t _index = 0;
};

#endif // __OPENSPACE_ASSETBUILDER___SCENEGRAPHNODEEDITOR___H__
