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

#ifndef __OPENSPACE_ASSETBUILDER___CONTENTSLISTWIDGET___H__
#define __OPENSPACE_ASSETBUILDER___CONTENTSLISTWIDGET___H__

#include <QWidget>

struct JAsset;
class QListWidget;

/**
 * Widget showing the contents list with add/duplicate/remove actions. Owns the
 * QListWidget and all content-item mutation logic.
 */
class ContentsListWidget final : public QWidget {
Q_OBJECT
public:
    explicit ContentsListWidget(QWidget* parent = nullptr);

    /**
     * Sets the asset pointer used for editing.
     *
     * \param asset Non-owning pointer to the JAsset
     */
    void setAsset(JAsset* asset);

    /**
     * Rebuilds the contents list from the current asset.
     */
    void refresh();

signals:
    /**
     * Emitted when the contents list selection changes.
     */
    void selectionChanged(int row);

    /**
     * Emitted whenever this widget mutates the asset.
     */
    void assetModified();

private:
    void buildUi();

    /**
     * Appends a new default SceneGraphNode and selects it.
     */
    void addSceneGraphNode();

    /**
     * Duplicates the content item at the given row index.
     *
     * \param row Index into asset contents
     */
    void duplicateSceneGraphNode(int row);

    /**
     * Removes the content item at the given row index after confirmation.
     *
     * \param row Index into asset contents
     */
    void removeSceneGraphNode(int row);

    JAsset* _asset = nullptr;
    QListWidget* _contentsList = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___CONTENTSLISTWIDGET___H__
