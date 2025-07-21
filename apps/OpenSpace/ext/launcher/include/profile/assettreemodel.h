/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_UI_LAUNCHER___ASSETTREEMODEL___H__
#define __OPENSPACE_UI_LAUNCHER___ASSETTREEMODEL___H__

#include <QAbstractItemModel>

#include "assettreeitem.h"
#include "openspace/scene/profile.h"
#include <memory>

class AssetTreeModel final : public QAbstractItemModel {
Q_OBJECT
public:
    AssetTreeModel(QObject* parent = nullptr);

    /**
      * Returns the data contained at an index.
      *
      * \param index The index defining where the item is located in the tree model
      * \param role Qt-defined role that describes the reason Qt is calling the function
      *        (can be multiple times)
      * \return QVariant data object
      */
    QVariant data(const QModelIndex& index, int role) const final;

    /**
      * Returns the header data of the tree view.
      *
      * \param section The data to be obtained from header
      * \param orientation The orientation of the query (e.g. Qt::horizontal)
      * \param role Qt-defined role that describes the reason Qt is calling the function
      *        (can be multiple times)
      * \return QVariant data object in the header
      */
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const final;

    /**
      * Returns the index of item in QModelIndex object form.
      *
      * \param row The row number
      * \param column The column number
      * \param parent Index of parent
      * \return QModelIndex index of the item at specified position
      */
    QModelIndex index(int row, int column,
        const QModelIndex& parent = QModelIndex()) const final;

    /**
      * Returns the index of the parent of the item specified by input param.
      *
      * \param index The index of item that is a child of the parent
      * \return QModelIndex index of the parent
      */
    QModelIndex parent(const QModelIndex& index) const final;

    /**
      * Returns the index of the parent of the item specified by the input params.
      *
      * \param row The row number
      * \param column The column number
      * \param parent Index of parent
      * \return QModelIndex index of the parent
      */
    QModelIndex parent(int row, int column,
        const QModelIndex& parent = QModelIndex()) const;

    /**
      * Returns asset item at specified index.
      *
      * \param index The index of item that is a child of the parent
      * \return Pointer to the item at the provided index
      */
    AssetTreeItem* assetItem(const QModelIndex& index);

    /**
      * Returns number of children/rows of the parent.
      *
      * \param parent QModelIndex of the parent item
      * \return The number of children/rows of this parent
      */
    int rowCount(const QModelIndex& parent = QModelIndex()) const final;

    /**
      * Returns the number of columns of data in each item of the tree.
      *
      * \param parent The parent specified by the QModelIndex index
      * \return The number of data columns
      */
    int columnCount(const QModelIndex& parent = QModelIndex()) const final;

    /**
      * Return the Qt flags of the item specified by index, which can include
      * Qt::ItemIsEnabled, Qt::ItemIsSelectable.
      *
      * \param index The index specified by the QModelIndex index
      * \return The Qt flags
      */
    Qt::ItemFlags flags(const QModelIndex& index) const final;

    /**
      * Set data at index \p index.
      *
      * \param index Location of the item to set
      * \param value The QVariant data element to store at column \p column
      * \param role Qt-specific role to define context of the call
      * \return `true` if the data set was successful
      */
    bool setData(const QModelIndex& index, const QVariant& value,
        int role = Qt::EditRole) final;

    /**
      * Returns a vector of all #Asset%s selected in the tree view.
      *
      * \param outputPaths The vector of paths that were selected
      * \param outputItems The vector of AssetTreeItem that were selected
      */
    void getSelectedAssets(std::vector<std::string>& outputPaths,
        std::vector<AssetTreeItem*>& outputItems);

    /**
      * Imports asset tree data for this model by recursively traversing the folder
      * structure.
      *
      * \param assetBasePath The base path where to find all assets
      * \param assetBasePath The base path where to find user assets
      */
    void importModelData(const std::string& assetBasePath,
        const std::string& userAssetBasePath);

    /**
      * Returns bool for if item is checked/selected.
      *
      * \param index Location of the item to set
      * \return `true` if the item is checked
      */
    bool isChecked(QModelIndex& index) const;

    /**
      * Answers query about whether or not item is an asset.
      *
      * \param index Location of the item to query
      * \return `true` if the item is an asset (and not a directory)
      */
    bool isAsset(QModelIndex& index) const;

    /**
      * Answers query about whether or not item is in the current filesystem.
      *
      * \param index Location of the item to query
      * \return `true` if the data is in the filesystem
      */
    bool inFilesystem(QModelIndex& index) const;

    /**
      * Returns number of child items of referenced item.
      *
      * \param index Location of the item to query
      * \return Number of child items
      */
    int childCount(QModelIndex& index) const;

    /**
      * Returns a pointer to a child item of the current item.
      *
      * \param row The child number of the current item
      * \return The pointer to the child
      */
    AssetTreeItem* child(int row) const;

    /**
      * Returns the asset name of the specified item.
      *
      * \param index Location of the item to query
      * \return The asset name of the item
      */
    QString name(QModelIndex& index) const;

    /**
      * Set asset name at specified index.
      *
      * \param index Location of the item to set
      * \param name The asset name to set
      */
    void setName(QModelIndex& index, const QString& name);

    /**
      * Set state of checked/selected of an item.
      *
      * \param index Location of the item to set
      * \param checked `true` if item is checked/selected
      */
    void setChecked(QModelIndex& index, bool checked);

    /**
      * Set state of whether or not asset exists in filesystem.
      *
      * \param index Location of the item to set
      * \param fileExists `true` if asset exists in filesystem
      */
    void setExistenceInFilesystem(QModelIndex& index, bool fileExists);

private:
    AssetTreeItem* item(const QModelIndex& index) const;

    std::unique_ptr<AssetTreeItem> _rootItem;
};

#endif // __OPENSPACE_UI_LAUNCHER___ASSETTREEMODEL___H__
