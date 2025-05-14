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

#ifndef __OPENSPACE_UI_LAUNCHER___ASSETTREEITEM___H__
#define __OPENSPACE_UI_LAUNCHER___ASSETTREEITEM___H__

#include <QVariant>
#include <vector>

class AssetTreeItem {
public:
    static constexpr int CheckboxColumn = 1;

    /**
     * Constructor for assetTreeItem class.
     *
     * \param data QVector containing the data contained in this tree view item
     * \param parentItem The parent that contains this (and possibly other) children in
     *        the tree structure (optional)
     */
    AssetTreeItem(std::vector<QVariant> data, AssetTreeItem* parentItem = nullptr);

    /**
     * Destructor for assetTreeItem class.
     */
    ~AssetTreeItem();

    /**
      * Returns pointer to this tree item's child at position \p row.
      *
      * \param row The row number of the child to get pointer
      */
    AssetTreeItem* child(int row) const;

    /**
      * Returns the number of children this item has.
      *
      * \return The number of children
      */
    int childCount() const;

    /**
      * Returns the number of data columns of this item.
      *
      * \return The number of data columns
      */
    int columnCount() const;

    /**
      * Returns the data at column \p column of this item.
      *
      * \param column Column number from which to retrieve data
      * \return The data contained in the column
      */
    QVariant data(int column) const;

    /**
      * Inserts children item(s) to the current item.
      *
      * \param position Where in this item's children to insert new children
      * \param count Number of children to insert
      * \param columns Number of data columns in each child
      *
      * \return `true` if new children were successfully inserted
      */
    bool insertChildren(int position, int count, int columns);

    /**
      * Inserts data column(s) in the current item.
      *
      * \param position Column number at which to insert column(s)
      * \param columns Number of columns to insert
      * \return `true` if columns were successfully inserted
      */
    bool insertColumns(int position, int columns);

    /**
      * Returns a pointer to the current item's parent.
      *
      * \return Pointer to the \p assetTreeItem parent
      */
    AssetTreeItem* parent();

    /**
      * Returns the row number / child number of this item's parent.
      *
      * \return The row number of this item's parent
      */
    int row() const;

    /**
      * Returns the data at column \p column of this item.
      *
      * \param position The position of the child(ren) to remove from the current item
      *        (which is parent)
      * \param count The number of children to remove
      * \return `true` if child item(s) successfully removed
      */
    bool removeChildren(int position, int count);

    /**
      * Set data at column \p column.
      *
      * \param column The data column number to set
      * \param value The #QVariant data element to store at column \p column
      * \return `true` if the data set was successful
      */
    bool setData(int column, const QVariant& value);

    /**
      * Returns the checked state of this item. If an asset is selected to be included
      * in a profile, then it is checked.
      *
      * \return `true` if this item is checked
      */
    bool isChecked() const;

    /**
      * Sets the checked state of this item (whether or not it is selected for a profile).
      *
      * \param set Whether or not this item is checked
      */
    void setChecked(bool set);

    /**
      * Returns a bool for whether or not this is an asset. If it is a file (has no
      * children in the filesystem tree), then it is an asset.
      *
      * \return `true` if this is an asset file and not a directory containing asset(s)
      */
    bool isAsset() const;

    /**
      * Returns a bool for whether or not this is a category. If it is a directory and not
      * an asset (file), then it is a category.
      *
      * \return `true` if this is a category
      */
    bool isCategory() const;

    /**
      * Sets status of whether or not the asset exists in the current filesystem. It is
      * possible that an imported profile lists an asset from another system that is not
      * included on the current filesystem.
      *
      * \param fileExists `true` if the asset file exists in the current filesystem
      */
    void setExistsInFilesystem(bool fileExists);

    /**
      * Returns bool for whether or not the asset exists in the current filesystem.
      *
      * \return `true` if the asset exists in the current filesystem
      */
    bool doesExistInFilesystem() const;

    /**
      * Returns the asset name of the current item.
      *
      * \return The asset name
      */
    QString name() const;

private:
    std::vector<AssetTreeItem*> _childItems;
    std::vector<QVariant> _itemData;
    AssetTreeItem* _parentItem;
    bool _isChecked = false;
    bool _fileExists = true;
};

#endif // __OPENSPACE_UI_LAUNCHER___ASSETTREEITEM___H__
