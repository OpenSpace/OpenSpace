/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_LAUNCHER___ASSETTREEMODEL___H__
#define __OPENSPACE_LAUNCHER___ASSETTREEMODEL___H__

#include <QAbstractItemModel>
#include "assettreeitem.h"

struct importElement
{
    importElement(std::string l, int lev, bool chk)
        : line(l), level(lev), checked(chk) {}
    std::string line;
    int level = -1;
    bool checked = false;
};

class assetTreeModel : public QAbstractItemModel
{
    Q_OBJECT

public:
    explicit assetTreeModel(QString header1, QString header2, QObject* parent = nullptr);
    ~assetTreeModel();

    QVariant data(const QModelIndex &index, int role) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;
    QModelIndex index(int row, int column,
        const QModelIndex &parent = QModelIndex()) const override;
    QModelIndex parent(const QModelIndex &index) const override;
    QModelIndex parent(int row, int column,
        const QModelIndex& parent = QModelIndex()) const;
    int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    int columnCount(const QModelIndex &parent = QModelIndex()) const override;
    Qt::ItemFlags flags(const QModelIndex &index) const override;
    bool setData(const QModelIndex &index, const QVariant &value,
        int role = Qt::EditRole) override;
    std::vector<std::string> selectedAssets();
    void importModelData(const std::string contents);
    bool isItemChecked(QModelIndex& index) const;
    bool isItemAsset(QModelIndex& index) const;
    int childCount(QModelIndex& index) const;
    assetTreeItem* child(int row) const;

private:
    std::string headerTitle;
    assetTreeItem *getItem(const QModelIndex &index) const;
    assetTreeItem *rootItem;
    void parseChildrenForSelected(assetTreeItem* item, std::vector<std::string>& output,
        std::string pathPrefix);
    void importInsertItem(std::istringstream& iss, assetTreeItem* parent,
        importElement& elem, int level);
    bool importGetNextLine(importElement& elem, std::istringstream& iss);
    void trimWhitespaceFromLine(std::string& line);
    int getLevelFromLine(std::string line);
};

#endif // __OPENSPACE_LAUNCHER___ASSETTREEMODEL___H__
