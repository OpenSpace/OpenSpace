/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include "profile/assettreeitem.h"
#include "profile/assettreemodel.h"
#include "filesystemaccess.h"
#include <ghoul/misc/stringhelper.h>
#include <sstream>
#include <QColor>

namespace {
    constexpr std::string_view Header1 = "Asset";
    constexpr std::string_view Header2 = "Enabled";

    struct ImportElement {
        std::string line;
        int level = -1;
        bool checked = false;
        bool existsInFilesystem = true;
    };

    int getLevelFromLine(const std::string& line) {
        int level = 0;
        for (unsigned int i = 0; i < line.length(); i++) {
            if (line.substr(i, 1) == " ") {
                level++;
            }
            else {
                break;
            }
        }
        return level;
    }

    void trimWhitespaceFromLine(std::string& line) {
        line.erase(0, line.find_first_not_of(" \t\n"));
        line.erase(line.find_last_not_of(" \t\n") + 1);
    }

    bool importGetNextLine(ImportElement& elem, std::istringstream& iss) {
        ghoul::getline(iss, elem.line);
        const bool ok = iss.good();
        if (!ok) {
            elem.line = "";
            elem.level = -1;
        }
        else {
            elem.checked = elem.line.substr(0, 1) != "0";
            elem.existsInFilesystem = elem.line.substr(0, 1) != "x";
            elem.line = elem.line.substr(1);
            elem.level = getLevelFromLine(elem.line);
            trimWhitespaceFromLine(elem.line);
        }
        return ok;
    }

    void importInsertItem(std::istringstream& iss, AssetTreeItem* parent,
                          ImportElement& elem, int level)
    {
        int nChildInsert = -1;
        bool continueToNextLine = true;

        while (continueToNextLine && !elem.line.empty()) {
            const int levelChange = elem.level - level;

            if (levelChange == 0) {
                parent->insertChildren(++nChildInsert, 1, 3);
                parent->child(nChildInsert)->setData(
                    0,
                    QString::fromStdString(elem.line)
                );
                const bool shouldMakeElemChecked =
                    (elem.checked || !elem.existsInFilesystem);
                const Qt::CheckState check =
                    shouldMakeElemChecked ? Qt::Checked : Qt::Unchecked;
                parent->child(nChildInsert)->setData(1, check);
                parent->child(nChildInsert)->setExistsInFilesystem(elem.existsInFilesystem);
                continueToNextLine = importGetNextLine(elem, iss);
            }
            else if (levelChange == 1) {
                importInsertItem(iss, parent->child(nChildInsert), elem, level + 1);
            }
            else if (levelChange < 0) {
                break;
            }
        }
    }


    void parseChildrenForSelected(AssetTreeItem* item,
                                  std::vector<std::string>& outputPaths,
                                  std::vector<AssetTreeItem*>& outputItems,
                                  std::string pathPrefix)
    {
        const std::string itemName = item->data(0).toString().toStdString();
        const bool isPathPrefix = ((pathPrefix.length()) == 0 && (itemName == Header1));

        if (item->isAsset()) {
            if (item->isChecked()) {
                std::string path = pathPrefix + itemName;
                outputItems.push_back(item);
                outputPaths.push_back(std::move(path));
            }
        }
        else {
            if (!isPathPrefix) {
                pathPrefix += itemName;
                pathPrefix += "/";
            }
            for (int i = 0; i < item->childCount(); i++) {
                parseChildrenForSelected(
                    item->child(i),
                    outputPaths,
                    outputItems,
                    pathPrefix
                );
            }
        }
    }
} // namespace

AssetTreeModel::AssetTreeModel(QObject* parent)
    : QAbstractItemModel(parent)
{
    _rootItem = std::make_unique<AssetTreeItem>(
        std::vector<QVariant> {
            QString::fromStdString(std::string(Header1)),
            QString::fromStdString(std::string(Header2))
        }
    );
}

void AssetTreeModel::importModelData(const std::string& assetBasePath,
    const std::string& userAssetBasePath) {
    FileSystemAccess assets(
        ".asset",
        true,
        true
    );
    std::string assetList = assets.useQtFileSystemModelToTraverseDir(assetBasePath);
    assetList += assets.useQtFileSystemModelToTraverseDir(userAssetBasePath, true);
    std::istringstream iss(assetList);
    ImportElement rootElem = {
        .line = "",
        .level = 0,
    };

    if (importGetNextLine(rootElem, iss)) {
        importInsertItem(iss, _rootItem.get(), rootElem, 0);
    }
}

AssetTreeItem* AssetTreeModel::item(const QModelIndex& index) const {
    if (index.isValid()) {
        AssetTreeItem* item = static_cast<AssetTreeItem*>(index.internalPointer());
        if (item) {
            return item;
        }
    }
    return _rootItem.get();
}

bool AssetTreeModel::isChecked(QModelIndex& index) const {
    AssetTreeItem* i = item(index);
    const int isChecked = i->data(1).toInt();
    return isChecked == Qt::Checked;
}

bool AssetTreeModel::isAsset(QModelIndex& index) const {
    AssetTreeItem* i = item(index);
    return i->isAsset();
}

bool AssetTreeModel::inFilesystem(QModelIndex& index) const {
    AssetTreeItem* i = item(index);
    return i->doesExistInFilesystem();
}

int AssetTreeModel::childCount(QModelIndex& index) const {
    return item(index)->childCount();
}

QString AssetTreeModel::name(QModelIndex& index) const {
    return item(index)->name();
}

void AssetTreeModel::setName(QModelIndex& index, const QString& name) {
    item(index)->setData(0, name);
}

void AssetTreeModel::setChecked(QModelIndex& index, bool checked) {
    item(index)->setData(1, checked ? Qt::Checked : Qt::Unchecked);
}

void AssetTreeModel::setExistenceInFilesystem(QModelIndex& index, bool fileExists) {
    item(index)->setExistsInFilesystem(fileExists);
}

AssetTreeItem* AssetTreeModel::child(int row) const {
    QModelIndex i = index(row, 0);
    const int nChildren = childCount(i);
    if (row < nChildren) {
        return item(i)->child(row);
    }
    return nullptr;
}

QModelIndex AssetTreeModel::index(int row, int column, const QModelIndex& parent) const {
    if (parent.isValid() && parent.column() != 0) {
        return QModelIndex();
    }
    if (!hasIndex(row, column, parent)) {
        return QModelIndex();
    }
    AssetTreeItem* parentItem = item(parent);
    if (!parentItem) {
        return QModelIndex();
    }

    if (!parent.isValid()) {
        parentItem = _rootItem.get();
    }

    AssetTreeItem* childItem = parentItem->child(row);
    if (childItem) {
        return createIndex(row, column, childItem);
    }
    return QModelIndex();
}

QModelIndex AssetTreeModel::parent(int row, int column, const QModelIndex& parent) const {
    const QModelIndex idx = index(row, column, parent);
    return AssetTreeModel::parent(idx);
}

QModelIndex AssetTreeModel::parent(const QModelIndex& index) const {
    if (!index.isValid()) {
        return QModelIndex();
    }

    AssetTreeItem* childItem = item(index);
    AssetTreeItem* parentItem = childItem ? childItem->parent() : nullptr;
    if (parentItem == _rootItem.get() || !parentItem) {
        return QModelIndex();
    }

    return createIndex(parentItem->row(), 0, parentItem);
}

AssetTreeItem* AssetTreeModel::assetItem(const QModelIndex& index) {
    return item(index);
}

int AssetTreeModel::rowCount(const QModelIndex& parent) const {
    const AssetTreeItem* parentItem = item(parent);
    return parentItem ? parentItem->childCount() : 0;
}

int AssetTreeModel::columnCount(const QModelIndex&) const {
    return _rootItem->columnCount();
}

QVariant AssetTreeModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid()) {
        return QVariant();
    }

    AssetTreeItem* item = static_cast<AssetTreeItem*>(index.internalPointer());

    if (index.column() == AssetTreeItem::CheckboxColumn) {
        if (item->isAsset() && (role == Qt::CheckStateRole)) {
            return static_cast<int>(item->isChecked() ? Qt::Checked : Qt::Unchecked);
        }
        else {
            return QVariant();
        }
    }

    if (role == Qt::ForegroundRole) {
        return item->doesExistInFilesystem() ? QColor(Qt::black) : QColor(Qt::red);
    }
    else if (role == Qt::DisplayRole) {
        return item->data(index.column());
    }
    else {
        return QVariant();
    }
}

bool AssetTreeModel::setData(const QModelIndex& index, const QVariant& value, int role) {
    bool setSuccess = false;

    if (!index.isValid()) {
        return false;
    }

    AssetTreeItem* item = static_cast<AssetTreeItem*>(index.internalPointer());

    if (role == Qt::CheckStateRole && index.column() == AssetTreeItem::CheckboxColumn) {
        setSuccess = item->setData(index.column(), value);
    }
    return setSuccess;
}

Qt::ItemFlags AssetTreeModel::flags(const QModelIndex& index) const {
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsEnabled;

    if (index.column() == AssetTreeItem::CheckboxColumn) {
        flags |= Qt::ItemIsUserCheckable;
    }

    return flags;
}

QVariant AssetTreeModel::headerData(int section, Qt::Orientation orientation,
                                    int role) const
{
    if (orientation == Qt::Horizontal && role == Qt::DisplayRole) {
        return _rootItem->data(section);
    }
    else {
        return QVariant();
    }
}

void AssetTreeModel::getSelectedAssets(std::vector<std::string>& outputPaths,
                                       std::vector<AssetTreeItem*>& outputItems)
{
    parseChildrenForSelected(_rootItem.get(), outputPaths, outputItems, "");
}
