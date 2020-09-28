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

#include "assettreeitem.h"
#include "assettreemodel.h"
#include <sstream>
#include <QColor>

assetTreeModel::assetTreeModel(QString header1, QString header2, QString header3,
                               QObject* parent)
    : QAbstractItemModel(parent),
      headerTitle(header1.toUtf8().constData())
{
    QVector<QVariant> rootData;
    rootItem = new assetTreeItem({header1, header2, header3});
}

assetTreeModel::~assetTreeModel() {
    delete rootItem;
}

void assetTreeModel::importModelData(const std::string contents) {
    std::istringstream iss(contents.c_str());
    importElement rootElem = {"", 0, false, ""};

    if (importGetNextLine(rootElem, iss)) {
        importInsertItem(iss, rootItem, rootElem, 0);
    }
}

void assetTreeModel::importInsertItem(std::istringstream& iss, assetTreeItem* parent,
                                      importElement& elem, int level)
{
    int nChildInsert = -1;
    bool continueToNextLine = true;

    while (continueToNextLine && elem.line.length() != 0) {
        int levelChange = elem.level - level;

        if (levelChange == 0) {
            parent->insertChildren(++nChildInsert, 1, 3);
            parent->child(nChildInsert)->setData(0, QString::fromUtf8(elem.line.c_str()));
            bool shouldMakeElemChecked = (elem.checked || !elem.existsInFilesystem);
            Qt::CheckState check = (shouldMakeElemChecked) ? Qt::Checked : Qt::Unchecked;
            parent->child(nChildInsert)->setData(1, check);
            parent->child(nChildInsert)->setData(2, "");
            parent->child(nChildInsert)->setExistsInFilesystem(elem.existsInFilesystem);
            continueToNextLine = importGetNextLine(elem, iss);
        }
        else if (levelChange == 1) {
            importInsertItem(iss, parent->child(nChildInsert), elem, level + 1);
        }
        else if (levelChange < 0) {
            continueToNextLine = false;
            break;
        }
    }
}

bool assetTreeModel::importGetNextLine(importElement& elem, std::istringstream& iss) {
    bool ok = std::getline(iss, elem.line) ? true : false;
    if (!ok) {
        elem.line = "";
        elem.level = -1;
    }
    else {
        elem.checked = (elem.line.substr(0, 1).compare("0") == 0) ? false : true;
        elem.existsInFilesystem = (elem.line.substr(0, 1).compare("x") == 0) ?
            false : true;
        elem.line = elem.line.substr(1);
        elem.level = getLevelFromLine(elem.line);
        trimWhitespaceFromLine(elem.line);
    }
    return ok;
}

void assetTreeModel::trimWhitespaceFromLine(std::string& line) {
    line.erase(0, line.find_first_not_of(" \t\n"));
    line.erase(line.find_last_not_of(" \t\n") + 1);
}

int assetTreeModel::getLevelFromLine(std::string line) {
    int level = 0;
    for (unsigned int i = 0; i < line.length(); ++i) {
        if (line.substr(i, 1).compare(" ") == 0) {
            level++;
        }
        else {
            break;
        }
    }
    return level;
}

assetTreeItem* assetTreeModel::getItem(const QModelIndex &index) const {
    if (index.isValid()) {
        assetTreeItem* item = static_cast<assetTreeItem*>(index.internalPointer());
        if (item)
            return item;
    }
    return rootItem;
}

bool assetTreeModel::isChecked(QModelIndex& index) const {
    assetTreeItem* item = getItem(index);
    int checked = item->data(1).toInt();
    return (checked == Qt::Checked) ? true : false;
}

bool assetTreeModel::isAsset(QModelIndex& index) const {
    assetTreeItem* item = getItem(index);
    return item->isAsset();
}

bool assetTreeModel::inFilesystem(QModelIndex& index) const {
    assetTreeItem* item = getItem(index);
    return item->doesExistInFilesystem();
}

int assetTreeModel::childCount(QModelIndex& index) const {
    return getItem(index)->childCount();
}

QString assetTreeModel::name(QModelIndex& index) const {
    return getItem(index)->name();
}

QString assetTreeModel::varName(const QModelIndex& index) const {
    return getItem(index)->varName();
}

void assetTreeModel::setVarName(QModelIndex& index, QString varName) {
    getItem(index)->setData(2, varName);
}

void assetTreeModel::setName(QModelIndex& index, QString name) {
    getItem(index)->setData(0, name);
}

void assetTreeModel::setChecked(QModelIndex& index, bool checked) {
    getItem(index)->setData(1, checked ? Qt::Checked : Qt::Unchecked);
}

void assetTreeModel::setExistenceInFilesystem(QModelIndex& index, bool fileExists) {
    getItem(index)->setExistsInFilesystem(fileExists);
}

assetTreeItem* assetTreeModel::child(int row) const {
    QModelIndex i = index(row, 0);
    int nKids = childCount(i);
    if (row < nKids) {
        return getItem(i)->child(row);
    }
    return nullptr;
}

QModelIndex assetTreeModel::index(int row, int column, const QModelIndex& parent) const {
    if (parent.isValid() && parent.column() != 0)
        return QModelIndex();
    if (!hasIndex(row, column, parent))
        return QModelIndex();
    assetTreeItem *parentItem = getItem(parent);
    if (!parentItem)
        return QModelIndex();

    if (!parent.isValid())
        parentItem = rootItem;

    assetTreeItem* childItem = parentItem->child(row);
    if (childItem)
        return createIndex(row, column, childItem);
    return QModelIndex();
}

QModelIndex assetTreeModel::parent(int row, int column, const QModelIndex& parent) const
{
    QModelIndex idx = index(row, column, parent);
    return assetTreeModel::parent(idx);
}

QModelIndex assetTreeModel::parent(const QModelIndex& index) const {
    if (!index.isValid())
        return QModelIndex();

    assetTreeItem* childItem = getItem(index);

    assetTreeItem* parentItem = childItem ? childItem->parent() : nullptr;

    if (parentItem == rootItem || !parentItem) {
        return QModelIndex();
    }

    return createIndex(parentItem->childNumber(), 0, parentItem);
}

assetTreeItem* assetTreeModel::assetItem(const QModelIndex &index) {
    return getItem(index);
}

int assetTreeModel::rowCount(const QModelIndex& parent) const {
    const assetTreeItem* parentItem = getItem(parent);
    return parentItem ? parentItem->childCount() : 0;
}

int assetTreeModel::columnCount(const QModelIndex& parent) const {
    Q_UNUSED(parent);
    return rootItem->columnCount();
}

QVariant assetTreeModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return QVariant();

    assetTreeItem* item = static_cast<assetTreeItem*>(index.internalPointer());

    if (index.column() == assetTreeItem::checkboxColumn) {
        if (item->isAsset() && (role == Qt::CheckStateRole)) {
            return static_cast<int>(item->isChecked() ? Qt::Checked : Qt::Unchecked);
        }
        else {
            return QVariant();
        }
    }

    if (role == Qt::ForegroundRole) {
        if (item->doesExistInFilesystem()) {
            return QVariant(QColor(Qt::black));
        }
        else {
            return QVariant(QColor(Qt::red));
        }
    }
    else if (role == Qt::DisplayRole) {
        return item->data(index.column());
    }
    else {
        return QVariant();
    }
}

bool assetTreeModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    bool setSuccess = false;

    if (!index.isValid())
        return false;

    assetTreeItem* item = static_cast<assetTreeItem*>(index.internalPointer());

    if (role == Qt::CheckStateRole && index.column() == assetTreeItem::checkboxColumn) {
        setSuccess = item->setData(index.column(), value);
    }
    return setSuccess;
}

Qt::ItemFlags assetTreeModel::flags(const QModelIndex& index) const {
    if (!index.isValid())
        return Qt::NoItemFlags;

    Qt::ItemFlags flags = Qt::ItemIsEnabled | Qt::ItemIsSelectable;

    if ( index.column() == assetTreeItem::checkboxColumn )
        flags |= Qt::ItemIsUserCheckable;

    return flags;
}

QVariant assetTreeModel::headerData(int section, Qt::Orientation orientation,
                               int role) const
{
    if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
        return rootItem->data(section);

    return QVariant();
}

std::vector<openspace::Profile::Asset> assetTreeModel::selectedAssets() {
    std::vector<openspace::Profile::Asset> output;
    parseChildrenForSelected(rootItem, output, "");
    return output;
}

void assetTreeModel::parseChildrenForSelected(assetTreeItem* item,
                                           std::vector<openspace::Profile::Asset>& output,
                                              std::string pathPrefix)
{
    std::string itemName = item->data(0).toString().toUtf8().constData();
    std::string varName = item->data(2).toString().toUtf8().constData();
    bool isPathPrefix = ((pathPrefix.length()) == 0 && (itemName == headerTitle));

    if (item->isAsset()) {
        if (item->isChecked()) {
            std::string path = pathPrefix + itemName;
            output.push_back(openspace::Profile::Asset({path, varName}));
        }
    }
    else {
        if (!isPathPrefix) {
            pathPrefix += itemName;
            pathPrefix += "/";
        }
        for (int i = 0; i < item->childCount(); ++i) {
            parseChildrenForSelected(item->child(i), output, pathPrefix);
        }
    }
}
