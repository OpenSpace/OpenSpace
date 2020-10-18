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

#include "profile/assettreeitem.h"

AssetTreeItem::AssetTreeItem(const std::vector<QVariant>& data, AssetTreeItem* parentItem)
    : _itemData(data)
    , _parentItem(parentItem)
{}

AssetTreeItem::~AssetTreeItem() {
    for (AssetTreeItem* item : _childItems) {
        delete item;
    }
}

AssetTreeItem* AssetTreeItem::child(int row) {
    if (row < 0 || row >= static_cast<int>(_childItems.size())) {
        return nullptr;
    }
    else {
        return _childItems.at(row);
    }
}

int AssetTreeItem::childCount() const {
    return static_cast<int>(_childItems.size());
}

int AssetTreeItem::row() const {
    if (_parentItem) {
        const auto it = std::find(
            _parentItem->_childItems.cbegin(),
            _parentItem->_childItems.cend(),
            this
        );
        return std::distance(_parentItem->_childItems.cbegin(), it);
    }
    else {
        return 0;
    }
}

int AssetTreeItem::columnCount() const {
    return _itemData.size();
}

QVariant AssetTreeItem::data(int column) const {
    if (column < 0 || column >= _itemData.size()) {
        return QVariant();
    }
    else {
        return _itemData.at(column);
    }
}

bool AssetTreeItem::setData(int column, const QVariant& value) {
    if (column < 0 || column >= _itemData.size()) {
        return false;
    }

    _itemData[column] = value;
    if (column == CheckboxColumn) {
        _isChecked = value.toBool();
    }
    return true;
}

bool AssetTreeItem::isChecked() const {
    return _isChecked;
}

void AssetTreeItem::setChecked(bool set) {
    _isChecked = set;
}

bool AssetTreeItem::isAsset() const {
    return (childCount() == 0);
}

bool AssetTreeItem::isCategory() const {
    return (childCount() > 0);
}

void AssetTreeItem::setExistsInFilesystem(bool fileExists) {
    _fileExists = fileExists;
}

bool AssetTreeItem::doesExistInFilesystem() const {
    return _fileExists;
}

QString AssetTreeItem::name() const {
    return QString(data(0).toString());
}

bool AssetTreeItem::insertChildren(int position, int count, int columns) {
    if (position < 0 || position > _childItems.size()) {
        return false;
    }

    for (int row = 0; row < count; ++row) {
        std::vector<QVariant> data(columns);
        AssetTreeItem*item = new AssetTreeItem(data, this);
        _childItems.insert(_childItems.begin() + position, item);
    }

    return true;
}

bool AssetTreeItem::removeChildren(int position, int count) {
    if (position < 0 || position + count > _childItems.size()) {
        return false;
    }

    for (int row = 0; row < count; ++row) {
        delete _childItems[position];
        _childItems.erase(_childItems.begin() + position);
    }

    return true;
}

bool AssetTreeItem::insertColumns(int position, int columns) {
    if (position < 0 || position > _itemData.size()) {
        return false;
    }

    for (int column = 0; column < columns; ++column) {
        _itemData.insert(_itemData.begin() + position, QVariant());
    }

    for (AssetTreeItem* child : qAsConst(_childItems)) {
        child->insertColumns(position, columns);
    }

    return true;
}

AssetTreeItem* AssetTreeItem::parent() {
    return _parentItem;
}
