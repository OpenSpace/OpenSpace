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

#include "profile/assettreeitem.h"

AssetTreeItem::AssetTreeItem(std::vector<QVariant> data, AssetTreeItem* parentItem)
    : _itemData(std::move(data))
    , _parentItem(parentItem)
{}

AssetTreeItem::~AssetTreeItem() {
    for (AssetTreeItem* item : _childItems) {
        delete item;
    }
}

AssetTreeItem* AssetTreeItem::child(int row) const {
    if (row >= 0 && row < static_cast<int>(_childItems.size())) {
        return _childItems.at(row);
    }
    else {
        return nullptr;
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
        return static_cast<int>(std::distance(_parentItem->_childItems.cbegin(), it));
    }
    else {
        return 0;
    }
}

int AssetTreeItem::columnCount() const {
    return static_cast<int>(_itemData.size());
}

QVariant AssetTreeItem::data(int column) const {
    if (column >= 0 && column < static_cast<int>(_itemData.size())) {
        return _itemData.at(column);
    }
    else {
        return QVariant();
    }
}

bool AssetTreeItem::setData(int column, const QVariant& value) {
    if (column < 0 || column >= static_cast<int>(_itemData.size())) {
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
    return childCount() == 0;
}

bool AssetTreeItem::isCategory() const {
    return childCount() > 0;
}

void AssetTreeItem::setExistsInFilesystem(bool fileExists) {
    _fileExists = fileExists;
}

bool AssetTreeItem::doesExistInFilesystem() const {
    return _fileExists;
}

QString AssetTreeItem::name() const {
    return data(0).toString();
}

bool AssetTreeItem::insertChildren(int position, int count, int columns) {
    if (position < 0 || position > static_cast<int>(_childItems.size())) {
        return false;
    }

    for (int row = 0; row < count; ++row) {
        std::vector<QVariant> data = std::vector<QVariant>(columns);
        AssetTreeItem* item = new AssetTreeItem(std::move(data), this);
        _childItems.insert(_childItems.begin() + position, item);
    }

    return true;
}

bool AssetTreeItem::removeChildren(int position, int count) {
    if (position < 0 || position + count > static_cast<int>(_childItems.size())) {
        return false;
    }

    for (int row = 0; row < count; ++row) {
        delete _childItems[position];
        _childItems.erase(_childItems.begin() + position);
    }

    return true;
}

bool AssetTreeItem::insertColumns(int position, int columns) {
    if (position < 0 || position > static_cast<int>(_itemData.size())) {
        return false;
    }

    for (int column = 0; column < columns; ++column) {
        _itemData.insert(_itemData.begin() + position, QVariant());
    }

    for (AssetTreeItem* child : _childItems) {
        child->insertColumns(position, columns);
    }

    return true;
}

AssetTreeItem* AssetTreeItem::parent() {
    return _parentItem;
}
