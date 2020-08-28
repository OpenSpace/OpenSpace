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

assetTreeItem::assetTreeItem(const QVector<QVariant> &data, assetTreeItem* parentItem)
    : _itemData(data), _parentItem(parentItem)
{
}

assetTreeItem::~assetTreeItem()
{
    qDeleteAll(_childItems);
}

assetTreeItem* assetTreeItem::child(int row)
{
    if (row < 0 || row >= _childItems.size())
        return nullptr;
    return _childItems.at(row);
}

int assetTreeItem::childCount() const
{
    return _childItems.count();
}

int assetTreeItem::row() const
{
    if (_parentItem)
        return _parentItem->_childItems.indexOf(const_cast<assetTreeItem*>(this));

    return 0;
}

int assetTreeItem::columnCount() const
{
    return _itemData.count();
}

int assetTreeItem::childNumber() const
{
    if (_parentItem)
        return _parentItem->_childItems.indexOf(const_cast<assetTreeItem*>(this));
    return 0;
}

QVariant assetTreeItem::data(int column) const
{
    if (column < 0 || column >= _itemData.size())
        return QVariant();
    return _itemData.at(column);
}

bool assetTreeItem::setData(int column, const QVariant &value)
{
    if (column < 0 || column >= _itemData.size())
        return false;

    _itemData[column] = value;
    if (column == checkboxColumn) {
        _checked = value.toBool();
    }
    return true;
}

bool assetTreeItem::isAsset() {
    return (childCount() == 0);
}

bool assetTreeItem::isCategory() {
    return (childCount() > 0);
}

bool assetTreeItem::insertChildren(int position, int count, int columns)
{
    if (position < 0 || position > _childItems.size())
        return false;

    for (int row = 0; row < count; ++row) {
        QVector<QVariant> data(columns);
        assetTreeItem *item = new assetTreeItem(data, this);
        _childItems.insert(position, item);
    }

    return true;
}

bool assetTreeItem::removeChildren(int position, int count)
{
    if (position < 0 || position + count > _childItems.size())
        return false;

    for (int row = 0; row < count; ++row)
        delete _childItems.takeAt(position);

    return true;
}

bool assetTreeItem::insertColumns(int position, int columns)
{
    if (position < 0 || position > _itemData.size())
        return false;

    for (int column = 0; column < columns; ++column)
        _itemData.insert(position, QVariant());

    for (assetTreeItem *child : qAsConst(_childItems))
        child->insertColumns(position, columns);

    return true;
}

assetTreeItem* assetTreeItem::parent()
{
    return _parentItem;
}
