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

// Based on the Qt Flow Layout example (BSD license):
// https://doc.qt.io/qt-6/qtwidgets-layouts-flowlayout-example.html

#include "form/flowlayout.h"

#include <QWidget>
#include <algorithm>

FlowLayout::FlowLayout(int hSpacing, int vSpacing, QWidget* parent)
    : QLayout(parent)
    , _hSpacing(hSpacing)
    , _vSpacing(vSpacing)
{}

FlowLayout::~FlowLayout() {
    // QLayoutItem does not inherit QObject, so items in _items have no Qt parent-child
    // ownership and must be freed manually
    while (QLayoutItem* item = takeAt(0)) {
        delete item;
    }
}

void FlowLayout::addItem(QLayoutItem* item) {
    _items.append(item);
}

int FlowLayout::count() const {
    return _items.size();
}

QLayoutItem* FlowLayout::itemAt(int index) const {
    if (index >= 0 && index < _items.size()) {
        return _items[index];
    }
    return nullptr;
}

QLayoutItem* FlowLayout::takeAt(int index) {
    if (index >= 0 && index < _items.size()) {
        return _items.takeAt(index);
    }
    return nullptr;
}

Qt::Orientations FlowLayout::expandingDirections() const {
    return Qt::Orientations();
}

bool FlowLayout::hasHeightForWidth() const {
    return true;
}

int FlowLayout::heightForWidth(int width) const {
    return doLayout(QRect(0, 0, width, 0), true);
}

QSize FlowLayout::minimumSize() const {
    QSize size;
    for (const QLayoutItem* item : _items) {
        size = size.expandedTo(item->minimumSize());
    }
    // Add the layout's padding (space between the layout edge and its content) to the
    // minimum size so the parent reserves enough room
    const QMargins m = contentsMargins();
    size += QSize(m.left() + m.right(), m.top() + m.bottom());
    return size;
}

QSize FlowLayout::sizeHint() const {
    return minimumSize();
}

void FlowLayout::setGeometry(const QRect& rect) {
    QLayout::setGeometry(rect);
    doLayout(rect, false);
}

int FlowLayout::doLayout(const QRect& rect, bool testOnly) const {
    // Inset the available rectangle by the layout's padding so items are not placed
    // directly against the layout edge. adjusted() adds offsets to each edge, so right
    // and bottom are negated to move those edges inward rather than outward
    const QMargins m = contentsMargins();
    const QRect effective = rect.adjusted(m.left(), m.top(), -m.right(), -m.bottom());
    int x = effective.x();
    int y = effective.y();
    int rowHeight = 0;

    for (QLayoutItem* item : _items) {
        const QSize itemSize = item->sizeHint();
        // effective.right() is inclusive (left + width - 1), so + 1 gives the right edge
        // that the item must not cross
        const bool exceedsWidth = x + itemSize.width() > effective.right() + 1;
        // Never wrap when the current row is still empty, otherwise an item wider than
        // the row would push to a new row that is equally too narrow
        const bool rowHasItems = rowHeight > 0;
        // Wrap to next row if this item exceeds the available width
        if (exceedsWidth && rowHasItems) {
            x = effective.x();
            y += rowHeight + _vSpacing;
            rowHeight = 0;
        }
        if (!testOnly) {
            item->setGeometry(QRect(QPoint(x, y), itemSize));
        }
        x += itemSize.width() + _hSpacing;
        rowHeight = std::max(rowHeight, itemSize.height());
    }
    // Total height: content rows plus bottom padding
    return y + rowHeight - rect.y() + m.bottom();
}
