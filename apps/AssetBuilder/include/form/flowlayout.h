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

#ifndef __OPENSPACE_ASSETBUILDER___FLOWLAYOUT___H__
#define __OPENSPACE_ASSETBUILDER___FLOWLAYOUT___H__

#include <QLayout>

/**
 * Custom layout that arranges child widgets left-to-right, wrapping to the next row when
 * horizontal space runs out.
 */
class FlowLayout final : public QLayout {
public:
    /**
     * \param hSpacing Horizontal spacing between items
     * \param vSpacing Vertical spacing between rows
     * \param parent Parent widget
     */
    explicit FlowLayout(int hSpacing, int vSpacing, QWidget* parent);

    ~FlowLayout() override;

    /**
     * Adds an item to the layout. Ownership is transferred to the layout.
     *
     * \param item The layout item to add
     */
    void addItem(QLayoutItem* item) override;

    /**
     * Returns the number of items in the layout.
     *
     * \return Item count
     */
    int count() const override;

    /**
     * Returns the item at the given index, or `nullptr` if out of range.
     *
     * \param index Zero-based item index
     * \return The layout item, or `nullptr`
     */
    QLayoutItem* itemAt(int index) const override;

    /**
     * Removes and returns the item at the given index, or `nullptr` if out of range.
     *
     * \param index Zero-based item index
     * \return The removed layout item, or `nullptr`
     */
    QLayoutItem* takeAt(int index) override;

    /**
     * QLayout's default returns Qt::Horizontal | Qt::Vertical, which tells parent layouts
     * that this layout wants to grow to fill available space. We return empty
     * orientations instead so the flow layout only takes the height it actually
     * needs for its content, rather than stretching to fill the parent.
     *
     * \return Empty Qt::Orientations
     */
    Qt::Orientations expandingDirections() const override;

    /**
     * Returns `true` since the layout height depends on the available width.
     *
     * \return Always `true`
     */
    bool hasHeightForWidth() const override;

    /**
     * Returns the required height to lay out all items within the given width.
     *
     * \param width Available horizontal space in pixels
     * \return Total height consumed by the laid-out items
     */
    int heightForWidth(int width) const override;

    /**
     * Returns the minimum size needed to fit the largest single item.
     *
     * \return Minimum layout size
     */
    QSize minimumSize() const override;

    /**
     * Returns the preferred size (same as minimumSize).
     *
     * \return Preferred layout size
     */
    QSize sizeHint() const override;

    /**
     * Arranges all items within the given rectangle, wrapping to new rows as needed.
     *
     * \param rect The available rectangle for layout
     */
    void setGeometry(const QRect& rect) override;

private:
    /**
     * Performs the wrapping layout calculation.
     *
     * \param rect Available rectangle
     * \param testOnly When `true`, only computes height without moving widgets
     * \return Total height consumed by the laid-out items
     */
    int doLayout(const QRect& rect, bool testOnly) const;

    /// The layout items managed by this layout, in insertion order. Owned by the layout
    /// and freed in the destructor
    QList<QLayoutItem*> _items;
    /// Horizontal spacing in pixels between items on the same row
    int _hSpacing = 4;
    /// Vertical spacing in pixels between rows
    int _vSpacing = 4;
};

#endif // __OPENSPACE_ASSETBUILDER___FLOWLAYOUT___H__
