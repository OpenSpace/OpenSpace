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

#ifndef __OPENSPACE_ASSET_BUILDER___FORM_SEARCHDROPDOWN___H__
#define __OPENSPACE_ASSET_BUILDER___FORM_SEARCHDROPDOWN___H__

#include <QFrame>
#include <QVariant>
#include <vector>

class QLineEdit;
class QListWidget;
class QPushButton;

/**
 * A search-as-you-type dropdown, modelled after Semantic UI's search Dropdown.
 *
 * Layout:  [search input (stretches) | v]   <- container row
 *                    | (on open)
 *          [floating list popup (plain QFrame overlay, no Qt::Popup)]
 *
 * The popup uses Qt::WindowDoesNotAcceptFocus so keyboard focus always stays
 * in the search input.  Arrow keys and Enter navigate / confirm without a click.
 */
class SearchDropdown : public QFrame {
Q_OBJECT
public:
    /**
     * \param parent Parent widget
     */
    explicit SearchDropdown(QWidget* parent = nullptr);

    /** Deletes the popup window which has no Qt parent. */
    ~SearchDropdown() override;

    /**
     * Appends an item to the dropdown list.
     *
     * \param displayText Text shown in the list and search input
     * \param userData    Optional data attached to the item (e.g. schema type key)
     */
    void addItem(const QString& displayText, const QVariant& userData = QVariant());

    /**
     * Returns the total number of items in the dropdown.
     *
     * \return Item count
     */
    int count() const;

    /**
     * Returns the index of the currently selected item, or -1 if nothing is selected.
     *
     * \return Selected item index
     */
    int currentIndex() const;

    /**
     * Returns the display text of the currently selected item.
     *
     * \return Display text, or empty string if nothing is selected
     */
    QString currentText() const;

    /**
     * Returns the user data of the currently selected item.
     *
     * \return User data, or invalid QVariant if nothing is selected
     */
    QVariant currentData() const;

    /**
     * Returns the user data for the item at the given index.
     *
     * \param index Item index
     * \return User data, or invalid QVariant if index is out of range
     */
    QVariant itemData(int index) const;

    /**
     * Sets the currently selected item by index. Pass -1 to deselect.
     *
     * \param index Item index, or -1 to clear the selection
     */
    void setCurrentIndex(int index);

    /**
     * Sets the placeholder text shown when the search input is empty.
     *
     * \param text Placeholder text
     */
    void setPlaceholderText(const QString& text);

    /**
     * Returns whether the popup is currently visible.
     *
     * \return true if the popup is open
     */
    bool isOpen() const;

signals:
    /** Emitted when the user confirms a selection. */
    void activated(int index);
    /** Emitted as the user hovers each item (for docs panel). */
    void highlighted(const QVariant& userData);

protected:
    /**
     * Intercepts key and mouse events on the search input to handle arrow
     * navigation, Enter/Escape, and click-to-open.
     *
     * \param watched The object that received the event (expected to be _searchEdit)
     * \param event   The event to filter
     * \return true if the event was handled, false to pass it through
     */
    bool eventFilter(QObject* watched, QEvent* event) override;

private:
    /** Creates the search input, chevron button, popup frame, and list widget. */
    void buildUi();

    /** Shows the popup and populates it with the full item list. */
    void openPopup();

    /** Hides the popup and restores the display text. */
    void closePopup();

    /** Moves and resizes the popup to align with the search input. */
    void repositionPopup();

    /**
     * Filters the list widget to show only items matching the query.
     *
     * \param query Search text; empty string shows all items
     */
    void filterItems(const QString& query);

    /**
     * Confirms an item selection, emits activated, and closes the popup.
     *
     * \param originalIndex Index into the _items vector
     */
    void selectItem(int originalIndex);

    /**
     * Moves the list widget highlight by delta rows.
     *
     * \param delta Number of rows to move (+1 down, -1 up)
     */
    void navigateList(int delta);

    /** Updates the search input text to show the current selection. */
    void updateSearchText();

    /// A single entry in the dropdown list.
    struct Item {
        /// Text shown in the list and search input
        QString displayText;
        /// Optional payload (e.g. schema type key)
        QVariant userData;
    };

    /// Search-as-you-type text input
    QLineEdit* _searchEdit = nullptr;
    /// Toggle button for opening/closing the popup
    QPushButton* _chevronButton = nullptr;
    /// Top-level frameless popup window (no Qt parent, manually deleted)
    QFrame* _popup = nullptr;
    /// Filterable item list inside the popup
    QListWidget* _listWidget = nullptr;

    /// All items added via addItem()
    std::vector<Item> _items;
    /// Index of the confirmed selection, -1 if none
    int _currentIndex = -1;
    /// Whether the popup is currently visible
    bool _isOpen = false;
};

#endif // __OPENSPACE_ASSET_BUILDER___FORM_SEARCHDROPDOWN___H__
