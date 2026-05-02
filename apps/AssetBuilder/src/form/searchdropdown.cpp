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

#include "form/searchdropdown.h"

#include <QApplication>
#include <QCursor>
#include <QEvent>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLineEdit>
#include <QListWidget>
#include <QPushButton>
#include <QVBoxLayout>

namespace {
    constexpr const char* ChevronDown = "\xe2\x96\xbe";  // v
    constexpr const char* ChevronUp   = "\xe2\x96\xb4";  // ^
} // namespace

SearchDropdown::SearchDropdown(QWidget* parent)
    : QFrame(parent)
{
    setObjectName("search-dropdown");
    setFrameShape(QFrame::NoFrame);

    QBoxLayout* layout = new QHBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    _searchEdit = new QLineEdit(this);
    _searchEdit->setObjectName("search-dropdown-input");
    _searchEdit->installEventFilter(this);
    layout->addWidget(_searchEdit, 1);

    _chevronButton = new QPushButton(ChevronDown, this);
    _chevronButton->setObjectName("search-dropdown-chevron");
    _chevronButton->setFixedWidth(22);
    layout->addWidget(_chevronButton);

    // Popup: top-level frameless window that never steals keyboard focus. Parented to
    // nullptr so it can float above the scroll area
    _popup = new QFrame(
        nullptr,
        Qt::Tool | Qt::FramelessWindowHint | Qt::WindowDoesNotAcceptFocus
    );
    _popup->setObjectName("search-dropdown-popup");

    QBoxLayout* popupLayout = new QVBoxLayout(_popup);
    popupLayout->setContentsMargins(0, 0, 0, 0);
    popupLayout->setSpacing(0);

    _listWidget = new QListWidget(_popup);
    _listWidget->setObjectName("search-dropdown-list");
    _listWidget->setFrameShape(QFrame::NoFrame);
    _listWidget->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    _listWidget->setFocusPolicy(Qt::NoFocus);
    _listWidget->setMouseTracking(true);
    popupLayout->addWidget(_listWidget);

    // Chevron toggles the popup
    connect(
        _chevronButton,
        &QPushButton::clicked,
        this,
        [this]() {
            if (_isOpen) {
                closePopup();
            }
            else {
                openPopup();
            }
        }
    );

    // Typing filters items
    connect(
        _searchEdit, &QLineEdit::textChanged,
        this, [this](const QString& query) { filterItems(query); }
    );

    // Click on a list item: select (guard against placeholder with index == -1)
    connect(
        _listWidget,
        &QListWidget::itemClicked,
        this,
        [this](QListWidgetItem* item) {
            const int index = item->data(Qt::UserRole).toInt();
            if (index >= 0) {
                selectItem(index);
            }
        }
    );

    // Hover over list item: emit highlighted for docs panel
    connect(
        _listWidget,
        &QListWidget::itemEntered,
        this,
        [this](QListWidgetItem* item) {
            const int index = item->data(Qt::UserRole).toInt();
            if (index >= 0 && index < static_cast<int>(_items.size())) {
                emit highlighted(_items[index].userData);
            }
        }
    );

    // Close popup when focus moves somewhere unrelated
    connect(qApp, &QApplication::focusChanged, this,
        [this](QWidget*, QWidget* now) {
            if (!_isOpen || !now) {
                return;
            }
            // Walk up the parent chain of the newly focused widget. If this
            // SearchDropdown is an ancestor, the user clicked within our own UI (e.g.
            // search input, chevron) so keep the popup open. Otherwise focus left
            // entirely, so close it. This check is needed because the popup is a
            // top-level window with no Qt parent
            QWidget* widget = now;
            while (widget) {
                if (widget == this) {
                    return;
                }
                widget = widget->parentWidget();
            }
            closePopup();
        }
    );
}

SearchDropdown::~SearchDropdown() {
    // _popup is a top-level window (no Qt parent), delete manually
    delete _popup;
}

void SearchDropdown::addItem(QString displayText, QVariant userData) {
    _items.push_back({ std::move(displayText), std::move(userData) });
}

int SearchDropdown::count() const {
    return static_cast<int>(_items.size());
}

int SearchDropdown::currentIndex() const {
    return _currentIndex;
}

QString SearchDropdown::currentText() const {
    if (_currentIndex < 0 || _currentIndex >= static_cast<int>(_items.size())) {
        return QString();
    }
    return _items[_currentIndex].displayText;
}

QVariant SearchDropdown::currentData() const {
    if (_currentIndex < 0 || _currentIndex >= static_cast<int>(_items.size())) {
        return QVariant();
    }
    return _items[_currentIndex].userData;
}

QVariant SearchDropdown::itemData(int index) const {
    if (index < 0 || index >= static_cast<int>(_items.size())) {
        return QVariant();
    }
    return _items[index].userData;
}

void SearchDropdown::setCurrentIndex(int index) {
    _currentIndex = index;
    updateSearchText();
}

void SearchDropdown::setPlaceholderText(const QString& text) {
    _searchEdit->setPlaceholderText(text);
}

bool SearchDropdown::isOpen() const {
    return _isOpen;
}

void SearchDropdown::openPopup() {
    if (_isOpen) {
        return;
    }
    _isOpen = true;

    // Show full list (clear filter first so all items populate)
    _searchEdit->blockSignals(true);
    _searchEdit->clear();
    _searchEdit->blockSignals(false);
    filterItems(QString());

    // Scroll to current selection if there is one
    if (_currentIndex >= 0) {
        for (int i = 0; i < _listWidget->count(); i++) {
            if (_listWidget->item(i)->data(Qt::UserRole).toInt() == _currentIndex) {
                _listWidget->setCurrentRow(i);
                _listWidget->scrollToItem(_listWidget->item(i));
                break;
            }
        }
    }

    repositionPopup();
    _popup->show();
    _popup->raise();

    qApp->installEventFilter(this);
    _chevronButton->setText(ChevronUp);
    _searchEdit->setFocus();
}

void SearchDropdown::closePopup() {
    if (!_isOpen) {
        return;
    }
    _isOpen = false;
    qApp->removeEventFilter(this);
    _popup->hide();
    _chevronButton->setText(ChevronDown);
    updateSearchText();
}

void SearchDropdown::repositionPopup() {
    const QPoint globalPos = mapToGlobal(QPoint(0, height()));
    _popup->setFixedWidth(width());

    // Height: at least 1 row, at most 8 rows. Use sizeHintForRow after the list is
    // populated (always >= 1 item)
    const int itemHeight = _listWidget->sizeHintForRow(0);
    const int nRows = std::clamp(_listWidget->count(), 1, 8);
    // Fall back to 24px per row if sizeHintForRow returns 0 (no items rendered yet)
    const int listHeight = itemHeight > 0 ? itemHeight * nRows : 24 * nRows;
    _listWidget->setFixedHeight(listHeight);

    // Set popup height explicitly -- adjustSize() is unreliable for Qt::Tool windows.
    // Add 2px for the 1px QSS border on top and bottom
    _popup->setFixedHeight(listHeight + 2);
    _popup->move(globalPos);
}

void SearchDropdown::filterItems(const QString& query) {
    _listWidget->clear();
    for (int i = 0; i < static_cast<int>(_items.size()); i++) {
        const bool hit = _items[i].displayText.contains(query, Qt::CaseInsensitive);

        if (query.isEmpty() || hit) {
            QListWidgetItem* item = new QListWidgetItem(
                _items[i].displayText,
                _listWidget
            );
            item->setData(Qt::UserRole, i);
            if (i == _currentIndex) {
                item->setSelected(true);
            }
        }
    }

    // Show placeholder when no items match
    if (_listWidget->count() == 0) {
        QListWidgetItem* placeholder = new QListWidgetItem(
            "No results found",
            _listWidget
        );
        // -1 marks this as a non-item so click handlers ignore it
        placeholder->setData(Qt::UserRole, -1);
        // Only enabled (visible), not selectable or clickable
        placeholder->setFlags(Qt::ItemIsEnabled);
        QFont italicFont = placeholder->font();
        italicFont.setItalic(true);
        placeholder->setFont(italicFont);
        placeholder->setForeground(palette().color(QPalette::Disabled, QPalette::Text));
    }

    if (_isOpen) {
        repositionPopup();
    }
}

void SearchDropdown::selectItem(int originalIndex) {
    _currentIndex = originalIndex;
    emit activated(originalIndex);
    closePopup();
}

void SearchDropdown::navigateList(int delta) {
    const int nItems = _listWidget->count();
    if (nItems == 0) {
        return;
    }
    // Clamp to valid range so arrow keys stop at the first and last item
    int row = qBound(0, _listWidget->currentRow() + delta, nItems - 1);
    _listWidget->setCurrentRow(row);
    // Retrieve the original item index stored in UserRole; skip placeholder items (-1)
    const int index = _listWidget->item(row)->data(Qt::UserRole).toInt();
    if (index >= 0 && index < static_cast<int>(_items.size())) {
        emit highlighted(_items[index].userData);
    }
}

void SearchDropdown::updateSearchText() {
    _searchEdit->blockSignals(true);
    if (_currentIndex >= 0 && _currentIndex < static_cast<int>(_items.size())) {
        _searchEdit->setText(_items[_currentIndex].displayText);
    }
    else {
        _searchEdit->clear();
    }
    _searchEdit->blockSignals(false);
}

bool SearchDropdown::eventFilter(QObject* object, QEvent* event) {
    // Block scrolling outside the popup while it is open (matches QComboBox behavior).
    // Check cursor position rather than widget parentage because the popup is a
    // Qt::Tool window that does not accept focus, so Qt may route wheel events to the
    // focused widget (_searchEdit) instead of the widget under the cursor
    if (_isOpen && event->type() == QEvent::Wheel) {
        if (!_popup->geometry().contains(QCursor::pos())) {
            return true;
        }
    }

    // Only intercept events for the search input; delegate everything else to the base
    // class which returns false (event not consumed)
    if (object != _searchEdit) {
        return QFrame::eventFilter(object, event);
    }

    // Click on the search input while closed: open the popup but return false so the
    // QLineEdit still receives the click and positions the text cursor
    if (event->type() == QEvent::MouseButtonPress && !_isOpen) {
        openPopup();
        return false;
    }

    if (event->type() != QEvent::KeyPress) {
        return QFrame::eventFilter(object, event);
    }

    switch (QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);  keyEvent->key()) {
        case Qt::Key_Down:
            if (!_isOpen) {
                openPopup();
            }
            navigateList(+1);
            return true;
        case Qt::Key_Up:
            navigateList(-1);
            return true;
        case Qt::Key_Return:
        case Qt::Key_Enter:
            if (_isOpen) {
                QListWidgetItem* currentItem = _listWidget->currentItem();
                if (currentItem) {
                    const int index = currentItem->data(Qt::UserRole).toInt();
                    if (index >= 0) {
                        selectItem(index);
                    }
                }
            }
            return true;
        case Qt::Key_Escape:
            if (_isOpen) {
                closePopup();
            }
            return true;
        default:
            // Any printable character: open popup if not already open
            if (!_isOpen && !keyEvent->text().isEmpty()) {
                openPopup();
            }
            break;
    }
    return QFrame::eventFilter(object, event);
}
