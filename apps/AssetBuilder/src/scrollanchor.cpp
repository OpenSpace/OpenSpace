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

#include "scrollanchor.h"

#include <QEvent>
#include <QScrollArea>
#include <QScrollBar>
#include <QTimer>
#include <algorithm>

ScrollAnchor::ScrollAnchor(QScrollArea* scroll, QWidget* content)
    : QObject(scroll)
    , _scroll(scroll)
{
    content->installEventFilter(this);
}

bool ScrollAnchor::eventFilter(QObject*, QEvent* event) {
    if (event->type() == QEvent::LayoutRequest && !_pending) {
        _savedValue = _scroll->verticalScrollBar()->value();
        _pending = true;
        // A zero-delay timer runs after the current event loop iteration completes, at
        // which point Qt has finished recalculating geometry. This lets us restore the
        // scroll position after the layout pass rather than during it
        QTimer::singleShot(
            0,
            this,
            [this]() {
                const int maxScroll = _scroll->verticalScrollBar()->maximum();
                _scroll->verticalScrollBar()->setValue(std::min(_savedValue, maxScroll));
                _pending = false;
            }
        );
    }
    return false;
}
