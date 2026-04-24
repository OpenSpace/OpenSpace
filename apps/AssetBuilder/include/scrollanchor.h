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

#ifndef __OPENSPACE_ASSETBUILDER___SCROLLANCHOR___H__
#define __OPENSPACE_ASSETBUILDER___SCROLLANCHOR___H__

#include <QObject>

class QScrollArea;

/**
 * Prevents the scroll area from jumping when child layout changes occur (e.g.
 * expanding/collapsing sections, toggling optional fields). Saves the scroll position
 * before each layout recalculation and restores it once the geometry pass completes,
 * keeping the viewport stable.
 */
class ScrollAnchor : public QObject {
Q_OBJECT
public:
    /**
     * Installs the scroll anchor on the given scroll area.
     *
     * \param scroll The scroll area to stabilize
     * \param content The content widget inside the scroll area
     */
    ScrollAnchor(QScrollArea* scroll, QWidget* content);

protected:
    /**
     * Intercepts LayoutRequest events to save and restore the scroll position.
     */
    bool eventFilter(QObject* watched, QEvent* event) override;

private:
    /// The scroll area whose vertical position is stabilized
    QScrollArea* _scroll;
    /// Scroll position captured before the layout pass
    int _savedValue = 0;
    /// `true` while a deferred restore is queued, prevents re-saving mid-pass
    bool _pending = false;
};

#endif // __OPENSPACE_ASSETBUILDER___SCROLLANCHOR___H__
