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

#ifndef __OPENSPACE_ASSETBUILDER___STRINGLISTWIDGET___H__
#define __OPENSPACE_ASSETBUILDER___STRINGLISTWIDGET___H__

#include <QWidget>

#include <QStringList>

class QLineEdit;
class QPushButton;

/**
 * Tag-style chip widget for editing a list of strings. Each entry is rendered as a
 * removable chip in a flow layout, with an input field and add button at the bottom.
 */
class StringListWidget : public QWidget {
Q_OBJECT
public:
    explicit StringListWidget(QWidget* parent = nullptr);

    /**
     * Returns the current list of string values from all chips.
     *
     * \return One entry per chip, in display order
     */
    QStringList values() const;

    /**
     * Replaces all chips with the given list of values.
     *
     * \param vals The strings to display as chips
     */
    void setValues(const QStringList& vals);

    /**
     * Returns `true` if at least one chip exists.
     *
     * \return `true` if the list is non-empty
     */
    bool hasContent() const;

    /**
     * Removes all chips and emits valueChanged.
     */
    void clear();

signals:
    /**
     * Emitted whenever a chip is added or removed.
     */
    void valueChanged();

private:
    /**
     * Creates a chip frame for the given text and adds it to the chip area.
     *
     * \param text The string value for this chip
     */
    void addChip(const QString& text);

    /// Container widget whose layout holds the chip frames
    QWidget* _chipArea = nullptr;
    /// Text field for entering new values
    QLineEdit* _input = nullptr;
    /// Button to add the current input as a new chip
    QPushButton* _addButton = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___STRINGLISTWIDGET___H__
