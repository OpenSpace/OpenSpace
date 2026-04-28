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

#ifndef __OPENSPACE_ASSETBUILDER___FORM_MATRIXWIDGET___H__
#define __OPENSPACE_ASSETBUILDER___FORM_MATRIXWIDGET___H__

#include <QWidget>

#include <jasset.h>
#include <vector>

class QGridLayout;
class QLineEdit;

/**
 * Reusable widget showing N numeric fields in a row (vectors) or grid (matrices).
 * Supports both integer and double modes.
 *
 * Internally all values are stored as doubles since JSON uses the same number type
 * for both integers and floats. The isInteger flag only controls what the user can
 * type and how values are displayed (e.g. "5" vs "5.0").
 */
class MatrixWidget : public QWidget {
Q_OBJECT
public:
    /**
     * \param nComponents Number of numeric components (2, 3, 4, 9, 16)
     * \param nColumns Number of columns for grid layout (use nComponents for row)
     * \param isInteger `true` = integer validation, `false` = double validation
     * \param parent Parent widget
     */
    MatrixWidget(int nComponents, int nColumns, bool isInteger,
        QWidget* parent = nullptr);

    /**
     * Returns current values as a PropertyList of doubles.
     *
     * \return One PropertyValue per component, each holding the field's double value
     */
    PropertyList values() const;

    /**
     * Sets field values from a PropertyList of doubles.
     *
     * \param vals List of values to write; must have exactly nComponents entries
     */
    virtual void setValues(const PropertyList& vals);

    /**
     * Returns true if any field is non-empty.
     *
     * \return true if at least one field contains text
     */
    bool hasContent() const;

    /**
     * Clears all fields and emits valueChanged.
     */
    void clear();

signals:
    /**
     * Emitted whenever a field value changes (user edit or programmatic).
     */
    void valueChanged();

protected:
    /// The grid layout holding all numeric fields
    QGridLayout* _grid = nullptr;
    /// One QLineEdit per numeric component, laid out in a grid
    std::vector<QLineEdit*> _fields;
    /// `true` for integer validation and display, `false` for double
    bool _isInteger = false;
};

#endif // __OPENSPACE_ASSETBUILDER___FORM_MATRIXWIDGET___H__
