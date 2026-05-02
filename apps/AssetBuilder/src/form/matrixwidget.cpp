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

#include "form/matrixwidget.h"

#include <ghoul/misc/assert.h>
#include <QDoubleValidator>
#include <QGridLayout>
#include <QIntValidator>
#include <QLineEdit>

namespace {
    constexpr int DecimalPrecision = 10;
} // namespace

MatrixWidget::MatrixWidget(int nComponents, int nColumns, bool isInteger, QWidget* parent)
    : QWidget(parent)
    , _isInteger(isInteger)
{
    ghoul_assert(
        nComponents == 2 || nComponents == 3 || nComponents == 4 || nComponents == 9 ||
        nComponents == 16,
        "Invalid number of components"
    );
    const bool useGrid = (nComponents > nColumns);

    _grid = new QGridLayout(this);
    _grid->setContentsMargins(0, 0, 0, 0);
    _grid->setHorizontalSpacing(4);
    _grid->setVerticalSpacing(2);

    _fields.reserve(nComponents);

    for (int i = 0; i < nComponents; i++) {
        const int row = useGrid ? (i / nColumns) : 0;
        const int col = useGrid ? (i % nColumns) : i;

        QLineEdit* field = new QLineEdit(this);
        if (isInteger) {
            field->setValidator(new QIntValidator(field));
            field->setPlaceholderText("0");
        }
        else {
            field->setValidator(new QDoubleValidator(field));
            field->setPlaceholderText("0.0");
        }
        connect(field, &QLineEdit::textEdited, this, &MatrixWidget::valueChanged);

        _grid->addWidget(field, row, col);
        _fields.push_back(field);
    }

    // Even stretch for value columns only — default stretch is 0 (content-based), so this
    // ensures equal width. Also keeps any extra columns added by subclasses (e.g.
    // ColorWidget's swatch button) at their minimum size
    const int nStretchCols = useGrid ? nColumns : nComponents;
    for (int col = 0; col < nStretchCols; col++) {
        _grid->setColumnStretch(col, 1);
    }
}

PropertyList MatrixWidget::values() const {
    PropertyList result;
    result.reserve(_fields.size());
    for (const QLineEdit* field : _fields) {
        result.push_back(PropertyValue{ field->text().toDouble() });
    }
    return result;
}

void MatrixWidget::setValues(const PropertyList& vals) {
    ghoul_assert(vals.size() == _fields.size(), "Wrong number of values");
    for (size_t i = 0; i < _fields.size(); i++) {
        // Suppress per-field textEdited so setText doesn't emit valueChanged
        // N times; we emit it once after the loop instead
        _fields[i]->blockSignals(true);
        const double val = vals[i].toDouble();

        if (_isInteger) {
            _fields[i]->setText(QString::number(static_cast<int>(val)));
        }
        else {
            // 'g' = shortest of fixed/scientific notation, trims trailing zeros
            _fields[i]->setText(QString::number(val, 'g', DecimalPrecision));
        }
        _fields[i]->blockSignals(false);
    }
    emit valueChanged();
}

bool MatrixWidget::hasContent() const {
    return std::any_of(
        _fields.begin(),
        _fields.end(),
        [](const QLineEdit* field) { return !field->text().isEmpty(); }
    );
}

void MatrixWidget::clear() {
    for (QLineEdit* field : _fields) {
        field->blockSignals(true);
        field->clear();
        field->blockSignals(false);
    }
    emit valueChanged();
}
