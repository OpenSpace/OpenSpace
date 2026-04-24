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

#include "form/colorwidget.h"

#include <QColorDialog>
#include <QDoubleValidator>
#include <QGridLayout>
#include <QLineEdit>
#include <QPushButton>
#include <format>

namespace {
    constexpr int SwatchSize = 24;
    constexpr int ColorDecimalPrecision = 4;
} // namespace

ColorWidget::ColorWidget(int nComponents, QWidget* parent)
    : MatrixWidget(
          nComponents,
          nComponents, // nColumns = nComponents (single row)
          false,       // double mode
          parent
      )
{
    // Override validators to enforce [0, 1] range
    for (QLineEdit* field : _fields) {
        field->setValidator(new QDoubleValidator(0.0, 1.0, ColorDecimalPrecision, field));
        field->setPlaceholderText("0.0");
    }

    // Add swatch button at the end of the existing layout
    _swatchButton = new QPushButton(this);
    _swatchButton->setObjectName("color-swatch-button");
    _swatchButton->setFixedSize(SwatchSize, SwatchSize);
    _swatchButton->setToolTip("Click to pick a color");

    // Cast needed for grid-specific API (columnCount, addWidget with row/col).
    // MatrixWidget always creates a QGridLayout
    QGridLayout* grid = qobject_cast<QGridLayout*>(layout());
    // Should never fail, but just to be safe
    Q_ASSERT(grid);
    const int swatchCol = grid->columnCount();
    // Row 0, next available column
    grid->addWidget(_swatchButton, 0, swatchCol);

    updateSwatch();

    connect(this, &MatrixWidget::valueChanged, this, &ColorWidget::updateSwatch);

    connect(
        _swatchButton,
        &QPushButton::clicked,
        this,
        [this]() {
            const QColor initial = toQColor();

            const QColorDialog::ColorDialogOptions opts = (_fields.size() > 3)
                ? QColorDialog::ColorDialogOptions(QColorDialog::ShowAlphaChannel)
                : QColorDialog::ColorDialogOptions();

            const QColor picked = QColorDialog::getColor(initial, this, "Pick Color", opts);

            if (!picked.isValid()) {
                return;
            }

            // Write back to fields
            PropertyList newVals;
            newVals.push_back(PropertyValue{ picked.redF() });
            newVals.push_back(PropertyValue{ picked.greenF() });
            newVals.push_back(PropertyValue{ picked.blueF() });
            if (_fields.size() > 3) {
                newVals.push_back(PropertyValue{ picked.alphaF() });
            }
            setValues(newVals);
        }
    );
}

void ColorWidget::setValues(const PropertyList& vals) {
    MatrixWidget::setValues(vals);
    // Base emits valueChanged which is normally connected to updateSwatch, but callers
    // may blockSignals during populate. Call directly to ensure the swatch always
    // reflects the current values
    updateSwatch();
}

QColor ColorWidget::toQColor() const {
    const PropertyList vals = values();
    for (const PropertyValue& v : vals) {
        Q_ASSERT(v.toDouble() >= 0.0 && v.toDouble() <= 1.0);
    }
    return QColor::fromRgbF(
        vals[0].toDouble(),
        vals[1].toDouble(),
        vals[2].toDouble(),
        vals.size() > 3 ? vals[3].toDouble() : 1.0
    );
}

void ColorWidget::updateSwatch() {
    const QColor color = toQColor();
    std::string style = std::format(
        "background-color: rgb({}, {}, {});",
        color.red(), color.green(), color.blue()
    );
    _swatchButton->setStyleSheet(QString::fromStdString(style));
}
