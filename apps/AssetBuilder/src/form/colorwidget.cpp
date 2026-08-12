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

#include <ghoul/misc/assert.h>
#include <QColorDialog>
#include <QDoubleValidator>
#include <QGridLayout>
#include <QLineEdit>
#include <QPushButton>
#include <format>

namespace {
    constexpr int SwatchSize = 24;
    constexpr int ColorDecimalPrecision = 4;

    QColor toQColor(const std::vector<double>& vals) {
        ghoul_assert(vals.size() == 3 || vals.size() == 4, "Invalid size");
        return QColor::fromRgbF(
            vals[0],
            vals[1],
            vals[2],
            vals.size() > 3 ? vals[3] : 1.0
        );
    }
} // namespace

ColorWidget::ColorWidget(int nComponents, QWidget* parent)
    : MatrixWidget(nComponents, nComponents, false, parent)
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

    const int swatchCol = _grid->columnCount();
    _grid->addWidget(_swatchButton, 0, swatchCol);

    updateSwatch();

    connect(this, &MatrixWidget::valueChanged, this, &ColorWidget::updateSwatch);

    connect(
        _swatchButton,
        &QPushButton::clicked,
        this,
        [this]() {
            const QColor initial = toQColor(values());

            const QColorDialog::ColorDialogOptions opts = (_fields.size() > 3) ?
                QColorDialog::ColorDialogOptions(QColorDialog::ShowAlphaChannel) :
                QColorDialog::ColorDialogOptions();

            const QColor picked =
                QColorDialog::getColor(initial, this, "Pick Color", opts);

            if (!picked.isValid()) {
                return;
            }

            // Write back to fields
            std::vector<double> newVals = {
                picked.redF(),
                picked.greenF(),
                picked.blueF()
            };
            if (_fields.size() > 3) {
                newVals.push_back(picked.alphaF());
            }
            setValues(newVals);
        }
    );
}

void ColorWidget::setValues(const std::vector<double>& vals) {
    MatrixWidget::setValues(vals);
    // Base emits valueChanged which is normally connected to updateSwatch, but callers
    // may blockSignals during populate. Call directly to ensure the swatch always
    // reflects the current values
    updateSwatch();
}

void ColorWidget::updateSwatch() {
    const QColor color = toQColor(values());
    std::string style = std::format(
        "background-color: rgb({}, {}, {});",
        color.red(), color.green(), color.blue()
    );
    _swatchButton->setStyleSheet(QString::fromStdString(style));
}
