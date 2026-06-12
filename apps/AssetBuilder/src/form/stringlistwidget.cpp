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

#include "form/stringlistwidget.h"

#include "form/flowlayout.h"
#include <QFrame>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QVBoxLayout>

namespace {
    constexpr int ChipSpacing = 4;

    // UTF-8 multiplication sign used as the remove glyph
    constexpr const char* RemoveGlyph = "\xc3\x97";
} // namespace

StringListWidget::StringListWidget(QWidget* parent)
    : QWidget(parent)
{
    // The FlowLayout reports its true (wrapped) height only via heightForWidth(). A
    // parent layout queries that only when the widget's size policy opts in, so enable
    // height-for-width here; otherwise the parent grid allocates just the one-row
    // sizeHint and clips chips that wrap to later rows (e.g. when loading a long list).
    QSizePolicy selfPolicy = sizePolicy();
    selfPolicy.setHeightForWidth(true);
    setSizePolicy(selfPolicy);

    QVBoxLayout* outer = new QVBoxLayout(this);
    outer->setContentsMargins(0, 0, 0, 0);
    outer->setSpacing(4);

    // Chip area with flow layout — hidden when empty to avoid dead spacing
    _chipArea = new QWidget(this);
    _chipArea->setVisible(false);
    new FlowLayout(ChipSpacing, ChipSpacing, _chipArea);
    QSizePolicy chipPolicy = _chipArea->sizePolicy();
    chipPolicy.setHeightForWidth(true);
    _chipArea->setSizePolicy(chipPolicy);
    outer->addWidget(_chipArea);

    // Input row: text field + add button
    QWidget* inputRow = new QWidget(this);
    QHBoxLayout* inputLayout = new QHBoxLayout(inputRow);
    inputLayout->setContentsMargins(0, 0, 0, 0);
    inputLayout->setSpacing(4);

    _input = new QLineEdit(inputRow);
    _input->setPlaceholderText("Add...");
    inputLayout->addWidget(_input, 1);

    _addButton = new QPushButton("Add", inputRow);
    _addButton->setObjectName("secondary");
    inputLayout->addWidget(_addButton);

    outer->addWidget(inputRow);

    auto addFromInput = [this]() {
        const QString text = _input->text().trimmed();
        if (text.isEmpty()) {
            return;
        }
        // Skip duplicates
        const QStringList current = values();
        if (current.contains(text)) {
            _input->clear();
            return;
        }
        addChip(text);
        _chipArea->setVisible(true);
        _input->clear();
        emit valueChanged();
    };

    connect(_input, &QLineEdit::returnPressed, this, addFromInput);
    connect(_addButton, &QPushButton::clicked, this, addFromInput);
}

QStringList StringListWidget::values() const {
    QStringList result;
    QLayout* layout = _chipArea->layout();
    if (!layout) {
        return result;
    }
    for (int i = 0; i < layout->count(); i++) {
        QWidget* chip = layout->itemAt(i)->widget();
        if (!chip) {
            continue;
        }
        QLabel* label = chip->findChild<QLabel*>();
        if (label) {
            result << label->text();
        }
    }
    return result;
}

void StringListWidget::setValues(const QStringList& values) {
    // Remove all existing chips
    QLayout* layout = _chipArea->layout();
    if (layout) {
        while (QLayoutItem* item = layout->takeAt(0)) {
            delete item->widget();
            delete item;
        }
    }
    for (const QString& value : values) {
        const QString trimmed = value.trimmed();
        if (!trimmed.isEmpty()) {
            addChip(trimmed);
        }
    }
    _chipArea->setVisible(_chipArea->layout()->count() > 0);
    // The chip count changed, so the wrapped height did too — prompt the parent layout
    // to re-query heightForWidth and give the area room for every row
    _chipArea->updateGeometry();
    updateGeometry();
}

bool StringListWidget::hasContent() const {
    QLayout* layout = _chipArea->layout();
    return layout && layout->count() > 0;
}

void StringListWidget::clear() {
    QLayout* layout = _chipArea->layout();
    if (layout) {
        while (QLayoutItem* item = layout->takeAt(0)) {
            delete item->widget();
            delete item;
        }
    }
    _chipArea->setVisible(false);
    emit valueChanged();
}

void StringListWidget::addChip(const QString& text) {
    QFrame* chip = new QFrame(_chipArea);
    chip->setObjectName("string-list-chip");

    QHBoxLayout* chipLayout = new QHBoxLayout(chip);
    chipLayout->setContentsMargins(0, 0, 0, 0);
    chipLayout->setSpacing(2);

    QLabel* label = new QLabel(text, chip);
    chipLayout->addWidget(label);

    QPushButton* removeBtn = new QPushButton(RemoveGlyph, chip);
    removeBtn->setObjectName("chip-remove-button");
    removeBtn->setFixedSize(16, 16);
    chipLayout->addWidget(removeBtn);

    // Button to remove this chip: deletes the chip widget and emits valueChanged
    connect(removeBtn, &QPushButton::clicked, this, [this, chip]() {
        chip->setParent(nullptr);
        delete chip;
        // Forces the flow layout to recalculate positions and update geometry
        _chipArea->layout()->activate();
        // Hide the chip area if this was the last chip
        const bool shouldShow = _chipArea->layout()->count() > 0;
        _chipArea->setVisible(shouldShow);
        emit valueChanged();
    });

    _chipArea->layout()->addWidget(chip);
}
