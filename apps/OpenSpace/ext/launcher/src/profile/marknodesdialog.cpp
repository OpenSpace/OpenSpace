/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include "profile/marknodesdialog.h"

#include "profile/line.h"
#include <openspace/scene/profile.h>
#include <QDialogButtonBox>
#include <QEvent>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPushButton>
#include <QVBoxLayout>

MarkNodesDialog::MarkNodesDialog(QWidget* parent, std::vector<std::string>* markedNodes)
    : QDialog(parent)
    , _markedNodes(markedNodes)
    , _markedNodesData(*_markedNodes)
{
    setWindowTitle("Mark Interesting Nodes");
    createWidgets();
}

void MarkNodesDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    _list = new QListWidget;
    connect(
        _list, &QListWidget::itemSelectionChanged,
        [this]() {
            _removeButton->setEnabled(true);
        }
    );
    _list->setAlternatingRowColors(true);
    _list->setMovement(QListView::Free);
    _list->setResizeMode(QListView::Adjust);

    for (size_t i = 0; i < _markedNodesData.size(); ++i) {
        _markedNodesListItems.push_back(
            new QListWidgetItem(QString::fromStdString(_markedNodesData[i]))
        );
        _list->addItem(_markedNodesListItems[i]);
    }
    layout->addWidget(_list);

    _removeButton = new QPushButton("Remove");
    connect(_removeButton, &QPushButton::clicked, this, &MarkNodesDialog::listItemRemove);
    layout->addWidget(_removeButton);

    {
        QBoxLayout* box = new QHBoxLayout;
        box->addWidget(new QLabel("Node to add:"));
        _newNode = new QLineEdit;
        _newNode->setToolTip(
            "Name of scenegraph node to add to list of \"interesting\" nodes"
        );
        box->addWidget(_newNode);

        QPushButton* addButton = new QPushButton("Add new");
        connect(addButton, &QPushButton::clicked, this, &MarkNodesDialog::listItemAdded);
        box->addWidget(addButton);
        layout->addLayout(box);
    }
    layout->addWidget(new Line);
    {
        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(
            buttons, &QDialogButtonBox::accepted,
            this, &MarkNodesDialog::parseSelections
        );
        connect(buttons, &QDialogButtonBox::rejected, this, &MarkNodesDialog::reject);
        layout->addWidget(buttons);
    }
}

void MarkNodesDialog::listItemAdded() {
    if (_newNode->text().isEmpty()) {
        return;
    }

    std::string itemToAdd = _newNode->text().toStdString();
    const auto it = std::find(
        _markedNodesData.cbegin(), _markedNodesData.cend(),
        itemToAdd
    );
    if (it != _markedNodesData.end()) {
        _list->setCurrentRow(
            static_cast<int>(std::distance(_markedNodesData.cbegin(), it))
        );
    }
    else {
        _markedNodesData.push_back(itemToAdd);
        _markedNodesListItems.push_back(new QListWidgetItem(_newNode->text()));
        _list->addItem(_markedNodesListItems.back());

        // Scroll down to that blank line highlighted
        _list->setCurrentItem(_markedNodesListItems.back());
    }

    // Blank-out entry again
    _newNode->clear();
}

void MarkNodesDialog::listItemRemove() {
    QListWidgetItem* item = _list->currentItem();
    int index = _list->row(item);

    if (index < 0 || index >= static_cast<int>(_markedNodesListItems.size())) {
        return;
    }

    _list->takeItem(index);
    _markedNodesData.erase(_markedNodesData.begin() + index);
    _markedNodesListItems.erase(_markedNodesListItems.begin() + index);
}

void MarkNodesDialog::parseSelections() {
    *_markedNodes = std::move(_markedNodesData);
    accept();
}

void MarkNodesDialog::keyPressEvent(QKeyEvent* evt) {
   if ((evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return) &&
       (!_newNode->text().isEmpty() && _newNode->hasFocus()))
   {
        listItemAdded();
        return;
}
    QDialog::keyPressEvent(evt);
}
