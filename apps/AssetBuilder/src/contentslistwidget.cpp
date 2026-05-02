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

#include "contentslistwidget.h"

#include "jasset.h"
#include <ghoul/misc/assert.h>
#include <QHBoxLayout>
#include <QLabel>
#include <QListWidget>
#include <QMenu>
#include <QMessageBox>
#include <QPushButton>
#include <set>

namespace {
    // U+25CF BLACK CIRCLE — shown before dirty content items
    constexpr const char* DirtyDot = "\xe2\x97\x8f ";

    // Returns a display name: GUI.Name > Identifier > type name
    QString displayName(const ContentItem& item) {
        const auto guiIt = item.properties.find("GUI");
        if (guiIt != item.properties.end() && guiIt->second.isMap()) {
            const PropertyMap& guiMap = guiIt->second.toMap();
            const auto nameIt = guiMap.find("Name");
            if (nameIt != guiMap.end() && nameIt->second.isString()) {
                return QString::fromStdString(nameIt->second.toString());
            }
        }

        const auto idIt = item.properties.find("Identifier");
        if (idIt != item.properties.end() && idIt->second.isString()) {
            return QString::fromStdString(idIt->second.toString());
        }
        // Fallback; use type
        return QString::fromStdString(item.type);
    }

    // Collects all Identifier strings from the asset's contents
    std::set<std::string> collectIdentifiers(const std::vector<ContentItem>& contents) {
        std::set<std::string> ids;
        for (const ContentItem& ci : contents) {
            auto it = ci.properties.find("Identifier");
            if (it != ci.properties.end() && it->second.isString()) {
                ids.insert(it->second.toString());
            }
        }
        return ids;
    }

    // Returns baseId + suffix such that the result is not in existing
    std::string makeUniqueId(const std::string& baseId,
                             const std::set<std::string>& existing, int startSuffix)
    {
        std::string id = baseId + std::to_string(startSuffix);
        while (existing.count(id) > 0) {
            startSuffix++;
            id = baseId + std::to_string(startSuffix);
        }
        return id;
    }
} // namespace

ContentsListWidget::ContentsListWidget(QWidget* parent, JAsset& asset)
    : QWidget(parent)
    , _asset(asset)
{
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    QBoxLayout* headerLayout = new QHBoxLayout;
    headerLayout->setContentsMargins(12, 8, 8, 4);
    headerLayout->setSpacing(4);
    layout->addLayout(headerLayout);

    QLabel* header = new QLabel("CONTENTS", this);
    header->setObjectName("section-header");
    headerLayout->addWidget(header);

    headerLayout->addStretch();

    QPushButton* addButton = new QPushButton("+", this);
    addButton->setObjectName("add-button");
    addButton->setFixedSize(20, 20);
    addButton->setToolTip("Add content");
    connect(
        addButton,
        &QPushButton::clicked,
        this,
        [this, addButton]() {
            QMenu menu(this);
            menu.addAction(
                "Add Scene Graph Node",
                this,
                &ContentsListWidget::addSceneGraphNode
            );
            // Align the menu to the bottom left edge of the button
            menu.exec(addButton->mapToGlobal(addButton->rect().bottomLeft()));
        }
    );
    headerLayout->addWidget(addButton);


    _contentsList = new QListWidget(this);
    // We want a custom right-click menu on the items
    _contentsList->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(
        _contentsList, &QListWidget::currentRowChanged,
        this, &ContentsListWidget::selectionChanged
    );

    // Create the right click menu
    connect(
        _contentsList,
        &QListWidget::customContextMenuRequested,
        this,
        [this](const QPoint& pos) {
            QListWidgetItem* item = _contentsList->itemAt(pos);
            if (!item) {
                return;
            }

            const int row = _contentsList->row(item);

            QMenu menu(this);
            menu.addAction(
                "Duplicate",
                this,
                [this, row]() { duplicateSceneGraphNode(row); }
            );
            menu.addAction(
                "Remove",
                this, [this, row]() { removeSceneGraphNode(row); }
            );
            menu.exec(_contentsList->mapToGlobal(pos));
        }
    );
    layout->addWidget(_contentsList);
}

void ContentsListWidget::refresh() {
    const int selected = _contentsList->currentRow();

    // Block signals so the rebuild doesn't fire selectionChanged
    _contentsList->blockSignals(true);
    _contentsList->clear();

    // Add all items again
    for (const ContentItem& item : _asset.contents) {
        const QString label = displayName(item);
        const QString typeLabel = QString::fromStdString(item.type);
        const QString dirtyDot = item.isDirty ? DirtyDot : "";

        _contentsList->addItem(
            QString("%1%2  (%3)").arg(dirtyDot).arg(label).arg(typeLabel)
        );
    }
    // Where selection should change is handled in the respective functions. Only set the
    // refresh index if it is the same as before
    if (selected >= 0 && selected < _contentsList->count()) {
        _contentsList->setCurrentRow(selected);
    }
    _contentsList->blockSignals(false);
}

void ContentsListWidget::addSceneGraphNode() {
    const std::set<std::string> existing = collectIdentifiers(_asset.contents);
    const int startSuffix = static_cast<int>(_asset.contents.size()) + 1;
    const std::string uniqueId = makeUniqueId("Node", existing, startSuffix);

    ContentItem item = {
        .type = "SceneGraphNode",
        .properties = PropertyMap{ { "Identifier", PropertyValue{ uniqueId } } },
        .isDirty = true
    };
    _asset.contents.push_back(std::move(item));

    emit assetModified();

    _contentsList->setCurrentRow(_contentsList->count() - 1);
}

void ContentsListWidget::duplicateSceneGraphNode(int row) {
    ghoul_assert(
        row >= 0 && row < static_cast<int>(_asset.contents.size()),
        "Invalid index"
    );

    ContentItem copy = _asset.contents[row];
    copy.isDirty = true;

    // Append " Copy" to GUI.Name if present
    auto guiIt = copy.properties.find("GUI");
    if (guiIt != copy.properties.end() && guiIt->second.isMap()) {
        PropertyMap& guiMap = guiIt->second.toMap();
        auto nameIt = guiMap.find("Name");
        if (nameIt != guiMap.end() && nameIt->second.isString()) {
            nameIt->second = PropertyValue{ nameIt->second.toString() + " Copy" };
        }
    }

    // Generate a unique Identifier
    auto idIt = copy.properties.find("Identifier");
    const std::string baseId =
        idIt != copy.properties.end() && idIt->second.isString() ?
        idIt->second.toString() + "Copy" :
        "NodeCopy";
    const std::set<std::string> existing = collectIdentifiers(_asset.contents);
    std::string uniqueId = baseId;
    if (existing.count(uniqueId) > 0) {
        // Assume the first item that was copied is "1", so go with "2"
        uniqueId = makeUniqueId(baseId, existing, 2);
    }
    copy.properties["Identifier"] = PropertyValue{ uniqueId };

    // Insert after the source item
    const int insertPos = row + 1;
    _asset.contents.insert(_asset.contents.begin() + insertPos, std::move(copy));
    emit assetModified();
    _contentsList->setCurrentRow(insertPos);
}

void ContentsListWidget::removeSceneGraphNode(int row) {
    ghoul_assert(
        row >= 0 && row < static_cast<int>(_asset.contents.size()),
        "Invalid index"
    );
    const QString name = displayName(_asset.contents[row]);

    const QMessageBox::StandardButton answer = QMessageBox::question(
        this,
        "Remove Content",
        "Remove \"" + name + "\"?",
        QMessageBox::Yes | QMessageBox::No,
        QMessageBox::No
    );
    if (answer != QMessageBox::Yes) {
        return;
    }

    _asset.contents.erase(_asset.contents.begin() + row);
    emit assetModified();

    // Select a neighbor or signal empty
    const int nItems = static_cast<int>(_asset.contents.size());
    if (nItems > 0) {
        // Select the previous row if it wasn't the last row. If it was, select the new
        // last row
        const int next = (row < nItems) ? row : nItems - 1;
        _contentsList->setCurrentRow(next);
    }
    else {
        // Empty: no selection
        emit selectionChanged(-1);
    }
}
