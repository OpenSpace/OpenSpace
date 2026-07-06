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

#include "editorpanel.h"

#include "documentation.h"
#include "scenegraphnodeeditor.h"
#include <jasset.h>
#include <QEvent>
#include <QFrame>
#include <QHBoxLayout>
#include <QLabel>
#include <QScrollArea>
#include <QScrollBar>
#include <QStackedWidget>
#include <QTimer>
#include <QVBoxLayout>

namespace {
    // Index into _centerStack
    constexpr int CenterPageEmpty = 0;
    constexpr int CenterPageEditor = 1;
} // namespace

EditorPanel::EditorPanel(QWidget* parent, JAsset& asset,
                         const IdentifierRegistry* registry)
    : QWidget(parent)
    , _asset(asset)
    , _registry(registry)
{
    QBoxLayout* root = new QVBoxLayout(this);
    root->setContentsMargins(0, 0, 0, 0);
    root->setSpacing(0);

    _centerStack = new QStackedWidget(this);
    root->addWidget(_centerStack);

    QWidget* emptyPage = new QWidget(_centerStack);
    QBoxLayout* emptyLayout = new QVBoxLayout(emptyPage);
    emptyLayout->setAlignment(Qt::AlignCenter);
    _centerStack->addWidget(emptyPage);  // index 0

    QLabel* emptyLabel = new QLabel("No node selected", emptyPage);
    emptyLabel->setObjectName("empty-state");
    emptyLabel->setAlignment(Qt::AlignCenter);
    emptyLayout->addWidget(emptyLabel);

    QLabel* emptySubLabel = new QLabel(
        "Add a Scene Graph Node or select one from the Contents panel",
        emptyPage
    );
    emptySubLabel->setObjectName("empty-state-sub");
    emptySubLabel->setAlignment(Qt::AlignCenter);
    emptySubLabel->setWordWrap(true);
    emptyLayout->addWidget(emptySubLabel);
}

void EditorPanel::showItemEditor(size_t index) {
    _currentIndex = index;

    // Remove and delete the previous editor page
    QWidget* existing = _centerStack->widget(CenterPageEditor);
    if (existing) {
        _centerStack->removeWidget(existing);
        existing->deleteLater();
    }

    // Scroll area + card scaffold
    _scroll = new QScrollArea(this);
    _scroll->setWidgetResizable(true);
    _scroll->setFrameShape(QFrame::NoFrame);

    QWidget* outerWrapper = new QWidget(_scroll);
    // Install ourselves as an event filter to enable a stable scroll anchor
    outerWrapper->installEventFilter(this);
    QBoxLayout* outerLayout = new QVBoxLayout(outerWrapper);
    outerLayout->setContentsMargins(0, 32, 0, 32);
    outerLayout->setSpacing(0);
    outerLayout->setAlignment(Qt::AlignTop);
    _scroll->setWidget(outerWrapper);

    QBoxLayout* cardRow = new QHBoxLayout;
    cardRow->setContentsMargins(0, 0, 0, 0);
    cardRow->setSpacing(0);
    cardRow->addStretch(1);

    QFrame* inner = new QFrame(outerWrapper);
    inner->setObjectName("form-card");
    inner->setMaximumWidth(1100);
    QBoxLayout* cardLayout = new QVBoxLayout(inner);
    cardLayout->setContentsMargins(32, 32, 32, 32);
    cardLayout->setSpacing(0);

    const std::string& type = _asset.contents[index].type;

    // Add more types here
    if (type == "SceneGraphNode") {
        SceneGraphNodeEditor* sgnEditor = new SceneGraphNodeEditor(
            _asset,
            _registry,
            index,
            parentWidget()
        );
        connect(
            sgnEditor, &SceneGraphNodeEditor::contentModified,
            this, &EditorPanel::contentModified
        );
        connect(
            sgnEditor, &SceneGraphNodeEditor::documentationRequested,
            this, &EditorPanel::documentationRequested
        );
        connect(
            sgnEditor, &SceneGraphNodeEditor::addDependency,
            this, &EditorPanel::addDependency
        );
        connect(
            sgnEditor, &SceneGraphNodeEditor::rebuildRequested,
            this, [this]() { showItemEditor(_currentIndex); }
        );
        cardLayout->addWidget(sgnEditor);
    }

    cardRow->addWidget(inner, 8);
    cardRow->addStretch(1);
    outerLayout->addLayout(cardRow);

    _centerStack->insertWidget(CenterPageEditor, _scroll);
    _centerStack->setCurrentIndex(CenterPageEditor);
}

void EditorPanel::showEmptyCenter() {
    _currentIndex = NoSelection;
    _centerStack->setCurrentIndex(CenterPageEmpty);
}

bool EditorPanel::eventFilter(QObject*, QEvent* event) {
    if (event->type() != QEvent::LayoutRequest || _isDeferredRestorePending) {
        return false;
    }

    _savedValue = _scroll->verticalScrollBar()->value();
    _isDeferredRestorePending = true;
    // A zero-delay timer runs after the current event loop iteration completes, at which
    // point Qt has finished recalculating geometry. This lets us restore the scroll
    // position after the layout pass rather than during it
    QTimer::singleShot(
        0,
        this,
        [this]() {
            const int maxScroll = _scroll->verticalScrollBar()->maximum();
            _scroll->verticalScrollBar()->setValue(std::min(_savedValue, maxScroll));
            _isDeferredRestorePending = false;
        }
    );
    return false;
}
