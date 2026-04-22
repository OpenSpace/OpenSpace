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

#include "form/collapsiblesection.h"

#include <QEvent>
#include <QHBoxLayout>
#include <QLabel>
#include <QMenu>
#include <QMouseEvent>
#include <QPushButton>
#include <QVBoxLayout>

namespace {
constexpr const char* ChevronDown = "\xe2\x96\xbe";  // v
constexpr const char* ChevronUp   = "\xe2\x96\xb4";  // ^
} // namespace

CollapsibleSection::CollapsibleSection(const QString& title, QWidget* parent)
    : QFrame(parent)
    , _title(title)
{
    buildUi(title);
}

bool CollapsibleSection::eventFilter(QObject* watched, QEvent* event) {
    if (watched == _headerFrame && event->type() == QEvent::MouseButtonRelease) {
        const auto* mouseEvent = static_cast<const QMouseEvent*>(event);
        if (mouseEvent->button() == Qt::LeftButton) {
            const QPoint position = mouseEvent->position().toPoint();
            if (_infoButton->isVisible() && _infoButton->geometry().contains(position)) {
                return false;
            }
            toggleExpanded();
            return true;
        }
        if (mouseEvent->button() == Qt::RightButton && !_sectionKey.isEmpty()) {
            QMenu menu(this);
            menu.addAction("Copy", this, [this]() {
                emit copyRequested(_sectionKey);
            });
            QAction* pasteAction = menu.addAction("Paste", this, [this]() {
                emit pasteRequested(_sectionKey);
            });
            pasteAction->setEnabled(_canPaste);
            menu.exec(_headerFrame->mapToGlobal(mouseEvent->position().toPoint()));
            return true;
        }
    }
    return QFrame::eventFilter(watched, event);
}

void CollapsibleSection::updateChevron() {
    _chevronLabel->setText(_isExpanded ? ChevronUp : ChevronDown);
}

void CollapsibleSection::setContentWidget(QWidget* widget) {
    QVBoxLayout* layout = static_cast<QVBoxLayout*>(_contentFrame->layout());

    // Explicitly delete previous content. The content frame itself stays alive,
    // so Qt's parent-child cleanup won't trigger. Without this, old widgets
    // would accumulate as hidden children on each call. deleteLater() is used
    // because the widget may still be processing events.
    while (layout->count() > 0) {
        QLayoutItem* item = layout->takeAt(0);
        if (item->widget()) {
            item->widget()->deleteLater();
        }
        delete item;
    }

    if (widget) {
        widget->setParent(_contentFrame);
        layout->addWidget(widget);
    }

    _contentFrame->setVisible(_isExpanded && (widget != nullptr));
}

QWidget* CollapsibleSection::contentWidget() const {
    const QVBoxLayout* layout = static_cast<const QVBoxLayout*>(_contentFrame->layout());
    if (layout->count() == 0) {
        return nullptr;
    }
    return layout->itemAt(0)->widget();
}

bool CollapsibleSection::isExpanded() const {
    return _isExpanded;
}

void CollapsibleSection::setExpanded(bool isExpanded) {
    _isExpanded = isExpanded;
    _contentFrame->setVisible(_isExpanded && (contentWidget() != nullptr));
    updateChevron();
}

void CollapsibleSection::setMandatory(bool isMandatory) {
    _mandatoryLabel->setVisible(isMandatory);
}

void CollapsibleSection::setSectionKey(const QString& key) {
    _sectionKey = key;
}

QString CollapsibleSection::sectionKey() const {
    return _sectionKey;
}

void CollapsibleSection::setPasteAvailable(bool isAvailable) {
    _canPaste = isAvailable;
}

void CollapsibleSection::setDocumentation(const QString& name, const QString& doc) {
    _documentation.name = name;
    _documentation.documentation = doc;
}

void CollapsibleSection::setDocumentation(const Documentation& info) {
    _documentation = info;
}

void CollapsibleSection::toggleExpanded() {
    setExpanded(!_isExpanded);
}

void CollapsibleSection::buildUi(const QString& title) {
    setObjectName("accordion-item");
    setFrameShape(QFrame::NoFrame);  // border comes from QSS

    QVBoxLayout* root = new QVBoxLayout(this);
    root->setContentsMargins(0, 0, 0, 0);
    root->setSpacing(0);

    // Header (full row is clickable)
    _headerFrame = new QFrame(this);
    _headerFrame->setObjectName("accordion-header");
    _headerFrame->setFrameShape(QFrame::NoFrame);
    _headerFrame->setAttribute(Qt::WA_Hover, true);
    _headerFrame->setCursor(Qt::PointingHandCursor);
    _headerFrame->installEventFilter(this);

    QHBoxLayout* headerLayout = new QHBoxLayout(_headerFrame);
    headerLayout->setContentsMargins(12, 8, 12, 8);
    headerLayout->setSpacing(8);

    // Title: transparent to mouse events so clicks reach the parent frame
    _titleLabel = new QLabel(title, _headerFrame);
    _titleLabel->setObjectName("accordion-title");
    _titleLabel->setAttribute(Qt::WA_TransparentForMouseEvents, true);
    _titleLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);

    // Info button -- always visible; default shows "No documentation available"
    _documentation.name = title;
    _infoButton = new QPushButton("i", _headerFrame);
    _infoButton->setObjectName("section-info-button");
    _infoButton->setFixedSize(18, 18);
    _infoButton->setVisible(true);
    connect(_infoButton, &QPushButton::clicked, this,
        [this]() { emit documentationRequested(_documentation); });

    // Chevron: transparent to mouse events so clicks reach the parent frame
    _chevronLabel = new QLabel(ChevronDown, _headerFrame);
    _chevronLabel->setObjectName("accordion-chevron");
    _chevronLabel->setAttribute(Qt::WA_TransparentForMouseEvents, true);

    _mandatoryLabel = new QLabel("*", _headerFrame);
    _mandatoryLabel->setObjectName("section-required-asterisk");
    _mandatoryLabel->setAttribute(Qt::WA_TransparentForMouseEvents, true);
    _mandatoryLabel->setVisible(false);

    headerLayout->addWidget(_infoButton);
    headerLayout->addWidget(_titleLabel);
    headerLayout->addWidget(_mandatoryLabel);
    headerLayout->addStretch(1);
    headerLayout->addWidget(_chevronLabel);

    // Content area
    _contentFrame = new QFrame(this);
    _contentFrame->setObjectName("accordion-content");
    _contentFrame->setFrameShape(QFrame::NoFrame);
    _contentFrame->setVisible(_isExpanded);

    QVBoxLayout* contentLayout = new QVBoxLayout(_contentFrame);
    contentLayout->setContentsMargins(16, 8, 16, 12);
    contentLayout->setSpacing(0);

    root->addWidget(_headerFrame);
    root->addWidget(_contentFrame);

    updateChevron();
}
