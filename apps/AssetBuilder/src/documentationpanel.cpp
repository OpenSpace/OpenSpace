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

#include "documentationpanel.h"

#include <QFrame>
#include <QLabel>
#include <QRegularExpression>
#include <QScrollArea>
#include <QTextBrowser>
#include <QVBoxLayout>

DocumentationPanel::DocumentationPanel(QWidget* parent)
    : QWidget(parent)
{
    buildUi();
}

void DocumentationPanel::showDocumentation(const Documentation& info) {
    _nameLabel->setText(info.name);

    // Meta line: "Type: X  Required/Optional"
    if (!info.type.isEmpty()) {
        const QString reqText = info.isOptional ? "Optional" : "Required";
        _metaLabel->setText("Type: " + info.type + "    " + reqText);
        _metaLabel->setVisible(true);
    }
    else {
        _metaLabel->setVisible(false);
    }

    // Short description
    if (!info.description.isEmpty()) {
        _descriptionLabel->setText(info.description);
        _descriptionLabel->setVisible(true);
    }
    else {
        _descriptionLabel->setVisible(false);
    }

    // Separator visible when either meta or description is shown
    _separator->setVisible(!info.type.isEmpty() || !info.description.isEmpty());

    // Full documentation — render markdown, then strip <a> tags so links appear as
    // regular text
    const QString text = info.documentation.isEmpty() ?
        "*No documentation available.*" :
        info.documentation;
    _textBrowser->setMarkdown(text);
    QString html = _textBrowser->toHtml();
    static const QRegularExpression linkOpen("<a[^>]*>");
    static const QRegularExpression linkClose("</a>");
    html.replace(linkOpen, "");
    html.replace(linkClose, "");
    _textBrowser->setHtml(html);
}

void DocumentationPanel::buildUi() {
    setObjectName("side-panel");
    setMinimumWidth(0);

    // Scrollable container
    QScrollArea* docsScroll = new QScrollArea(this);
    docsScroll->setWidgetResizable(true);
    docsScroll->setFrameShape(QFrame::NoFrame);

    // Inner widget and layout
    QWidget* docsInner = new QWidget();
    QBoxLayout* docsLayout = new QVBoxLayout(docsInner);
    docsLayout->setContentsMargins(12, 12, 12, 12);
    docsLayout->setSpacing(6);

    // Section header
    QLabel* docsHeader = new QLabel("DOCUMENTATION", docsInner);
    docsHeader->setObjectName("section-header");

    // Field or type name
    _nameLabel = new QLabel(docsInner);
    _nameLabel->setObjectName("asset-title");
    _nameLabel->setWordWrap(true);

    // Type and required/optional metadata line
    _metaLabel = new QLabel(docsInner);
    _metaLabel->setObjectName("docs-meta");
    _metaLabel->setWordWrap(true);
    _metaLabel->setVisible(false);

    // Horizontal separator between metadata and documentation text
    _separator = new QFrame(docsInner);
    _separator->setObjectName("docs-separator");
    _separator->setFrameShape(QFrame::HLine);
    _separator->setVisible(false);

    // Short one-line description
    _descriptionLabel = new QLabel(docsInner);
    _descriptionLabel->setObjectName("docs-description");
    _descriptionLabel->setWordWrap(true);
    _descriptionLabel->setVisible(false);

    // Full documentation browser with markdown rendering
    _textBrowser = new QTextBrowser(docsInner);
    _textBrowser->setObjectName("docs-browser");
    // Prevent links from opening a browser; docs are display-only
    _textBrowser->setOpenExternalLinks(false);
    // Allow text selection but disable clickable links
    _textBrowser->setTextInteractionFlags(
        Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard
    );
    // Remove the default sunken border to blend with the panel background
    _textBrowser->setFrameShape(QFrame::NoFrame);
    // Placeholder shown before any field is selected
    _textBrowser->setMarkdown("*Select a setting to see its documentation here.*");

    // Assemble layout
    docsLayout->addWidget(docsHeader);
    docsLayout->addWidget(_nameLabel);
    docsLayout->addWidget(_metaLabel);
    docsLayout->addWidget(_separator);
    docsLayout->addWidget(_descriptionLabel);
    docsLayout->addWidget(_textBrowser, 1);
    docsScroll->setWidget(docsInner);

    // Outer layout
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->addWidget(docsScroll);
}
