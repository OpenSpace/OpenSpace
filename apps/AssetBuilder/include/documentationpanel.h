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

#ifndef __OPENSPACE_ASSETBUILDER___DOCUMENTATIONPANEL___H__
#define __OPENSPACE_ASSETBUILDER___DOCUMENTATIONPANEL___H__

#include <QWidget>

#include <documentation.h>

class QFrame;
class QLabel;
class QTextBrowser;

/**
 * Right-side documentation panel. Shows a field name, type, required/optional status,
 * short description, and full documentation text when the user clicks a field info button
 * in the editor.
 */
class DocumentationPanel final : public QWidget {
Q_OBJECT
public:
    explicit DocumentationPanel(QWidget* parent);

public slots:
    /**
     * Updates the panel to display documentation for the given field.
     *
     * \param info Documentation bundle with name, type, description, and documentation
     */
    void showDocumentation(const Documentation& info);

private:
    /// Display name of the documented field or type
    QLabel* _nameLabel = nullptr;
    /// "Type: X  Required/Optional" metadata line
    QLabel* _metaLabel = nullptr;
    /// Horizontal line between metadata and documentation text
    QFrame* _separator = nullptr;
    /// Short one-line description of the field
    QLabel* _descriptionLabel = nullptr;
    /// Full documentation text rendered from Markdown
    QTextBrowser* _textBrowser = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___DOCUMENTATIONPANEL___H__
