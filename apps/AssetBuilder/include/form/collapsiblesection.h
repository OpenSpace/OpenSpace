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

#ifndef __OPENSPACE_ASSETBUILDER___FORM_COLLAPSIBLESECTION___H__
#define __OPENSPACE_ASSETBUILDER___FORM_COLLAPSIBLESECTION___H__

#include <QFrame>

#include <documentation.h>

class QBoxLayout;
class QLabel;
class QPushButton;

/**
 * Material-style accordion panel.
 *
 * Header layout (full row is clickable):
 *   [i]  [Title]  [*] (stretch) [v/^ chevron]
 *
 * Clicking anywhere on the header toggles expand/collapse, except the info button.
 */
class CollapsibleSection final : public QFrame {
Q_OBJECT
public:
    /**
     * \param parent Parent widget
     * \param title Section header text
     * \param documentation Display name and Markdown documentation text for the
              documentation panel
     * \param sectionKey Schema member name used as the copy/paste identifier
     */
    explicit CollapsibleSection(QWidget* parent, const QString& title, bool isExpanded,
        bool isMandatory = false,
        std::optional<std::pair<QString, QString>> documentation = std::nullopt,
        QString sectionKey = "");

    /**
     * Replaces the body content widget. Any existing content is deleted. Pass `nullptr`
     * to clear the body. The section takes ownership by reparenting the widget to the
     * internal content frame, so Qt' parent-child hierarchy handles deletion
     * automatically.
     *
     * \param widget The new content widget, or `nullptr` to clear
     */
    void setContentWidget(QWidget* widget);

    /**
     * Returns the current content widget, or nullptr if none is set.
     *
     * \return The body content widget
     */
    QWidget* contentWidget() const;

    /**
     * Returns true when the body area is visible.
     *
     * \return true if the section is expanded
     */
    bool isExpanded() const;

    /**
     * Returns the property key set by setSectionKey, or empty.
     *
     * \return The section's schema member key
     */
    QString sectionKey() const;

    /**
     * Controls whether the Paste action is enabled in the context menu.
     *
     * \param isAvailable `true` to enable paste, `false` to grey it out
     */
    void setPasteAvailable(bool isAvailable);

signals:
    /**
     * Emitted when the user clicks the info button on a section with documentation.
     */
    void documentationRequested(const Documentation& info);

    /**
     * Emitted when the user clicks the copy button.
     */
    void copyRequested(const QString& key);

    /**
     * Emitted when the user clicks the paste button.
     */
    void pasteRequested(const QString& key);

protected:
    /**
     * Handles mouse clicks on the header frame: left-click toggles expand/collapse,
     * right-click shows the copy/paste context menu.
     *
     * \param object The object that received the event (expected to be _headerFrame)
     * \param event The event to filter
     * \return `true` if the event was handled, `false` to pass it through
     */
    bool eventFilter(QObject* object, QEvent* event) override;

private slots:
    /**
     * Toggles the expanded/collapsed state of the section.
     */
    void toggleExpanded();

private:
    /// Clickable header row
    QFrame* _headerFrame = nullptr;
    /// Info button, always visible
    QPushButton* _infoButton = nullptr;
    /// Blue * shown for required sections
    QLabel* _mandatoryLabel = nullptr;
    /// Expand/collapse indicator (right side)
    QLabel* _chevronLabel = nullptr;
    /// Collapsible body frame
    QFrame* _contentFrame = nullptr;
    /// Layout inside the content frame
    QBoxLayout* _frameLayout = nullptr;

    /// Documentation bundle emitted when the info button is clicked
    const Documentation _documentation;
    /// Schema member key for copy/paste context menu
    const QString _sectionKey;
    /// Whether the Paste action is enabled in the context menu
    bool _canPaste = false;

    /// Whether the body area is currently visible
    bool _isExpanded = true;
};

#endif // __OPENSPACE_ASSETBUILDER___FORM_COLLAPSIBLESECTION___H__
