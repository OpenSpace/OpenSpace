/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_UI_LAUNCHER___MARKNODESDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___MARKNODESDIALOG___H__

#include <QDialog>

class QLineEdit;
class QListWidget;
class QListWidgetItem;
class QPushButton;

class MarkNodesDialog final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for markNodes class.
     *
     * \param parent Pointer to parent Qt widget
     * \param markedNodes The list of marked nodes containing all data of the new or
     *        imported profile
     */
    MarkNodesDialog(QWidget* parent, std::vector<std::string>* markedNodes);

    /**
     * Handles keypress while the Qt dialog window is open.
     *
     * \param evt QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent* evt) override;

private:
    void createWidgets();

    void listItemAdded();
    void listItemRemove();
    void parseSelections();

    std::vector<std::string>* _markedNodes;

    QListWidget* _list = nullptr;
    QPushButton* _removeButton = nullptr;
    QLineEdit* _newNode = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___MARKNODESDIALOG___H__
