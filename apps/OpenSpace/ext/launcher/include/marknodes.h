/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef MARKNODES_H
#define MARKNODES_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class markNodes;
}
QT_END_NAMESPACE

class markNodes : public QDialog
{
    Q_OBJECT

public slots:
    void listItemSelected();
    void listItemAdded();
    void listItemRemove();
    void parseSelections();

public:
    /**
     * Constructor for markNodes class
     *
     * \param imported The #openspace::Profile object containing all data of the
     *                 new or imported profile.
     * \param parent Pointer to parent Qt widget (optional)
     */
    explicit markNodes(openspace::Profile* imported, QWidget *parent = nullptr);

    /**
     * Destructor for markNodes class
     */
    ~markNodes();

    /**
     * Handles keypress while the Qt dialog window is open
     *
     * \param evt #QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent *evt);

private:
    Ui::markNodes *ui;
    QWidget* _parent;
    std::vector<QListWidgetItem*> _markedNodesListItems;
    openspace::Profile* _imported;
    std::vector<std::string> _data;
};

#endif // MARKNODES_H
