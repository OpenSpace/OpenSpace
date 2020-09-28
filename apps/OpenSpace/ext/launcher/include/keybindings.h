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

#ifndef KEYBINDINGS_H
#define KEYBINDINGS_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>

QT_BEGIN_NAMESPACE
namespace Ui {
class keybindings;
}
QT_END_NAMESPACE

class keybindings : public QDialog
{
    Q_OBJECT

public slots:
    void listItemSelected();
    void listItemAdded();
    void listItemRemove();
    void listItemSave();
    void listItemCancelSave();
    void transitionToEditMode();
    void parseSelections();

public:
    /**
     * Constructor for keybindings class
     *
     * \param imported The #openspace::Profile object containing all data of the
     *                 new or imported profile.
     * \param parent Pointer to parent Qt widget (optional)
     */
    explicit keybindings(openspace::Profile* imported, QWidget *parent = nullptr);

    /**
     * Destructor for keybindings class
     */
    ~keybindings();

    /**
     * Handles keypress while the Qt dialog window is open
     *
     * \param evt #QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent *evt);

protected:
    //void resizeEvent(QResizeEvent* event);

private:
    QString createOneLineSummary(openspace::Profile::Keybinding k);
    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    int indexInKeyMapping(std::vector<int>& mapVector, int keyInt);
    bool areRequiredFormsFilled();
    std::string truncateString(std::string& s);
    void replaceChars(std::string& src, const std::string& from,
        const std::string& to);
    bool isLineEmpty(int index);

    Ui::keybindings *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    std::vector<openspace::Profile::Keybinding> _data;
    std::vector<QListWidgetItem*> _keybindingsListItems;
    std::vector<int> _mapModKeyComboBoxIndexToKeyValue;
    std::vector<int> _mapKeyComboBoxIndexToKeyValue;
    bool _editModeNewItem = false;
    const openspace::Profile::Keybinding kBlank = {
        {openspace::Key::Unknown, openspace::KeyModifier::NoModifier},
        "",
        "",
        "",
        true,
        ""
    };
};

#endif // KEYBINDINGS_H
