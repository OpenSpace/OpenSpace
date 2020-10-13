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

#ifndef __OPENSPACE_UI_LAUNCHER___KEYBINDINGS___H__
#define __OPENSPACE_UI_LAUNCHER___KEYBINDINGS___H__

#include <QDialog>

#include <openspace/scene/profile.h>
#include <QWidget>
#include <QListWidgetItem>

class QComboBox;
class QCheckBox;
class QTextEdit;
class QDialogButtonBox;
class QListWidget;
class QLabel;
class QPushButton;

class KeybindingsDialog : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for keybindings class
     *
     * \param imported The #openspace::Profile object containing all data of the
     *                 new or imported profile.
     * \param parent Pointer to parent Qt widget (optional)
     */
    KeybindingsDialog(openspace::Profile& profile, QWidget* parent);

    /**
     * Handles keypress while the Qt dialog window is open
     *
     * \param evt #QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent *evt);

public slots:
    void listItemSelected();
    void listItemAdded();
    void listItemRemove();
    void listItemSave();
    void listItemCancelSave();
    void transitionToEditMode();
    void parseSelections();
    void keySelected(int index);

private:
    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    int indexInKeyMapping(std::vector<int>& mapVector, int keyInt);
    bool areRequiredFormsFilled();
    std::string truncateString(std::string& s);
    void replaceChars(std::string& src, const std::string& from,
        const std::string& to);
    bool isLineEmpty(int index);

    openspace::Profile& _profile;
    std::vector<openspace::Profile::Keybinding> _data;
    std::vector<int> _mapModKeyComboBoxIndexToKeyValue;
    std::vector<int> _mapKeyComboBoxIndexToKeyValue;
    bool _editModeNewItem = false;

    QListWidget* _list = nullptr;
    QLabel* _keyModLabel = nullptr;
    QComboBox* _keyModCombo = nullptr;
    QLabel* _keyLabel = nullptr;
    QComboBox* _keyCombo = nullptr;
    QLabel* _nameLabel = nullptr;
    QLineEdit* _nameEdit = nullptr;
    QLabel* _guiPathLabel = nullptr;
    QLineEdit* _guiPathEdit = nullptr;
    QLabel* _documentationLabel = nullptr;
    QLineEdit* _documentationEdit = nullptr;
    QCheckBox* _localCheck = nullptr;
    QLabel* _scriptLabel = nullptr;
    QTextEdit* _scriptEdit = nullptr;
    
    QPushButton* _addButton = nullptr;
    QPushButton* _removeButton = nullptr;
    QPushButton* _saveButton = nullptr;
    QPushButton* _cancelButton = nullptr;
    QDialogButtonBox* _buttonBox = nullptr;

    QLabel* _errorMsg = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___KEYBINDINGS___H__
