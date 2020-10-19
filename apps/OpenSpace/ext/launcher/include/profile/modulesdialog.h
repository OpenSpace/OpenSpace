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

#ifndef __OPENSPACE_UI_LAUNCHER___MODULES___H__
#define __OPENSPACE_UI_LAUNCHER___MODULES___H__

#include <QDialog>

#include <openspace/scene/profile.h>

class QDialogButtonBox;
class QLabel;
class QLineEdit;
class QListWidget;
class QPushButton;

class ModulesDialog : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for osmodules class
     *
     * \param profile The #openspace::Profile object containing all data of the
     *                new or imported profile.
     * \param parent Pointer to parent Qt widget
     */
    ModulesDialog(openspace::Profile& profile, QWidget* parent);

    /**
     * Handles keypress while the Qt dialog window is open
     *
     * \param evt #QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent* evt);

private slots:
    void listItemSelected();
    void listItemAdded();
    void listItemRemove();
    void listItemSave();
    void listItemCancelSave();
    void transitionToEditMode();
    void parseSelections();

private:
    void createWidgets();

    QString createOneLineSummary(openspace::Profile::Module m);
    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    bool isLineEmpty(int index) const;

    openspace::Profile& _profile;
    std::vector<openspace::Profile::Module> _data;
    bool _editModeNewItem = false;

    QListWidget* _list = nullptr;
    QLabel* _moduleLabel = nullptr;
    QLineEdit* _moduleEdit = nullptr;
    QLabel* _loadedLabel = nullptr;
    QLineEdit* _loadedEdit = nullptr;
    QLabel* _notLoadedLabel = nullptr;
    QLineEdit* _notLoadedEdit = nullptr;
    
    QPushButton* _buttonAdd = nullptr;
    QPushButton* _buttonRemove = nullptr;
    QPushButton* _buttonSave = nullptr;
    QPushButton* _buttonCancel = nullptr;
    QDialogButtonBox* _buttonBox = nullptr;

    QLabel* _errorMsg = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___MODULES___H__
