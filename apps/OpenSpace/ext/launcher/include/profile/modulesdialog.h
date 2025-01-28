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

#ifndef __OPENSPACE_UI_LAUNCHER___MODULESDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___MODULESDIALOG___H__

#include <QDialog>

#include <openspace/scene/profile.h>

class QDialogButtonBox;
class QLabel;
class QLineEdit;
class QListWidget;
class QPushButton;

class ModulesDialog final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for modules class.
     *
     * \param parent Pointer to parent Qt widget
     * \param modules The #openspace::Profile::Module object containing all data of the
     *        new or imported profile
     */
    ModulesDialog(QWidget* parent, std::vector<openspace::Profile::Module>* modules);

    /**
     * Handles keypress while the Qt dialog window is open.
     *
     * \param evt QKeyEvent object for the key press event
     */
    virtual void keyPressEvent(QKeyEvent* evt) override;

private:
    void createWidgets();

    void listItemSelected();
    void listItemAdded();
    void listItemRemove();
    void listItemSave();
    void listItemCancelSave();
    void transitionToEditMode();
    void parseSelections();

    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    bool isLineEmpty(int index) const;

    std::vector<openspace::Profile::Module>* _modules = nullptr;
    std::vector<openspace::Profile::Module> _moduleData;
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
};

#endif // __OPENSPACE_UI_LAUNCHER___MODULESDIALOG___H__
