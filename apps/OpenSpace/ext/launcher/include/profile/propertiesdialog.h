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

#ifndef __OPENSPACE_UI_LAUNCHER___PROPERTIESDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___PROPERTIESDIALOG___H__

#include <QDialog>

#include <openspace/scene/profile.h>

class QComboBox;
class QDialogButtonBox;
class QLabel;
class QLineEdit;
class QListWidget;
class QMessageBox;
class QPushButton;

class PropertiesDialog final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for properties class.
     *
     * \param parent Pointer to parent Qt widget
     * \param properties The #openspace::Profile::Property object containing all data of
     *        the new or imported profile
     */
    PropertiesDialog(QWidget* parent,
        std::vector<openspace::Profile::Property>* properties);

    /**
     * Handles keypress while the Qt dialog window is open.
     *
     * \param evt The QKeyEvent object for the key press event
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

    void selectLineFromScriptLog();

    void transitionFromEditMode();
    void editBoxDisabled(bool disabled);
    bool areRequiredFormsFilled();
    bool isLineEmpty(int index);

    std::vector<openspace::Profile::Property>* _properties = nullptr;
    std::vector<openspace::Profile::Property> _propertyData;
    bool _editModeNewItem = false;

    QListWidget* _list = nullptr;
    QPushButton* _addButton = nullptr;
    QPushButton* _removeButton = nullptr;

    QPushButton* _addFromScriptLog = nullptr;
    QLabel* _commandLabel = nullptr;
    QComboBox* _commandCombo = nullptr;
    QLabel* _propertyLabel = nullptr;
    QLineEdit* _propertyEdit = nullptr;
    QLabel* _valueLabel = nullptr;
    QLineEdit* _valueEdit = nullptr;
    QPushButton* _saveButton = nullptr;
    QPushButton* _cancelButton = nullptr;
    QDialogButtonBox* _buttonBox = nullptr;

    QMessageBox* _errorMsg = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___PROPERTIESDIALOG___H__
