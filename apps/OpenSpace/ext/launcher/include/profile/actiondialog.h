/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_UI_LAUNCHER___ACTIONDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___ACTIONDIALOG___H__

#include <QDialog>

#include <openspace/scene/profile.h>

class QCheckBox;
class QComboBox;
class QDialogButtonBox;
class QGridLayout;
class QLabel;
class QLineEdit;
class QListWidget;
class QPushButton;
class QTextEdit;

class ActionDialog final : public QDialog {
Q_OBJECT
public:
    ActionDialog(QWidget* parent, std::vector<openspace::Profile::Action>* actions,
        std::vector<openspace::Profile::Keybinding>* keybindings);

private:
    void createWidgets();
    void createActionWidgets(QGridLayout* layout);
    void createKeyboardWidgets(QGridLayout* layout);
    void applyChanges();

    openspace::Profile::Action* selectedAction();
    void actionAdd();
    void actionRemove();
    void actionSelected();
    void actionSaved();
    void clearActionFields() const;
    void actionRejected();
    void chooseScripts();
    void appendScriptsToTextfield(const std::vector<std::string>& scripts) const;

    openspace::Profile::Keybinding* selectedKeybinding();
    void keybindingAdd();
    void keybindingRemove();
    void keybindingSelected();
    void keybindingActionSelected(int) const;
    void keybindingSaved();
    void clearKeybindingFields() const;
    void keybindingRejected();

    std::vector<openspace::Profile::Action>* _actions = nullptr;
    std::vector<openspace::Profile::Action> _actionData;
    std::vector<openspace::Profile::Keybinding>* _keybindings = nullptr;
    std::vector<openspace::Profile::Keybinding> _keybindingsData;

    struct {
        QListWidget* list = nullptr;
        QLineEdit* identifier = nullptr;
        QLabel* infoText = nullptr;
        QLineEdit* name = nullptr;
        QLineEdit* guiPath = nullptr;
        QLineEdit* documentation = nullptr;
        QCheckBox* isLocal = nullptr;
        QPushButton* chooseScripts = nullptr;
        QTextEdit* script = nullptr;
        QPushButton* addButton = nullptr;
        QPushButton* removeButton = nullptr;
        QDialogButtonBox* saveButtons = nullptr;
    } _actionWidgets;

    struct {
        QListWidget* list = nullptr;
        QCheckBox* shiftModifier = nullptr;
        QCheckBox* ctrlModifier = nullptr;
        QCheckBox* altModifier = nullptr;
        QComboBox* key = nullptr;
        QComboBox* action = nullptr;
        QLineEdit* actionText = nullptr;
        QPushButton* addButton = nullptr;
        QPushButton* removeButton = nullptr;
        QDialogButtonBox* saveButtons = nullptr;
    } _keybindingWidgets;

    QDialogButtonBox* _mainButton = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___ACTIONDIALOG___H__
