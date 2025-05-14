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

#ifndef __OPENSPACE_UI_LAUNCHER___DELTATIMESDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___DELTATIMESDIALOG___H__

#include <QDialog>

class QDialogButtonBox;
class QLabel;
class QListWidget;
class QLineEdit;
class QPushButton;

class DeltaTimesDialog final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for deltaTimes class.
     *
     * \param parent Pointer to parent Qt widget
     * \param deltaTimes The list of delta times containing all data of the new or
     *        imported profile
     */
    DeltaTimesDialog(QWidget* parent, std::vector<double>* deltaTimes);

    /**
     * Returns a text summary of the delta time list for display purposes.
     *
     * \param idx index in dt list
     * \param forListView true if this summary is for the Qt list view, false if it is
     *        used for a different display mode
     */
    std::string createSummaryForDeltaTime(size_t idx, bool forListView);

    /**
     * Handles keypress while the Qt dialog window is open.
     *
     * \param evt QKeyEvent object for the key press event
     */
    virtual void keyPressEvent(QKeyEvent* evt) override;

private:
    void createWidgets();

    void listItemSelected();
    void valueChanged(const QString& text);
    void saveDeltaTimeValue();
    void discardDeltaTimeValue();
    void addDeltaTimeValue();
    void removeDeltaTimeValue();
    void parseSelections();

    /**
     * Called to transition to editing a particular dt value (gui settings).
     *
     * \param index Index in dt list
     * \param state `true` if the edit mode should be turned on, `false` otherwise
     */
    void transitionEditMode(int index, bool state);

    void setLabelForKey(int index, bool editMode, std::string_view color);
    bool isLineEmpty(int index);

    std::vector<double>* _deltaTimes = nullptr;
    std::vector<double> _deltaTimesData;
    bool _editModeNewItem = false;

    QListWidget* _listWidget = nullptr;
    QLabel* _adjustLabel = nullptr;
    QLineEdit* _seconds = nullptr;
    QLabel* _value = nullptr;

    QPushButton* _addButton = nullptr;
    QPushButton* _removeButton = nullptr;
    QPushButton* _saveButton = nullptr;
    QPushButton* _discardButton = nullptr;
    QDialogButtonBox* _buttonBox = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___DELTATIMESDIALOG___H__
