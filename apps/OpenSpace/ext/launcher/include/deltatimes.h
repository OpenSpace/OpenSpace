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

#ifndef DELTATIMES_H
#define DELTATIMES_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class deltaTimes;
}
QT_END_NAMESPACE

static const int _defaultDeltaTimes[30] = {
    1, 2, 5, 10, 30,
    60, 120, 300, 600, 1800,
    3600, 7200, 10800, 21600, 43200,
    86400, 172800, 345600, 604800, 1209600,
    2592000, 5184000, 7776000, 15552000, 31536000,
    63072000, 157680000, 315360000, 630720000, 1576800000
};

class deltaTimes : public QDialog
{
    Q_OBJECT

public slots:
    void listItemSelected();
    void valueChanged(const QString& text);
    void saveDeltaTimeValue();
    void cancelDeltaTimeValue();
    void addDeltaTimeValue();
    void removeDeltaTimeValue();
    void parseSelections();

public:
    /**
     * Constructor for deltaTimes class
     *
     * \param imported The #openspace::Profile object containing all data of the
     *                 new or imported profile.
     * \param parent Pointer to parent Qt widget (optional)
     */
     explicit deltaTimes(openspace::Profile* _imported, QWidget *parent = nullptr);

     /**
       * Destructor for addedScripts class
       */
    ~deltaTimes();

    /**
     * Sets the full list of delta times (called when importing data)
     *
     * \param dt vector of delta time values
     */
    void setDeltaTimes(std::vector<double>& dt);

    /**
     * Returns a text summary of the delta time list for display purposes
     *
     * \param idx index in dt list
     * \param forListView true if this summary is for the Qt list view, false if
     *                    it is used for a different display mode
     */
    QString createSummaryForDeltaTime(size_t idx, bool forListView);

    /**
     * Handles keypress while the Qt dialog window is open
     *
     * \param evt #QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent *evt);

    /**
     * Called to transition to editing a particular dt value (gui settings)
     *
     * \param index index in dt list
     */
    void transitionToEditMode(int index);

    /**
     * Called to transition from editing a particular dt value (gui settings)
     *
     * \param index index in dt list
     */
    void transitionFromEditMode(int index);

    /**
     * Called to enable/disable edit GUI elements
     *
     * \param disabled sets bool condition for enabling/disabling edit elements
     */
    void editBoxDisabled(bool disabled);

    struct timeIntervals {
        int index;
        int secondsPerInterval;
        QString intervalName;
    };
    int _maxSize = sizeof(_defaultDeltaTimes) / sizeof(int);
    const int secondsPerYear = 31536000;
    const int secondsPerMonth = 18144000;
    const int secondsPerWeek = 604800;
    const int secondsPerDay = 86400;
    const int secondsPerHour = 3600;
    const int secondsPerMinute = 60;

private:
    QString timeDescription(int value);
    bool checkForTimeDescription(QString& description, QString unit,
        int interval, int value);
    void setLabelForKey(int index, bool editMode, QString color);
    QString checkForTimeDescription(int intervalIndex, int value);
    int lastSelectableItem();
    bool isNumericalValue(QLineEdit* le);
    bool isLineEmpty(int index);

    Ui::deltaTimes *ui;
    QWidget* _parent;

    openspace::Profile* _imported;
    std::vector<int> _data;
    std::vector<std::string> _deltaTimeStrings;
    bool _editModeNewItem = false;

    std::vector<timeIntervals> _timeIntervals = {
        {0, 31536000, "year"},
        {1, 18144000, "month"},
        {2, 604800,   "week"},
        {3, 86400,    "day"},
        {4, 3600,     "hour"},
        {5, 60,       "minute"},
        {6, 1,        "second"},
    };
};

#endif // DELTATIMES_H
