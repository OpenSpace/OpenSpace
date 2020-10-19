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

#ifndef __OPENSPACE_UI_LAUNCHER___CAMERA___H__
#define __OPENSPACE_UI_LAUNCHER___CAMERA___H__

#include <QDialog>

namespace openspace { class Profile; }

class QLabel;
class QLineEdit;
class QTabWidget;

class CameraDialog : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for camera gui class
     *
     * \param profile The #openspace::Profile object containing all data of the
     *                new or imported profile.
     * \param parent Pointer to parent Qt widget (optional)
     */
    CameraDialog(openspace::Profile& profile, QWidget* parent);

private slots:
    void approved();
    void tabSelect(int);

private:
    void createWidgets();
    QWidget* createNavStateWidget();
    QWidget* createGeoWidget();

    void addErrorMsg(QString errorDescription);
    bool areRequiredFormsFilledAndValid();

    openspace::Profile& _profile;
    QTabWidget* _tabWidget = nullptr;
    struct {
        QLineEdit* anchor = nullptr;
        QLineEdit* aim = nullptr;
        QLineEdit* refFrame = nullptr;
        QLineEdit* positionX = nullptr;
        QLineEdit* positionY = nullptr;
        QLineEdit* positionZ = nullptr;
        QLineEdit* upX = nullptr;
        QLineEdit* upY = nullptr;
        QLineEdit* upZ = nullptr;
        QLineEdit* yaw = nullptr;
        QLineEdit* pitch = nullptr;
    } _navState;

    struct {
        QLineEdit* anchor = nullptr;
        QLineEdit* latitude = nullptr;
        QLineEdit* longitude = nullptr;
        QLineEdit* altitude = nullptr;
    } _geoState;

    QLabel* _errorMsg = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___CAMERA___H__
