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

#ifndef CAMERA_H
#define CAMERA_H

#include <QDialog>
#include <QWidget>
#include <QLineEdit>
#include <QLabel>
#include <optional>
#include <variant>

QT_BEGIN_NAMESPACE
namespace Ui {
class camera;
}
QT_END_NAMESPACE

class camera: public QDialog
{
    Q_OBJECT

public slots:
    void cancel();
    void approved();
    void tabSelect(int);

public:
    /**
     * Constructor for camera gui class
     *
     * \param imported The #openspace::Profile object containing all data of the
     *                 new or imported profile.
     * \param parent Pointer to parent Qt widget (optional)
     */
    explicit camera(openspace::Profile* imported, QWidget *parent = nullptr);

    /**
     * Destructor for camera gui class
     */
    ~camera();
    enum class cameraTypeTab : int {
        Nav = 0,
        Geo
    };
    /**
     * Handles keypress while the Qt dialog window is open
     *
     * \param evt #QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent *evt);

private:
    bool isEmpty(QLineEdit* le);
    bool isNumericalValue(QLineEdit* le);
    bool inNumericalRange(QLineEdit* le, float min, float max);
    void addErrorMsg(const QString& errorDescription);
    bool areRequiredFormsFilledAndValid();
    void setErrorTextFormat(QLabel* label, const QString& labelTxt, bool setErrorFormat);
    bool isUpVectorValid();

    Ui::camera *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    openspace::Profile::CameraType _data;
};

#endif // CAMERA_H
