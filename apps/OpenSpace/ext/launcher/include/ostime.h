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

#ifndef __OPENSPACE_UI_LAUNCHER___OSTIME___H__
#define __OPENSPACE_UI_LAUNCHER___OSTIME___H__

#include <QDialog>

#include <openspace/scene/profile.h>
#include <QWidget>
#include <optional>



QT_BEGIN_NAMESPACE
namespace Ui {
class time;
}
QT_END_NAMESPACE

class ostime : public QDialog
{
    Q_OBJECT

public slots:
    void enableAccordingToType(int);
    void cancel();
    void approved();

public:
    /**
     * Constructor for ostime class
     *
     * \param imported The #openspace::Profile object containing all data of the
     *                 new or imported profile.
     * \param parent Pointer to parent Qt widget (optional)
     */
    explicit ostime(openspace::Profile* imported, QWidget *parent = nullptr);

    /**
     * Destructor for ostime class
     */
    ~ostime();

private:
    void enableFormatForAbsolute(bool enableAbs);
    Ui::time *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    openspace::Profile::Time _data;
    bool _initializedAsAbsolute = true;
};

#endif // __OPENSPACE_UI_LAUNCHER___OSTIME___H__
