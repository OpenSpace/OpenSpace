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

#ifndef __OPENSPACE_UI_LAUNCHER___ADDITIONALSCRIPTS___H__
#define __OPENSPACE_UI_LAUNCHER___ADDITIONALSCRIPTS___H__

#include <QDialog>

namespace openspace { class Profile; }

class QTextEdit;

class AdditionalScriptsDialog : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for addedScripts class
     *
     * \param profile The #openspace::Profile object containing all data of the
     *                new or imported profile.
     * \param parent Pointer to parent Qt widget
     */
    AdditionalScriptsDialog(openspace::Profile& profile, QWidget* parent);

private slots:
    void parseScript();

private:
    void createWidgets();

    openspace::Profile& _profile;
    QTextEdit* _textScripts = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___ADDITIONALSCRIPTS___H__
