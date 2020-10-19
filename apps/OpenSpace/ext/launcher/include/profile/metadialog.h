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

#ifndef __OPENSPACE_UI_LAUNCHER___META___H__
#define __OPENSPACE_UI_LAUNCHER___META___H__

#include <QDialog>

namespace openspace { class Profile; }

class QLineEdit;
class QTextEdit;

class MetaDialog : public QDialog {
Q_OBJECT
public:
   /**
    * Constructor for meta class
    *
    * \param profile The #openspace::Profile object containing all data of the
    *                new or imported profile.
    * \param parent Pointer to parent Qt widget
    */
    MetaDialog(openspace::Profile& profile, QWidget* parent);

private slots:
    void save();

private:
    void createWidgets();

    openspace::Profile& _profile;

    QLineEdit* _nameEdit = nullptr;
    QLineEdit* _versionEdit = nullptr;
    QTextEdit* _descriptionEdit = nullptr;
    QLineEdit* _authorEdit = nullptr;
    QLineEdit* _urlEdit = nullptr;
    QLineEdit* _licenseEdit = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___META___H__
