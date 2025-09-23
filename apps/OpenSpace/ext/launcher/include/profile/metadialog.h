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

#ifndef __OPENSPACE_UI_LAUNCHER___METADIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___METADIALOG___H__

#include <QDialog>

#include <openspace/scene/profile.h>
#include <optional>

class QLineEdit;
class QTextEdit;

class MetaDialog final : public QDialog {
Q_OBJECT
public:
   /**
    * Constructor for meta class.
    *
    * \param parent Pointer to parent Qt widget
    * \param meta The #openspace::Profile::Meta object containing all data of the new or
    *        imported profile
    */
    MetaDialog(QWidget* parent, std::optional<openspace::Profile::Meta>* meta);

private:
    void createWidgets();

    void save();

    std::optional<openspace::Profile::Meta>* _meta = nullptr;

    QLineEdit* _nameEdit = nullptr;
    QLineEdit* _versionEdit = nullptr;
    QTextEdit* _descriptionEdit = nullptr;
    QLineEdit* _authorEdit = nullptr;
    QLineEdit* _urlEdit = nullptr;
    QLineEdit* _licenseEdit = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___METADIALOG___H__
