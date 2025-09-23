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

#ifndef __OPENSPACE_UI_LAUNCHER___ADDITIONALSCRIPTSDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___ADDITIONALSCRIPTSDIALOG___H__

#include <QDialog>

class QTextEdit;

class AdditionalScriptsDialog final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for addedScripts class.
     *
     * \param parent Pointer to parent Qt widget
     * \param scripts The list of scripts containing all data of the new or imported
     *        profile
     */
    AdditionalScriptsDialog(QWidget* parent, std::vector<std::string>* scripts);

private:
    void createWidgets();

    void parseScript();
    void chooseScripts();

    /**
     * Adds scripts to the _scriptEdit from outside dialogs.
     *
     * \param scripts The scripts to be appended
     */
    void appendScriptsToTextfield(const std::vector<std::string>& scripts);

    std::vector<std::string>* _scripts = nullptr;
    std::vector<std::string> _scriptsData;
    QTextEdit* _textScripts = nullptr;
    QPushButton* _chooseScriptsButton = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___ADDITIONALSCRIPTSDIALOG___H__
