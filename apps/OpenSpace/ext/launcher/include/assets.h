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

#ifndef ASSETS_H 
#define ASSETS_H

#include <QDialog>
#include <QWidget>
#include <QListWidgetItem>
#include "assettreemodel.h"
#include "filesystemaccess.h"
#include <openspace/scene/profile.h>

QT_BEGIN_NAMESPACE
namespace Ui {
class assets;
}
QT_END_NAMESPACE

class assets : public QDialog
{
    Q_OBJECT

public slots:
    void cancel();
    void parseSelections();
    void selected(const QModelIndex&);
    void setVarName();

public:
    /**
     * Constructor for assets class
     *
     * \param imported The #openspace::Profile object containing all data of the
     *                 new or imported profile.
     * \param reportAssets A full summary of all assets and their respective paths in
     *                     a text format unique to this application (more details are
     *                     in class #assetTreeModel)
     * \param parent Pointer to parent Qt widget (optional)
     */
    explicit assets(openspace::Profile* imported, const std::string reportAssets,
        QWidget *parent = nullptr);

    /**
     * Destructor for assets class
     */
    ~assets();

    /**
     * Creates a text summary of all assets and their paths
     *
     * \return the #std::string summary
     */
    std::string createTextSummary();

    /**
     * Handles keypress while the Qt dialog window is open
     *
     * \param evt #QKeyEvent object for the key press event
     */
    void keyPressEvent(QKeyEvent *evt);

protected:
    //void resizeEvent(QResizeEvent* event);

private:
    void compareFilesystemWithProfileAssets();
    bool traverseToExpandSelectedItems(int nRows, QModelIndex parent);
    void findPathMatch(std::string& path, std::string& varName);
    void traverseToFindFilesystemMatch(QModelIndex parent, int nRows,
        std::string dirname, std::string varName);
    Ui::assets *ui;
    QWidget* _parent;
    openspace::Profile* _imported;
    assetTreeModel _assetTreeModel;
    QModelIndex _selectedIdx;
};

#endif // ASSETS_H
