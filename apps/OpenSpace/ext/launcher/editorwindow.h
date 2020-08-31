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

#ifndef __OPENSPACE_LAUNCHER___EDITORWINDOW___H__
#define __OPENSPACE_LAUNCHER___EDITORWINDOW___H__

#include <QDir>
#include <QDialog>
#include <QLineEdit>
#include <QFileSystemModel>
#include <QTreeView>
#include "assettreemodel.h"
#include "filesystemaccess.h"

namespace Ui {
class editorwindow;
}

class editorwindow : public QDialog
{
    Q_OBJECT

public slots:
   void cancel();

public:
    explicit editorwindow(QString assetPath, const std::string& profileName,
        QWidget* parent = nullptr);
    ~editorwindow();
    std::vector<std::string> parseSelections();
    void setProfileName(QString name);
    void setStartTime(QString timeString);
    void setFeaturedNodes(QString featuredNodes);
    void setInitAnchorNode(QString initAnchorNode);
    void setCustomizations(QString customizations);

private:
    bool traverseToExpandSelectedItems(int nRows, QModelIndex parent);
    void loadProfile(std::string profilePath);
    Ui::editorwindow *ui;
    QString _helperTextProfileName = "Enter profile name";
    QString _helperTextStartTime = "YYYY-MM-DDThh:mm:ss (leave blank for default time)";
    QString _helperTextFeaturedNodes = "Comma-separated list (i.e. Moon, Earth, Sun)";
    QString _helperTextInitialAnchorNode = "i.e. Earth";
    QString _helperTextCustomizations;
    assetTreeModel _assetTreeModel;
    QWidget* _parent;
    filesystemAccess _filesystemAccess;
    QString _assetPath;
};

#endif // __OPENSPACE_LAUNCHER___EDITORWINDOW___H__
