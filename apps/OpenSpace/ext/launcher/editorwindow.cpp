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

#include "editorwindow.h"
#include "ui_editorwindow.h"
#include <QFileSystemModel>
#include <QScreen>
#include <sstream>
#include "launcherwindow.h"
#include "filesystemaccess.h"

std::string testContents =
"0Dir_1\n"
"0 File 1\n"
"0 File 2\n"
"0 Dir_x1\n"
"0  Dir_x2\n"
"0   Dir_x3\n"
"0    File x1\n"
"1   File x2\n"
"0  Dir_y1\n"
"0   Dir_y2\n"
"0    File y1\n"
"0  File y2\n"
"0 File 3\n"
"0File A\n"
"0Dir 2\n"
"0 Dir B1\n"
"1  File C1\n"
"0 File B2\n"
"0 Dir B1\n"
"0  File C1\n";

editorwindow::editorwindow(QString assetPath, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::editorwindow)
    , _assetTreeModel(tr("Asset"), tr("Enabled"))
    , _filesystemAccess(".asset", {"scene", "global", "customization", "examples"},
                        true, true)
    , _assetPath(assetPath)
{
    ui->setupUi(this);
    ui->lineEditProfile->setPlaceholderText(_helperTextProfileName);
    ui->lineEditStartTime->setPlaceholderText(_helperTextStartTime);
    ui->textEditFeaturedNodes->setPlaceholderText(_helperTextFeaturedNodes);
    ui->lineEditInitialAnchorNode->setPlaceholderText(_helperTextInitialAnchorNode);

    //Test section using QFileSystemModel
    std::string reportAssets = _filesystemAccess.useQtFileSystemModelToTraverseDir(
        _assetPath);
    _assetTreeModel.importModelData(reportAssets); //(testContents)

    ui->treeView->setModel(&_assetTreeModel);
    ui->treeView->setRootIndex(_assetTreeModel.index(-1, 0));
    ui->treeView->setColumnWidth(0, ui->treeView->width() * 0.8);
    ui->treeView->setAnimated(true);
    ui->treeView->setSortingEnabled(false);

    int column = 0;
    int nRows = _assetTreeModel.rowCount(_assetTreeModel.index(-1, column));
    traverseToExpandSelectedItems(nRows, _assetTreeModel.index(-1, 0));

    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
}

editorwindow::~editorwindow() {
    delete ui;
}

bool editorwindow::traverseToExpandSelectedItems(int nRows, QModelIndex parent) {
    bool isChecked = false;

    for (int r = 0; r < nRows; r++) {
        QModelIndex idx = _assetTreeModel.index(r, 0, parent);

        if (!_assetTreeModel.isItemAsset(idx)) {
            int nChildRows = _assetTreeModel.childCount(idx);
            if (traverseToExpandSelectedItems(nChildRows, idx)) {
                ui->treeView->setExpanded(idx, true);
                isChecked = true;
            }
        }
        else if (_assetTreeModel.isItemChecked(idx)) {
            isChecked = true;
        }
    }
    return isChecked;
}

void editorwindow::setProfileName(QString name) {
    _helperTextProfileName = name;
    ui->lineEditProfile->setText(_helperTextProfileName);
}

void editorwindow::setStartTime(QString timeString) {
    _helperTextStartTime = timeString;
    ui->lineEditStartTime->setText(_helperTextStartTime);
}

void editorwindow::setFeaturedNodes(QString featuredNodes) {
    _helperTextFeaturedNodes = featuredNodes;
    ui->textEditFeaturedNodes->setText(_helperTextFeaturedNodes);
}

void editorwindow::setInitAnchorNode(QString initAnchorNode) {
    _helperTextInitialAnchorNode = initAnchorNode;
    ui->lineEditInitialAnchorNode->setText(_helperTextInitialAnchorNode);
}

void editorwindow::setCustomizations(QString customizations) {
    _helperTextCustomizations = customizations;
    ui->textEditCustomizations->setText(_helperTextCustomizations);
}

std::vector<std::string> editorwindow::parseSelections() {
    return _assetTreeModel.selectedAssets();
}

void editorwindow::cancel() {

}
