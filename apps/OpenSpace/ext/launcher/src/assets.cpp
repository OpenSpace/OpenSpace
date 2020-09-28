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

#include "assets.h"
#include "./ui_assets.h"
#include <qevent.h>
#include <QFileSystemModel>
#include <QScreen>
#include <QKeyEvent>
#include <sstream>
#include <string>
#include <openspace/scene/profile.h>

assets::assets(openspace::Profile* imported, const std::string reportAssets,
               QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::assets)
    , _imported(imported)
    , _assetTreeModel(tr("Asset"), tr("Enabled"), tr("Variable Name"))
{
    ui->setupUi(this);

    _assetTreeModel.importModelData(reportAssets);

    ui->treeView->setModel(&_assetTreeModel);
    ui->treeView->setRootIndex(_assetTreeModel.index(-1, 0));
    ui->treeView->setColumnWidth(0, ui->treeView->width() * 0.63);
    ui->treeView->setColumnWidth(1, ui->treeView->width() * 0.12);
    ui->treeView->setColumnWidth(2, ui->treeView->width() * 0.25);
    ui->treeView->setAnimated(true);
    ui->treeView->setSortingEnabled(false);
    ui->treeView->setSelectionMode(QAbstractItemView::SingleSelection);

    compareFilesystemWithProfileAssets();

    int nRows = _assetTreeModel.rowCount(_assetTreeModel.index(-1, 0));
    traverseToExpandSelectedItems(nRows, _assetTreeModel.index(-1, 0));

    connect(ui->treeView,
            SIGNAL(clicked(const QModelIndex&)),
            this,
            SLOT(selected(const QModelIndex&))
    );
    connect(ui->varName, SIGNAL(clicked()), this, SLOT(setVarName()));
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
}

void assets::compareFilesystemWithProfileAssets() {
    for (openspace::Profile::Asset a : _imported->assets()) {
        findPathMatch(a.path, a.name);
    }
}

void assets::findPathMatch(std::string& path, std::string& varName) {
    QModelIndex parent = _assetTreeModel.index(-1, 0);
    int nRows = _assetTreeModel.rowCount(parent);
    traverseToFindFilesystemMatch(parent, nRows, path, varName);
}

void assets::traverseToFindFilesystemMatch(QModelIndex parent, int nRows,
                                           std::string path, std::string varName)
{
    size_t slash = path.find_first_of('/', 0);
    bool endOfPath = (slash == std::string::npos);
    std::string firstDir = endOfPath ? "" : path.substr(0, slash);

    if (!endOfPath) {
        std::string nextPath = (slash == std::string::npos) ? path :
            path.substr(slash + 1);
        bool foundDirMatch = false;
        for (int r = 0; r < nRows; r++) {
            QModelIndex idx = _assetTreeModel.index(r, 0, parent);
            std::string assetName = _assetTreeModel.name(idx).toUtf8().constData();
            if (!_assetTreeModel.isAsset(idx)) {
                if (firstDir.compare(assetName) == 0) {
                    int nChildRows = _assetTreeModel.childCount(idx);
                    foundDirMatch = true;
                    traverseToFindFilesystemMatch(idx, nChildRows, nextPath, varName);
                    break;
                }
            }
            else {
                continue;
            }
        }
        if (!foundDirMatch) {
            //Insert missing directory here with name and exists=false condition
            _assetTreeModel.assetItem(parent)->insertChildren(nRows, 1, 3);
            QModelIndex idx = _assetTreeModel.index(nRows, 0, parent);
            _assetTreeModel.setName(idx, QString(firstDir.c_str()));
            _assetTreeModel.setExistenceInFilesystem(idx, false);
            traverseToFindFilesystemMatch(idx, 0, nextPath, varName);
        }
    }
    else {
        bool foundFileMatch = false;
        for (int r = 0; r < nRows; r++) {
            QModelIndex idx = _assetTreeModel.index(r, 0, parent);
            std::string assetName = _assetTreeModel.name(idx).toUtf8().constData();

            if (path.compare(assetName) == 0) {
                foundFileMatch = true;
                _assetTreeModel.setChecked(idx, true);
                _assetTreeModel.setVarName(idx, QString(varName.c_str()));
                break;
            }
        }
        if (!foundFileMatch) {
            //Insert missing file here with name and exists=false condition
            _assetTreeModel.assetItem(parent)->insertChildren(nRows, 1, 3);
            QModelIndex idx = _assetTreeModel.index(nRows, 0, parent);
            _assetTreeModel.setName(idx, QString(path.c_str()));
            _assetTreeModel.setChecked(idx, true);
            _assetTreeModel.setVarName(idx, QString(varName.c_str()));
            _assetTreeModel.setExistenceInFilesystem(idx, false);
        }
    }
}

bool assets::traverseToExpandSelectedItems(int nRows, QModelIndex parent) {
    bool isExpanded = false;

    for (int r = 0; r < nRows; r++) {
        QModelIndex idx = _assetTreeModel.index(r, 0, parent);

        if (!_assetTreeModel.isAsset(idx)) {
            int nChildRows = _assetTreeModel.childCount(idx);
            if (traverseToExpandSelectedItems(nChildRows, idx)) {
                ui->treeView->setExpanded(idx, true);
                isExpanded = true;
            }
        }
        else if (_assetTreeModel.isChecked(idx) || !_assetTreeModel.inFilesystem(idx)) {
            isExpanded = true;
        }
    }
    return isExpanded;
}

std::string assets::createTextSummary() {
    std::string summary;
    for (openspace::Profile::Asset sel : _assetTreeModel.selectedAssets()) {
        summary += sel.path + "  " + sel.name + "\n";
    }
    return summary;
}

void assets::parseSelections() {
    _imported->clearAssets();
    for (openspace::Profile::Asset sel : _assetTreeModel.selectedAssets()) {
        _imported->addAsset(sel.path, sel.name);
    }
    accept();
}

void assets::selected(const QModelIndex& sel) {
    _selectedIdx = sel;
    QString existingVarName = _assetTreeModel.varName(_selectedIdx);
    ui->lineEdit->setText(existingVarName);
}

void assets::setVarName() {
    _assetTreeModel.setVarName(_selectedIdx, ui->lineEdit->text());
    ui->treeView->reset();
    int nRows = _assetTreeModel.rowCount(_assetTreeModel.index(-1, 0));
    traverseToExpandSelectedItems(nRows, _assetTreeModel.index(-1, 0));
}

assets::~assets() {
    delete ui;
}

void assets::cancel() {
}

void assets::keyPressEvent(QKeyEvent *evt)
{
    if(evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return)
        return;
    QDialog::keyPressEvent(evt);
}
