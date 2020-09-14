#include "assets.h"
#include "./ui_assets.h"
#include <qevent.h>
#include <QFileSystemModel>
#include <QScreen>
#include <sstream>
#include <string>
#include <openspace/scene/profile.h>

assets::assets(openspace::Profile* imported, std::string& reportAssets, QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::assets)
    , _imported(imported)
    , _assetTreeModel(tr("Asset"), tr("Enabled"))
{
    ui->setupUi(this);

    _assetTreeModel.importModelData(reportAssets);

    ui->treeView->setModel(&_assetTreeModel);
    ui->treeView->setRootIndex(_assetTreeModel.index(-1, 0));
    ui->treeView->setColumnWidth(0, ui->treeView->width() * 0.8);
    ui->treeView->setAnimated(true);
    ui->treeView->setSortingEnabled(false);

    compareFilesystemWithProfileAssets();

    int nRows = _assetTreeModel.rowCount(_assetTreeModel.index(-1, 0));
    traverseToExpandSelectedItems(nRows, _assetTreeModel.index(-1, 0));

    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(parseSelections()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(cancel()));
}

void assets::compareFilesystemWithProfileAssets() {
    for (openspace::Profile::Asset a : _imported->assets()) {
        findPathMatch(a.path, a.name);
    }
}

void assets::findPathMatch(std::string& path, std::string& filename) {
    QModelIndex parent = _assetTreeModel.index(-1, 0);
    int nRows = _assetTreeModel.rowCount(parent);
    traverseToFindFilesystemMatch(parent, nRows, path, filename);
}

void assets::traverseToFindFilesystemMatch(QModelIndex parent, int nRows,
                                           std::string path, std::string filename)
{
    bool endOfPath = (path.length() == 0);
    size_t slash = path.find_first_of('/', 0);
    std::string firstDir = endOfPath ? path : path.substr(0, slash);

    if (!endOfPath) {
        std::string nextPath = (slash == std::string::npos) ? "" :
            path.substr(slash + 1);
        bool foundDirMatch = false;
        for (int r = 0; r < nRows; r++) {
            QModelIndex idx = _assetTreeModel.index(r, 0, parent);
            std::string assetName = _assetTreeModel.name(idx).toUtf8().constData();
            if (!_assetTreeModel.isAsset(idx)) {
                if (firstDir.compare(assetName) == 0) {
                    int nChildRows = _assetTreeModel.childCount(idx);
                    foundDirMatch = true;
                    traverseToFindFilesystemMatch(idx, nChildRows, nextPath, filename);
                    break;
                }
            }
            else {
                continue;
            }
        }
        if (!foundDirMatch) {
            //Insert missing directory here with name and exists=false condition
            _assetTreeModel.assetItem(parent)->insertChildren(nRows, 1, 2);
            QModelIndex idx = _assetTreeModel.index(nRows, 0, parent);
            _assetTreeModel.setName(idx, QString(firstDir.c_str()));
            _assetTreeModel.setExistenceInFilesystem(idx, false);
            traverseToFindFilesystemMatch(idx, 0, nextPath, filename);
        }
    }
    else {
        bool foundFileMatch = false;
        for (int r = 0; r < nRows; r++) {
            QModelIndex idx = _assetTreeModel.index(r, 0, parent);
            std::string assetName = _assetTreeModel.name(idx).toUtf8().constData();

            if (filename.compare(assetName) == 0) {
                foundFileMatch = true;
                _assetTreeModel.setChecked(idx, true);
                break;
            }
        }
        if (!foundFileMatch) {
            //Insert missing file here with name and exists=false condition
            _assetTreeModel.assetItem(parent)->insertChildren(nRows, 1, 2);
            QModelIndex idx = _assetTreeModel.index(nRows, 0, parent);
            _assetTreeModel.setName(idx, QString(filename.c_str()));
            _assetTreeModel.setChecked(idx, true);
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
    for (std::string line : _assetTreeModel.selectedAssets()) {
        summary += line + "\n";
    }
    return summary;
}

void assets::parseSelections() {
    _imported->clearAssets();
    for (std::string selected : _assetTreeModel.selectedAssets()) {
        openspace::Profile::Asset a;
        size_t slash = selected.find_last_of('/');
        if (slash == std::string::npos) {
            a.path = "";
            a.name = selected;
        }
        else {
            a.path = selected.substr(0, slash);
            a.name = selected.substr(slash + 1);
        }
	_imported->addAsset(a.path + "/" + a.name);
    }
    accept();
}

assets::~assets() {
    delete ui;
}

void assets::cancel() {
}
