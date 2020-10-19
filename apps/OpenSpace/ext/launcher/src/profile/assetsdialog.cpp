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

#include "profile/assetsdialog.h"

#include "profile/line.h"
#include <openspace/scene/profile.h>
#include <QDialogButtonBox>
#include <QHeaderView>
#include <QLabel>
#include <QVBoxLayout>
#include <QTextEdit>
#include <QTreeView>

namespace {
    bool traverseToExpandSelectedItems(QTreeView& tree, AssetTreeModel& model,
                                       int rows, QModelIndex parent)
    {
        bool isExpanded = false;

        for (int r = 0; r < rows; r++) {
            QModelIndex idx = model.index(r, 0, parent);

            if (!model.isAsset(idx)) {
                int nChildRows = model.childCount(idx);
                if (traverseToExpandSelectedItems(tree, model, nChildRows, idx)) {
                    tree.setExpanded(idx, true);
                    isExpanded = true;
                }
            }
            else if (model.isChecked(idx) || !model.inFilesystem(idx)) {
                isExpanded = true;
            }
        }
        return isExpanded;
    }

    void traverseToFindFilesystemMatch(AssetTreeModel& model, QModelIndex parent,
                                       int nRows, const std::string& path)
    {
        const size_t slash = path.find_first_of('/', 0);
        const bool endOfPath = (slash == std::string::npos);
        std::string firstDir = endOfPath ? "" : path.substr(0, slash);

        if (!endOfPath) {
            std::string nextPath = (slash == std::string::npos) ?
                path :
                path.substr(slash + 1);
            bool foundDirMatch = false;
            for (int r = 0; r < nRows; r++) {
                QModelIndex idx = model.index(r, 0, parent);
                std::string assetName = model.name(idx).toStdString();
                if (!model.isAsset(idx)) {
                    if (firstDir == assetName) {
                        int nChildRows = model.childCount(idx);
                        foundDirMatch = true;
                        traverseToFindFilesystemMatch(model, idx, nChildRows, nextPath);
                        break;
                    }
                }
                else {
                    continue;
                }
            }
            if (!foundDirMatch) {
                // Insert missing directory here with name and exists=false condition
                model.assetItem(parent)->insertChildren(nRows, 1, 3);
                QModelIndex idx = model.index(nRows, 0, parent);
                model.setName(idx, QString::fromStdString(firstDir));
                model.setExistenceInFilesystem(idx, false);
                traverseToFindFilesystemMatch(model, idx, 0, nextPath);
            }
        }
        else {
            bool foundFileMatch = false;
            for (int r = 0; r < nRows; r++) {
                QModelIndex idx = model.index(r, 0, parent);
                std::string assetName = model.name(idx).toStdString();

                if (path == assetName) {
                    foundFileMatch = true;
                    model.setChecked(idx, true);
                    break;
                }
            }
            if (!foundFileMatch) {
                // Insert missing file here with name and exists=false condition
                model.assetItem(parent)->insertChildren(nRows, 1, 3);
                QModelIndex idx = model.index(nRows, 0, parent);
                model.setName(idx, QString::fromStdString(path));
                model.setChecked(idx, true);
                model.setExistenceInFilesystem(idx, false);
            }
        }
    }
} // namespace

AssetsDialog::AssetsDialog(openspace::Profile& profile, const std::string& assetBasePath,
                           QWidget* parent)
    : QDialog(parent)
    , _profile(profile)
{
    setWindowTitle("Assets");
    _assetTreeModel.importModelData(assetBasePath);
    createWidgets();
}

void AssetsDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QLabel* heading = new QLabel("Select assets from /data/assets");
        heading->setObjectName("heading");
        layout->addWidget(heading);
    }
    {
        _assetTree = new QTreeView;
        _assetTree->setToolTip(
            "Expand arrow entries to browse assets in this OpenSpace installation. "
            "Enable checkbox to include an asset. Those assets highlighted in red are "
            "present in the profile but do not exist in this OpenSpace installation."
        );
        _assetTree->setAlternatingRowColors(true);
        _assetTree->setModel(&_assetTreeModel);
        _assetTree->setRootIndex(_assetTreeModel.index(-1, 0));
        _assetTree->setColumnWidth(1, 60);
        _assetTree->setColumnWidth(0, _assetTree->width() - 60);
        _assetTree->header()->setSectionResizeMode(0, QHeaderView::Stretch);
        _assetTree->header()->setSectionResizeMode(1, QHeaderView::Fixed);
        _assetTree->header()->setStretchLastSection(false);
        _assetTree->setAnimated(true);
        _assetTree->setSortingEnabled(false);
        _assetTree->setSelectionMode(QAbstractItemView::SingleSelection);
        connect(_assetTree, &QTreeView::clicked, this, &AssetsDialog::selected);


        for (const std::string& a : _profile.assets()) {
            QModelIndex parent = _assetTreeModel.index(-1, 0);
            int nRows = _assetTreeModel.rowCount(parent);
            traverseToFindFilesystemMatch(_assetTreeModel, parent, nRows, a);
        }

        int nRows = _assetTreeModel.rowCount(_assetTreeModel.index(-1, 0));
        traverseToExpandSelectedItems(
            *_assetTree,
            _assetTreeModel,
            nRows,
            _assetTreeModel.index(-1, 0)
        );
        layout->addWidget(_assetTree);
    }
    {
        QLabel* summaryHeading = new QLabel("Selection summary");
        summaryHeading->setObjectName("heading");
        layout->addWidget(summaryHeading);
    }
    {
        _summary = new QTextEdit;
        _summary->setReadOnly(true);
        _summary->setText(createTextSummary());
        layout->addWidget(_summary);
    }

    layout->addWidget(new Line);

    {
        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(
            buttons, &QDialogButtonBox::accepted,
            this, &AssetsDialog::parseSelections
        );
        connect(
            buttons, &QDialogButtonBox::rejected,
            this, &AssetsDialog::reject
        );
        layout->addWidget(buttons);
    }
}

QString AssetsDialog::createTextSummary() {
    std::vector<std::string> summaryPaths;
    std::vector<AssetTreeItem*> summaryItems;
    _assetTreeModel.getSelectedAssets(summaryPaths, summaryItems);

    if (summaryPaths.size() != summaryItems.size()) {
        return "";
    }
    QString summary;
    for (int i = 0; i < summaryItems.size(); ++i) {
        bool existsInFilesystem = summaryItems.at(i)->doesExistInFilesystem();

        constexpr const char* ExistsFormat = "{}<br>";
        constexpr const char* NotExistsFormat = "<font color='{}'>{}</font><br>";

        std::string s = existsInFilesystem ?
            fmt::format("{}<br>", summaryPaths.at(i)) :
            fmt::format("<font color='red'>{}</font><br>", summaryPaths.at(i));
        summary += QString::fromStdString(s);
    }
    return summary;
}

void AssetsDialog::parseSelections() {
    _profile.clearAssets();
    std::vector<std::string> summaryPaths;
    std::vector<AssetTreeItem*> summaryItems;
    _assetTreeModel.getSelectedAssets(summaryPaths, summaryItems);

    for (const std::string& sel : summaryPaths) {
        _profile.addAsset(sel);
    }
    accept();
}

void AssetsDialog::selected(const QModelIndex& sel) {
    _summary->setText(createTextSummary());
}
