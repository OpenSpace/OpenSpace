/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include "profile/assetedit.h"
#include "profile/line.h"
#include <openspace/scene/profile.h>
#include <ghoul/fmt.h>
#include <QDialogButtonBox>
#include <QHeaderView>
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>
#include <QTextEdit>
#include <QTreeView>

namespace {
    bool traverseToExpandSelectedItems(QTreeView& tree, AssetTreeModel& model, int rows,
                                       QModelIndex parent)
    {
        bool isExpanded = false;

        for (int r = 0; r < rows; r++) {
            QModelIndex idx = model.index(r, 0, parent);

            if (!model.isAsset(idx)) {
                const int nChildRows = model.childCount(idx);
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
        int startIndex = 0;
        const std::string token = "${USER_ASSETS}/";
        if (path.starts_with(token)) {
            startIndex = static_cast<int>(token.length());
        }
        const size_t slash = path.find_first_of('/', startIndex);
        const bool endOfPath = (slash == std::string::npos);
        const std::string firstDir = endOfPath ? "" : path.substr(0, slash);

        if (!endOfPath) {
            const std::string nextPath = (slash == std::string::npos) ?
                path :
                path.substr(slash + 1);
            bool foundDirMatch = false;
            for (int r = 0; r < nRows; r++) {
                QModelIndex idx = model.index(r, 0, parent);
                const std::string assetName = model.name(idx).toStdString();
                if (!model.isAsset(idx)) {
                    if (firstDir == assetName) {
                        const int nChildRows = model.childCount(idx);
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
                const std::string assetName = model.name(idx).toStdString();
                // Need to check if it actually is an asset to prevent issue #2154
                if (model.isAsset(idx) && path == assetName) {
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

AssetsDialog::AssetsDialog(QWidget* parent, openspace::Profile* profile,
                           const std::string& assetBasePath,
                           const std::string& userAssetBasePath)
    : QDialog(parent)
    , _profile(profile)
{
    setWindowTitle("Assets");
    _assetTreeModel.importModelData(assetBasePath, userAssetBasePath);

    QBoxLayout* layout = new QVBoxLayout(this);
    {
        QGridLayout* container = new QGridLayout;
        container->setColumnStretch(1, 1);

        QLabel* heading = new QLabel("Select assets from /data/assets");
        heading->setObjectName("heading");
        container->addWidget(heading, 0, 0);

        QPushButton* newAssetButton = new QPushButton("New Asset");
        connect(
            newAssetButton, &QPushButton::released,
            this, &AssetsDialog::openAssetEditor
        );
        newAssetButton->setCursor(Qt::PointingHandCursor);
        newAssetButton->setDefault(false);
        container->addWidget(newAssetButton, 0, 2);
        layout->addLayout(container);

        QLabel* searchLabel = new QLabel("Search: ");
        searchLabel->setObjectName("heading");
        _searchTextBox = new QLineEdit;
        _searchTextBox->setClearButtonEnabled(true);
        QBoxLayout* layoutSearchBox = new QHBoxLayout;
        layoutSearchBox->addWidget(searchLabel);
        layoutSearchBox->addWidget(_searchTextBox);
        layout->addLayout(layoutSearchBox);
    }
    {
        _assetTree = new QTreeView;
        _assetTree->setToolTip(
            "Expand arrow entries to browse assets in this OpenSpace installation. "
            "Enable checkbox to include an asset. Those assets highlighted in red are "
            "present in the profile but do not exist in this OpenSpace installation"
        );
        setViewToBaseModel();
        connect(_assetTree, &QTreeView::clicked, this, &AssetsDialog::selected);


        for (const std::string& a : _profile->assets) {
            const QModelIndex p = _assetTreeModel.index(-1, 0);
            const int nRows = _assetTreeModel.rowCount(p);
            traverseToFindFilesystemMatch(_assetTreeModel, p, nRows, a);
        }

        const int nRows = _assetTreeModel.rowCount(_assetTreeModel.index(-1, 0));
        traverseToExpandSelectedItems(
            *_assetTree,
            _assetTreeModel,
            nRows,
            _assetTreeModel.index(-1, 0)
        );
        layout->addWidget(_assetTree, 4);
    }
    {
        QWidget* box = new QWidget;
        QBoxLayout* boxLayout = new QVBoxLayout(box);
        QLabel* summaryHeading = new QLabel("Selection summary");
        summaryHeading->setObjectName("heading");
        boxLayout->addWidget(summaryHeading);
        _summary = new QTextEdit;
        _summary->setReadOnly(true);
        _summary->setText(createTextSummary());
        boxLayout->addWidget(_summary);
        layout->addWidget(box, 1);
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
        connect(
            _searchTextBox, &QLineEdit::textChanged,
            this, &AssetsDialog::searchTextChanged
        );
        layout->addWidget(buttons);
    }
    {
        _searchProxyModel = new SearchProxyModel(this);
        _searchProxyModel->setSourceModel(&_assetTreeModel);
        _assetProxyModel = qobject_cast<QSortFilterProxyModel*>(_searchProxyModel);
    }
}

void AssetsDialog::setViewToBaseModel() {
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
}

QString AssetsDialog::createTextSummary() {
    std::vector<std::string> summaryPaths;
    std::vector<AssetTreeItem*> summaryItems;
    _assetTreeModel.getSelectedAssets(summaryPaths, summaryItems);

    if (summaryPaths.size() != summaryItems.size()) {
        return "";
    }
    QString summary;
    for (size_t i = 0; i < summaryItems.size(); i++) {
        const bool existsInFilesystem = summaryItems.at(i)->doesExistInFilesystem();

        const std::string s = existsInFilesystem ?
            std::format("{}<br>", summaryPaths.at(i)) :
            std::format("<font color='red'>{}</font><br>", summaryPaths.at(i));
        summary += QString::fromStdString(s);
    }
    return summary;
}

void AssetsDialog::openAssetEditor() {
    AssetEdit editor(this);
    editor.exec();
}

void AssetsDialog::parseSelections() {
    _profile->assets.clear();
    std::vector<std::string> summaryPaths;
    std::vector<AssetTreeItem*> summaryItems;
    _assetTreeModel.getSelectedAssets(summaryPaths, summaryItems);

    for (const std::string& sel : summaryPaths) {
        _profile->addAsset(sel);
    }
    accept();
}

void AssetsDialog::selected(const QModelIndex&) {
    _summary->setText(createTextSummary());
}

void AssetsDialog::searchTextChanged(const QString& text) {
    if (!_assetProxyModel) {
        return;
    }
    if (text.isEmpty()) {
        setViewToBaseModel();
        traverseToExpandSelectedItems(
            *_assetTree,
            _assetTreeModel,
            _assetTreeModel.rowCount(_assetTreeModel.index(-1, 0)),
            _assetTreeModel.index(-1, 0)
        );
    }
    else {
        _assetTree->setModel(_searchProxyModel);
        _searchProxyModel->setFilterRegularExpression(text);
        _assetTree->expandAll();
    }
}

SearchProxyModel::SearchProxyModel(QObject* parent)
    : QSortFilterProxyModel(parent)
{}

void SearchProxyModel::setFilterRegularExpression(const QString& pattern) {
    _regExPattern = new QRegularExpression(
        pattern,
        QRegularExpression::CaseInsensitiveOption
    );
    QSortFilterProxyModel::setFilterRegularExpression(*_regExPattern);
}

bool SearchProxyModel::filterAcceptsRow(int sourceRow,
                                        const QModelIndex& sourceParent) const
{
    const QModelIndex idx = sourceModel()->index(sourceRow, 0, sourceParent);
    return acceptIndex(idx);
}

bool SearchProxyModel::acceptIndex(const QModelIndex& idx) const {
    if (!idx.isValid() || !_regExPattern) {
        return false;
    }
    const QString text = idx.data(Qt::DisplayRole).toString();
    const QRegularExpressionMatchIterator matchIt = _regExPattern->globalMatch(text);
    if (matchIt.hasNext()) {
        return true;
    }
    for (int row = 0; row < idx.model()->rowCount(idx); ++row) {
        const bool accept = acceptIndex(idx.model()->index(row, 0, idx));
        if (accept) {
            return true;
        }
    }

    return false;
}
