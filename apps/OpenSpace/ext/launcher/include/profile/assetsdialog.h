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

#ifndef __OPENSPACE_UI_LAUNCHER___ASSETSDIALOG___H__
#define __OPENSPACE_UI_LAUNCHER___ASSETSDIALOG___H__

#include <QDialog>
#include <QRegularExpression>
#include <QSortFilterProxyModel>
#include "assettreemodel.h"

class QTextEdit;
class QTreeView;

class SearchProxyModel : public QSortFilterProxyModel {
Q_OBJECT
public:
    /**
     * Constructor for SearchProxyModel class.
     *
     * \param parent The parent object of this object
     */
    SearchProxyModel(QObject* parent = nullptr);

public slots:
    /**
     * Sets the regular expression pattern to apply to the filter.
     *
     * \param pattern The regex pattern used for filtering
     */
    void setFilterRegularExpression(const QString& pattern);

protected:
    bool filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const override;

private:
    bool acceptIndex(const QModelIndex& idx) const;

    QRegularExpression* _regExPattern = nullptr;
};

class AssetsDialog final : public QDialog {
Q_OBJECT
public:
    /**
     * Constructor for assets class.
     *
     * \param parent Pointer to parent Qt widget
     * \param profile The #openspace::Profile object containing all data of the new or
     *        imported profile
     * \param assetBasePath The path to the folder in which all of the assets are living
     * \param userAssetBasePath The path to the folder in which the users' assets are
     *        living
     */
    AssetsDialog(QWidget* parent, openspace::Profile* profile,
        const std::filesystem::path& assetBasePath,
        const std::filesystem::path& userAssetBasePath);

private slots:
    void searchTextChanged(const QString& text);

private:
    void createWidgets();
    void setViewToBaseModel();
    void parseSelections();
    void selected(const QModelIndex&);

    /**
     * Creates a text summary of all assets and their paths.
     */
    QString createTextSummary();
    void openAssetEditor();

    openspace::Profile* _profile = nullptr;
    AssetTreeModel _assetTreeModel;
    QTreeView* _assetTree = nullptr;
    QLineEdit* _searchTextBox = nullptr;
    QTextEdit* _summary = nullptr;
    QSortFilterProxyModel* _assetProxyModel = nullptr;
    SearchProxyModel* _searchProxyModel = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___ASSETSDIALOG___H__
