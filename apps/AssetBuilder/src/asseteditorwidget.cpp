/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include "asseteditorwidget.h"

#include "documentationpanel.h"
#include "editorpanel.h"
#include "identifierregistry.h"
#include "sidepanel.h"
#include <QDir>
#include <QFile>
#include <QHBoxLayout>
#include <QMessageBox>
#include <QSplitter>

namespace {
    constexpr int LeftPanelWidth = 240;
    constexpr int Stretch = 9999;
    constexpr int RightPanelWidth = 350;
} // namespace

AssetEditorWidget::AssetEditorWidget(QWidget* parent)
    : QWidget(parent)
{
    _identifierRegistry = new IdentifierRegistry(this);
    connect(
        _identifierRegistry,
        &IdentifierRegistry::duplicatesFound,
        this,
        [this](const QString& message) {
            QMessageBox::warning(this, "Duplicate Identifiers", message);
        }
    );

    QBoxLayout* root = new QHBoxLayout(this);
    root->setContentsMargins(0, 0, 0, 0);
    root->setSpacing(0);

    QSplitter* splitter = new QSplitter(Qt::Horizontal, this);
    splitter->setChildrenCollapsible(true);
    splitter->setHandleWidth(4);
    root->addWidget(splitter);


    //
    // Sidepanel
    //
    _sidePanel = new SidePanel(splitter, _asset, _filePath);
    connect(
        _sidePanel, &SidePanel::selectionChanged,
        this, &AssetEditorWidget::onSelectionChanged
    );
    connect(
        _sidePanel, &SidePanel::assetModified,
        this, &AssetEditorWidget::onContentModified
    );


    //
    // Editor
    //
    _editorPanel = new EditorPanel(splitter, _asset, _identifierRegistry);
    connect(
        _editorPanel, &EditorPanel::contentModified,
        this, &AssetEditorWidget::onContentModified
    );
    connect(
        _editorPanel, &EditorPanel::addDependency,
        _sidePanel, &SidePanel::addDependency
    );


    //
    // Documentation
    //
    _docsPanel = new DocumentationPanel(splitter);
    connect(
        _editorPanel, &EditorPanel::documentationRequested,
        _docsPanel, &DocumentationPanel::showDocumentation
    );


    splitter->setSizes({ LeftPanelWidth, Stretch, RightPanelWidth });
    splitter->setStretchFactor(0, 0);
    splitter->setStretchFactor(1, 1);
    splitter->setStretchFactor(2, 0);
}

bool AssetEditorWidget::isDirty() const {
    return _isDirty;
}

const std::filesystem::path& AssetEditorWidget::filePath() const {
    return _filePath;
}

QString AssetEditorWidget::displayName() const {
    QString name = _filePath.empty() ?
        "Untitled" :
        QString::fromStdWString(_filePath.filename().wstring());
    if (_isDirty) {
        name += " *";
    }
    return name;
}

QString AssetEditorWidget::displayPath() const {
    QString path = _filePath.empty() ?
        "Untitled" :
        QDir::toNativeSeparators(QString::fromStdWString(_filePath.wstring()));
    if (_isDirty) {
        path += " *";
    }
    return path;
}

void AssetEditorWidget::newAsset() {
    _asset = JAsset();
    _isDirty = false;
    _filePath.clear();
    refreshPanels();
    _editorPanel->showEmptyCenter();
}

bool AssetEditorWidget::loadAsset(const std::filesystem::path& path) {
    const QString qPath = QString::fromStdWString(path.wstring());
    QFile file = QFile(qPath);
    if (!file.open(QFile::ReadOnly)) {
        QMessageBox::critical(
            this,
            "Load Failed",
            "Could not open file:\n" + qPath + "\n\n" +
            file.errorString()
        );
        return false;
    }

    QJsonParseError err;
    const QJsonDocument doc = QJsonDocument::fromJson(file.readAll(), &err);
    if (doc.isNull() || !doc.isObject()) {
        const QString detail = err.error != QJsonParseError::NoError ?
            err.errorString() :
            "File does not contain a JSON object.";
        QMessageBox::critical(
            this,
            "Load Failed",
            "Could not parse file:\n" + qPath + "\n\n" + detail
        );
        return false;
    }

    _asset = jassetFromJson(doc.object());
    _isDirty = false;
    _filePath = path;
    refreshPanels();
    _editorPanel->showEmptyCenter();
    return true;
}

bool AssetEditorWidget::saveAsset(const std::filesystem::path& path) {
    const QJsonObject root = jassetToJson(_asset);

    QFile file = QFile(QString::fromStdWString(path.wstring()));
    // Truncate to avoid stale trailing bytes when new content is shorter
    if (!file.open(QFile::WriteOnly | QFile::Truncate)) {
        return false;
    }
    file.write(QJsonDocument(root).toJson(QJsonDocument::Indented));

    _isDirty = false;
    for (ContentItem& item : _asset.contents) {
        item.isDirty = false;
    }
    _filePath = path;
    refreshPanels();
    return true;
}

void AssetEditorWidget::onSelectionChanged(size_t row) {
    if (row >= _asset.contents.size()) {
        _editorPanel->showEmptyCenter();
        return;
    }
    _editorPanel->showItemEditor(row);
}

void AssetEditorWidget::onContentModified() {
    _isDirty = true;
    refreshPanels();
    emit assetModified();
}

void AssetEditorWidget::refreshPanels() {
    _identifierRegistry->rebuildFromAsset(_asset, _filePath);
    _sidePanel->refreshAll();
}
