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

#include "sidepanel.h"

#include "contentslistwidget.h"
#include "dependencieswidget.h"
#include "metadatawidget.h"
#include <QSplitter>
#include <QVBoxLayout>

SidePanel::SidePanel(QWidget* parent)
    : QWidget(parent)
{
    buildUi();
}

void SidePanel::setAsset(JAsset* asset) {
    _contentsList->setAsset(asset);
    _dependencies->setAsset(asset);
    _metadata->setAsset(asset);
}

void SidePanel::setFilePath(const std::filesystem::path& path) {
    _dependencies->setFilePath(path);
}

void SidePanel::refreshAll() {
    _contentsList->refresh();
    _dependencies->refresh();
    _metadata->refresh();
}

void SidePanel::addDependencyViaDialog() {
    _dependencies->addDependencyViaDialog();
}

void SidePanel::addDependency(const QString& filePath) {
    _dependencies->addDependency(filePath);
}

void SidePanel::buildUi() {
    setObjectName("side-panel");
    setMinimumWidth(0);

    QBoxLayout* root = new QVBoxLayout(this);
    root->setContentsMargins(0, 0, 0, 0);
    root->setSpacing(0);

    QSplitter* splitter = new QSplitter(Qt::Vertical, this);
    splitter->setChildrenCollapsible(false);
    splitter->setHandleWidth(4);

    _contentsList = new ContentsListWidget(splitter);
    _dependencies = new DependenciesWidget(splitter);
    _metadata = new MetadataWidget(splitter);

    // Wire child signals to own signals
    connect(
        _contentsList,
        &ContentsListWidget::selectionChanged,
        this,
        [this](int row) { emit selectionChanged(static_cast<size_t>(row)); }
    );
    connect(
        _contentsList, &ContentsListWidget::assetModified,
        this, &SidePanel::assetModified
    );
    connect(
        _dependencies, &DependenciesWidget::assetModified,
        this, &SidePanel::assetModified
    );
    connect(
        _metadata, &MetadataWidget::assetModified,
        this, &SidePanel::assetModified
    );

    // Sizing: contents gets most space, deps gets some, metadata its natural size
    splitter->addWidget(_contentsList);
    splitter->addWidget(_dependencies);
    splitter->addWidget(_metadata);
    splitter->setStretchFactor(0, 2);
    splitter->setStretchFactor(1, 1);
    splitter->setStretchFactor(2, 0);

    root->addWidget(splitter);
}
