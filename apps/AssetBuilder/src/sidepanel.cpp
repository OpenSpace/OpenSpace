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

SidePanel::SidePanel(QWidget* parent, JAsset& asset, std::filesystem::path& path)
    : QWidget(parent)
{
    setObjectName("side-panel");
    setMinimumWidth(0);

    QBoxLayout* root = new QVBoxLayout(this);
    root->setContentsMargins(0, 0, 0, 0);
    root->setSpacing(0);

    QSplitter* splitter = new QSplitter(Qt::Vertical, this);
    splitter->setChildrenCollapsible(false);
    splitter->setHandleWidth(4);
    root->addWidget(splitter);

    _contentsList = new ContentsListWidget(splitter, asset);
    splitter->addWidget(_contentsList);
    splitter->setStretchFactor(0, 2);

    _dependencies = new DependenciesWidget(splitter, asset, path);
    splitter->addWidget(_dependencies);
    splitter->setStretchFactor(1, 1);

    _metadata = new MetadataWidget(splitter, asset);
    splitter->addWidget(_metadata);
    splitter->setStretchFactor(2, 0);

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
    connect(_metadata, &MetadataWidget::assetModified, this, &SidePanel::assetModified);
}

void SidePanel::refreshAll() {
    _contentsList->refresh();
    _dependencies->refresh();
    _metadata->refresh();
}

void SidePanel::addDependency(const QString& filePath) {
    _dependencies->addDependency(filePath);
}
