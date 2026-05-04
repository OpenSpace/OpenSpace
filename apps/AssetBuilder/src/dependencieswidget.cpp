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

#include "dependencieswidget.h"

#include <jasset.h>
#include <ghoul/filesystem/filesystem.h>
#include <QDir>
#include <QFileDialog>
#include <QHBoxLayout>
#include <QLabel>
#include <QListWidget>
#include <QMenu>
#include <QMessageBox>
#include <QPushButton>

namespace {
    // We want to allow dependencies to be any file type
    // TODO (@ylvse 2026-04-19) Need to check if this is true?^
    constexpr const char* FileFilter = "Asset files (*.jasset);;All files (*)";

    // Icons for dependency status
    constexpr const char* IconExists = "\u2713";  // Checkmark
    constexpr const char* IconMissing = "\u26a0"; // Warning triangle

    QString pathTypeBadge(PathType type) {
        switch (type) {
            case PathType::Data:     return "[data]";
            case PathType::Relative: return "[rel]";
            case PathType::Absolute: return "[abs]";
            default:                 return "";
        }
    }
} // namespace

DependenciesWidget::DependenciesWidget(QWidget* parent, JAsset& asset,
                                       std::filesystem::path& path)
    : QWidget(parent)
    , _asset(asset)
    , _filePath(path)
{
    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    QBoxLayout* headerLayout = new QHBoxLayout;
    headerLayout->setContentsMargins(12, 8, 8, 4);
    headerLayout->setSpacing(4);
    layout->addLayout(headerLayout);

    QLabel* header = new QLabel("DEPENDENCIES", this);
    header->setObjectName("section-header");
    headerLayout->addWidget(header);

    headerLayout->addStretch();

    QPushButton* addButton = new QPushButton("+", this);
    addButton->setObjectName("add-button");
    addButton->setFixedSize(20, 20);
    addButton->setToolTip("Add dependency");
    connect(
        addButton, &QPushButton::clicked,
        this, &DependenciesWidget::addDependencyViaDialog
    );
    headerLayout->addWidget(addButton);

    _dependenciesList = new QListWidget(this);
    _dependenciesList->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(
        _dependenciesList, &QListWidget::customContextMenuRequested,
        this, [this](const QPoint& pos) { showContextMenu(pos); }
    );
    layout->addWidget(_dependenciesList);
}

void DependenciesWidget::refresh() {
    if (!_dependenciesList) {
        return;
    }

    _dependenciesList->clear();

    if (_asset.dependencies.empty()) {
        QListWidgetItem* placeholder = new QListWidgetItem(
            "No dependencies",
            _dependenciesList
        );
        // Non-interactive; styled via QListWidget::item:disabled
        placeholder->setFlags(Qt::NoItemFlags);
        return;
    }

    const std::filesystem::path root = absPath("${USER_ASSETS}");
    const std::filesystem::path assetDirectory = _filePath.parent_path();

    for (const std::string& dep : _asset.dependencies) {
        const std::filesystem::path depPath = resolvePath(dep, root, assetDirectory);
        const bool exists = std::filesystem::exists(depPath);

        const QString badge = pathTypeBadge(detectPathType(dep));
        const QString icon = exists ? IconExists : IconMissing;
        const QString display = badge + " " + icon + "  " + QString::fromStdString(dep);

        QListWidgetItem* item = new QListWidgetItem(display, _dependenciesList);
        item->setToolTip(QString::fromStdWString(depPath.wstring()));
    }
}

void DependenciesWidget::addDependencyViaDialog() {
    const std::filesystem::path assetDirectory = _filePath.parent_path();
    const QString startDir = assetDirectory.empty() ?
        QDir::homePath() :
        QString::fromStdWString(assetDirectory.wstring());

    const QString selected = QFileDialog::getOpenFileName(
        this,
        "Add Dependency",
        startDir,
        FileFilter
    );
    if (selected.isEmpty()) {
        return;
    }

    addDependency(selected);
}

void DependenciesWidget::addDependency(const QString& filePath) {
    const std::filesystem::path selectedPath = filePath.toStdWString();
    const std::filesystem::path assetDirectory = _filePath.parent_path();

    // Store as data-relative if inside the data root, relative if the asset file is
    // saved, or absolute as a last resort
    std::string dependencyString;
    const std::filesystem::path root = absPath("${USER_ASSETS}");
    if (!root.empty()) {
        std::error_code error;
        const std::filesystem::path relative =
            std::filesystem::relative(selectedPath, root, error);
        // Only accept data-relative path if the dependency is actually IN the data dir
        if (!error && !relative.empty() && !relative.string().starts_with("..")) {
            dependencyString = relative.string();
        }
    }
    // Fall back to relative path if the current jasset file is saved
    if (dependencyString.empty() && !assetDirectory.empty()) {
        std::error_code error;
        const std::filesystem::path relative =
            std::filesystem::relative(selectedPath, assetDirectory, error);
        if (!error && !relative.empty()) {
            dependencyString = relative.string();
            // Ensure ./ prefix so the path is detected as relative
            if (!dependencyString.starts_with(".")) {
                dependencyString = "./" + dependencyString;
            }
        }
    }
    // Last resort; absolute path
    if (dependencyString.empty()) {
        dependencyString = selectedPath.string();
    }
    std::replace(dependencyString.begin(), dependencyString.end(), '\\', '/');

    // Avoid duplicates
    for (const std::string& existing : _asset.dependencies) {
        if (existing == dependencyString) {
            QMessageBox::information(
                this,
                "Duplicate Dependency",
                QString::fromStdString(
                    "This dependency is already added:\n" + dependencyString
                )
            );
            return;
        }
    }

    _asset.dependencies.push_back(dependencyString);
    emit assetModified();
}

void DependenciesWidget::removeDependency(size_t row) {
    if (row >= _asset.dependencies.size()) {
        return;
    }

    const QString dependency = QString::fromStdString(_asset.dependencies[row]);
    const QMessageBox::StandardButton answer = QMessageBox::question(
        this,
        "Remove Dependency",
        "Remove \"" + dependency + "\"?",
        QMessageBox::Yes | QMessageBox::No,
        QMessageBox::No
    );

    if (answer == QMessageBox::Yes) {
        _asset.dependencies.erase(_asset.dependencies.begin() + row);
        emit assetModified();
    }
}

void DependenciesWidget::convertDependencyPath(size_t row, PathType target) {
    if (row >= _asset.dependencies.size()) {
        return;
    }

    const std::string current = _asset.dependencies[row];
    const PathType currentType = detectPathType(current);

    if (currentType == target) {
        return;
    }

    const std::filesystem::path assetDirectory = _filePath.parent_path();

    // Any conversion involving a relative path requires a saved file
    const bool needsAssetDir =
        currentType == PathType::Relative || target == PathType::Relative;

    if (needsAssetDir && assetDirectory.empty()) {
        QMessageBox::warning(
            this,
            "Cannot Convert",
            "Save the asset first to enable path conversion."
        );
        return;
    }

    // Step 1: Resolve current path to absolute
    std::filesystem::path root = absPath("${USER_ASSETS}");
    const std::filesystem::path absolute = resolvePath(current, root, assetDirectory);

    // Step 2: Convert from absolute to target
    std::string converted;
    if (target == PathType::Absolute) {
        converted = absolute.string();
    }
    else if (target == PathType::Relative) {
        std::error_code error;
        std::filesystem::path rel =
            std::filesystem::relative(absolute, assetDirectory, error);
        if (error || rel.empty()) {
            QMessageBox::warning(
                this,
                "Cannot Convert",
                "Could not compute a relative path."
            );
            return;
        }
        converted = rel.string();

        // Add ./ prefix so the path is detected as relative, not data-relative.
        // relative can return without ./ or ../ prefix so we need to make sure
        if (!converted.starts_with(".")) {
            converted = "./" + converted;
        }
    }
    else {
        // Data-relative: make path relative to data root
        std::error_code error;
        std::filesystem::path rel = std::filesystem::relative(absolute, root, error);

        if (error || rel.empty() || rel.string().starts_with("..")) {
            QMessageBox::warning(
                this,
                "Cannot Convert",
                "The dependency does not reside inside the data directory:\n" +
                QString::fromStdWString(root.wstring())
            );
            return;
        }
        converted = rel.string();
    }

    std::replace(converted.begin(), converted.end(), '\\', '/');
    _asset.dependencies[row] = converted;
    emit assetModified();
}

void DependenciesWidget::showContextMenu(const QPoint& pos) {
    QListWidgetItem* clicked = _dependenciesList->itemAt(pos);
    if (!clicked || !(clicked->flags() & Qt::ItemIsSelectable)) {
        return;
    }
    const size_t row = static_cast<size_t>(_dependenciesList->row(clicked));
    if (row >= _asset.dependencies.size()) {
        return;
    }

    QMenu menu(this);
    menu.addAction("Remove", this, [this, row]() { removeDependency(row); });

    menu.addSeparator();


    // Adds a conversion action; disabled + checked if already that type
    auto addConvertAction = [&](const QString& label, PathType target, const PathType cur)
    {
        QAction* action = menu.addAction(
            label,
            this, [this, row, target]() { convertDependencyPath(row, target); }
        );
        if (cur == target) {
            action->setEnabled(false);
            action->setCheckable(true);
            action->setChecked(true);
        }
    };
    const PathType current = detectPathType(_asset.dependencies[row]);
    addConvertAction("Convert to relative", PathType::Relative, current);
    addConvertAction("Convert to data-relative", PathType::Data, current);
    addConvertAction("Convert to absolute", PathType::Absolute, current);

    menu.exec(_dependenciesList->mapToGlobal(pos));
}
