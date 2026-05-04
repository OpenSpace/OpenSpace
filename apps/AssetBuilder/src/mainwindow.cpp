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

#include "mainwindow.h"

#include "asseteditorwidget.h"
#include "path.h"
#include "welcomedialog.h"
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <QCloseEvent>
#include <QFileDialog>
#include <QLabel>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QSettings>
#include <QVBoxLayout>

namespace {
    constexpr std::string_view AppName = "AssetBuilder";
    constexpr std::string_view FileFilter = "Asset files (*.jasset)";
    constexpr int MaxRecentFiles = 10;
} // namespace

MainWindow::MainWindow() {
    setMinimumSize(900, 600);

    // File menu
    QMenu* fileMenu = menuBar()->addMenu("File");

    QAction* newAction = fileMenu->addAction("New");
    newAction->setShortcut(QKeySequence::New);
    connect(newAction, &QAction::triggered, this, &MainWindow::newAsset);

    QAction* openAction = fileMenu->addAction("Open...");
    openAction->setShortcut(QKeySequence::Open);
    connect(openAction, &QAction::triggered, this, &MainWindow::openAsset);

    fileMenu->addSeparator();

    QAction* saveAction = fileMenu->addAction("Save");
    saveAction->setShortcut(QKeySequence::Save);
    connect(saveAction, &QAction::triggered, this, &MainWindow::saveAsset);

    QAction* saveAsAction = fileMenu->addAction("Save As...");
    saveAsAction->setShortcut(QKeySequence::SaveAs);
    connect(saveAsAction, &QAction::triggered, this, &MainWindow::saveAssetAs);

    fileMenu->addSeparator();

    QAction* closeAction = fileMenu->addAction("Close");
    closeAction->setShortcut(QKeySequence::Close);
    connect(closeAction, &QAction::triggered, this, &MainWindow::closeAsset);

    fileMenu->addSeparator();

    QAction* exitAction = fileMenu->addAction("Exit");
    exitAction->setShortcut(QKeySequence::Quit);
    connect(exitAction, &QAction::triggered, this, &QWidget::close);

    QMenu* helpMenu = menuBar()->addMenu("Help");

    QAction* aboutAction = helpMenu->addAction("About AssetBuilder");
    connect(aboutAction, &QAction::triggered, this, &MainWindow::showAbout);

    // Path label centered over the menu bar
    // Should be removed when the TabWidget is added
    _pathLabel = new QLabel("Untitled", menuBar());
    _pathLabel->setObjectName("file-path-bar");
    _pathLabel->setAlignment(Qt::AlignCenter);
    _pathLabel->setAttribute(Qt::WA_TransparentForMouseEvents);

    showEmptyState();
    updateTitle();

    // Show the welcome dialog immediately so the user can create or open an asset
    QMetaObject::invokeMethod(
        this,
        &MainWindow::showWelcomeDialog,
        Qt::QueuedConnection // needs to be queued so the main window gets created first
    );
}

void MainWindow::showEmptyState() {
    QWidget* placeholder = new QWidget(this);
    setCentralWidget(placeholder);
    QBoxLayout* layout = new QVBoxLayout(placeholder);
    layout->setAlignment(Qt::AlignCenter);

    QLabel* label = new QLabel("No asset open", placeholder);
    label->setObjectName("empty-state");
    label->setAlignment(Qt::AlignCenter);
    layout->addWidget(label);

    QLabel* sub = new QLabel("Use File > New or File > Open to get started", placeholder);
    sub->setObjectName("empty-state-sub");
    sub->setAlignment(Qt::AlignCenter);
    layout->addWidget(sub);
}

void MainWindow::showWelcomeDialog() {
    WelcomeDialog dialog = WelcomeDialog(this);
    const int dialogResult = dialog.exec();

    if (dialogResult != QDialog::Accepted) {
        return;
    }

    createEditor();

    switch (dialog.selectedAction()) {
        case WelcomeDialog::Action::CreateEmpty:
            _editor->newAsset();
            break;
        case WelcomeDialog::Action::OpenFile: {
            const std::filesystem::path file = dialog.selectedFile();
            if (_editor->loadAsset(file)) {
                addToRecentFiles(file);
            }
            break;
        }
        case WelcomeDialog::Action::None:
            break;
    }

    updateTitle();
}

void MainWindow::createEditor() {
    if (_editor) {
        return;
    }
    _editor = new AssetEditorWidget(this);
    connect(_editor, &AssetEditorWidget::assetModified, this, &MainWindow::updateTitle);
    setCentralWidget(_editor);
}

void MainWindow::updateTitle() {
    QString title = QString::fromStdString(std::string(AppName));
    if (_editor) {
        // Add an emdash and then the file name (with asterisk if dirty)
        title += QString::fromStdString(" \u2014 ") + _editor->displayName();
    }
    setWindowTitle(title);

    _pathLabel->setText(_editor ? _editor->displayPath() : "");
}

bool MainWindow::maybeSave() {
    if (!_editor || !_editor->isDirty()) {
        return true;
    }

    const QMessageBox::StandardButton result = QMessageBox::question(
        this,
        "Unsaved Changes",
        "The asset has unsaved changes. Do you want to save them?",
        QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel
    );

    if (result == QMessageBox::Save) {
        return saveAsset();
    }
    if (result == QMessageBox::Cancel) {
        return false;
    }
    return true;
}

void MainWindow::newAsset() {
    if (!maybeSave()) {
        return;
    }
    showWelcomeDialog();
}

void MainWindow::openAsset() {
    if (!maybeSave()) {
        return;
    }

    const QString path = QFileDialog::getOpenFileName(
        this,
        "Open Asset",
        QString::fromStdString(absPath("${USER_ASSETS}").string()),
        QString::fromStdString(std::string(FileFilter))
    );

    if (path.isEmpty()) {
        return;
    }

    createEditor();
    const bool success = _editor->loadAsset(std::filesystem::path(path.toStdWString()));
    if (!success) {
        return;
    }
    addToRecentFiles(_editor->filePath());
    updateTitle();
}

bool MainWindow::saveAsset() {
    if (!_editor || _editor->filePath().empty()) {
        return saveAssetAs();
    }
    const bool success = _editor->saveAsset(_editor->filePath());
    if (!success) {
        QMessageBox::critical(
            this,
            "Save Failed",
            "Could not save to:\n" +
                QString::fromStdWString(_editor->filePath().wstring()) +
                "\n\nCheck that the file is not read-only and the directory exists."
        );
        return false;
    }
    updateTitle();
    return true;
}

bool MainWindow::saveAssetAs() {
    if (!_editor) {
        return false;
    }

    const QString suggestion = (_editor && !_editor->filePath().empty()) ?
        QString::fromStdWString(_editor->filePath().wstring()) :
        QString();

    const QString path = QFileDialog::getSaveFileName(
        this,
        "Save Asset As",
        suggestion,
        QString::fromStdString(std::string(FileFilter))
    );
    if (path.isEmpty()) {
        return false;
    }

    const bool success = _editor->saveAsset(std::filesystem::path(path.toStdWString()));
    if (!success) {
        QMessageBox::critical(
            this,
            "Save Failed",
            "Could not save to:\n" + path +
                "\n\nCheck that the file is not read-only and the directory exists."
        );
        return false;
    }
    addToRecentFiles(_editor->filePath());
    updateTitle();
    return true;
}

void MainWindow::closeAsset() {
    if (!maybeSave()) {
        return;
    }
    _editor = nullptr;
    showEmptyState();
    updateTitle();
}

void MainWindow::showAbout() {
    QMessageBox::about(
        this,
        "About AssetBuilder",
        "AssetBuilder\n\nA visual editor for OpenSpace .jasset files.\n\nPart of the "
        "OpenSpace project -- openspaceproject.com"
    );
}

void MainWindow::addToRecentFiles(const std::filesystem::path& path) {
    QSettings settings;
    QStringList recents = settings.value("recentFiles").toStringList();

    // Remove duplicates and push to front of list
    const QString entry = QString::fromStdWString(path.wstring());
    recents.removeAll(entry);
    recents.prepend(entry);

    // Ensure max n recent files
    if (recents.size() > MaxRecentFiles) {
        recents.removeLast();
    }
    settings.setValue("recentFiles", recents);
}

void MainWindow::keyPressEvent(QKeyEvent* evt) {
    if (evt->key() == Qt::Key_Escape) {
        close();
        return;
    }
    QMainWindow::keyPressEvent(evt);
}

void MainWindow::resizeEvent(QResizeEvent* evt) {
    _pathLabel->setGeometry(0, 0, menuBar()->width(), menuBar()->height());

    QMainWindow::resizeEvent(evt);
}

void MainWindow::closeEvent(QCloseEvent* evt) {
    if (maybeSave()) {
        evt->accept();
    }
    else {
        evt->ignore();
    }
}
