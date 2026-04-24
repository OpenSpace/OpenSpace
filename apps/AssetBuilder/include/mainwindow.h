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

#ifndef __OPENSPACE_ASSETBUILDER___MAINWINDOW___H__
#define __OPENSPACE_ASSETBUILDER___MAINWINDOW___H__

#include <QMainWindow>

#include <filesystem>

class AssetEditorWidget;
class QLabel;

/**
 * Thin window shell for the AssetBuilder standalone application. Hosts the
 * AssetEditorWidget, owns the menu bar and window-level actions.
 *
 * All per-asset state lives inside AssetEditorWidget so that future multi-tab
 * support only requires wrapping multiple editors in a QTabWidget here.
 */
class MainWindow final : public QMainWindow {
Q_OBJECT
public:
    MainWindow();

    /**
     * Intercepts Escape to close the window.
     *
     * \param evt The key press event
     */
    void keyPressEvent(QKeyEvent* evt) override;

    /**
     * Prompts the user to save unsaved changes before closing.
     *
     * \param evt The close event
     */
    void closeEvent(QCloseEvent* evt) override;

    /**
     * Repositions the path label to stay centered in the menu bar.
     *
     * \param evt The resize event
     */
    void resizeEvent(QResizeEvent* evt) override;

private slots:
    /** Creates a new empty asset, prompting to save if needed. */
    void newAsset();
    /** Opens a .jasset file via file dialog, prompting to save if needed. */
    void openAsset();
    /** Saves the current asset to its file path, or prompts for one. */
    bool saveAsset();
    /** Prompts for a file path and saves the current asset. */
    bool saveAssetAs();
    /** Closes the current asset, prompting to save if needed. */
    void closeAsset();
    /** Shows the About dialog. */
    void showAbout();

    /** Opens a directory picker to set the persistent data root. */
    void setDataRootViaDialog();

    /** Shows the WelcomeDialog and acts on the user's choice. */
    void showWelcomeDialog();

private:
    /** Creates the application menu bar and connects all actions. */
    void createMenus();

    /** Replaces the central widget with the empty-state placeholder. */
    void showEmptyState();

    /** Updates the window title and path label to reflect current state. */
    void updateTitle();

    /** Creates the editor widget and sets it as the central widget. */
    void createEditor();

    /**
     * Prompts the user to save if there are unsaved changes.
     *
     * \return True if it is safe to proceed (no changes, or user confirmed)
     */
    bool maybeSave();

    /** Adds the given path to the QSettings-backed recent files list. */
    void addToRecentFiles(const std::filesystem::path& path);

    /// The editor widget, null when no asset is open
    AssetEditorWidget* _editor = nullptr;

    /// File path label shown in the menu bar
    QLabel* _pathLabel = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___MAINWINDOW___H__
