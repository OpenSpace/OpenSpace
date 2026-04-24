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

#ifndef __OPENSPACE_ASSETBUILDER___WELCOMEDIALOG___H__
#define __OPENSPACE_ASSETBUILDER___WELCOMEDIALOG___H__

#include <QDialog>

#include <filesystem>

/**
 * Shown on application startup and when File -> New is triggered. Lets the user
 * create a new asset or open an existing one.
 *
 * After exec() returns Accepted, read selectedAction() and selectedFile() to determine
 * what the user chose.
 */
class WelcomeDialog final : public QDialog {
Q_OBJECT
public:
    enum class Action {
        None,
        CreateEmpty,    /// Create a new blank SceneGraphNode asset
        OpenFile        /// Open an existing .jasset file (path in selectedFile())
    };

    explicit WelcomeDialog(QWidget* parent = nullptr);

    /**
     * Returns the action the user chose before the dialog closed.
     *
     * \return The selected Action
     */
    Action selectedAction() const;

    /**
     * Returns the file path chosen when selectedAction() == OpenFile. Empty otherwise.
     *
     * \return The path to the selected .jasset file
     */
    std::filesystem::path selectedFile() const;

private slots:
    /** Accepts the dialog with Action::CreateEmpty. */
    void createEmpty();

    /** Opens a file picker and accepts with Action::OpenFile on success. */
    void browseForFile();

    /**
     * Accepts the dialog with Action::OpenFile using the given path.
     *
     * \param path The recently-used file path to open
     */
    void openRecentFile(const std::filesystem::path& path);

private:
    /** Constructs and lays out all child widgets. */
    void buildUi();

    Action _selectedAction = Action::None;
    std::filesystem::path _selectedFile;
};

#endif // __OPENSPACE_ASSETBUILDER___WELCOMEDIALOG___H__
