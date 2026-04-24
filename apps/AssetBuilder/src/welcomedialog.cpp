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

#include "welcomedialog.h"

#include <QFileDialog>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSettings>

namespace {
    constexpr std::string_view FileFilter = "Asset files (*.jasset)";
    constexpr int DialogWidth  = 480;
    constexpr int DialogHeight = 400;
} // namespace

WelcomeDialog::WelcomeDialog(QWidget* parent)
    : QDialog(parent)
{
    setWindowTitle("AssetBuilder");
    setFixedSize(DialogWidth, DialogHeight);
    buildUi();

    // Center on parent
    if (parent) {
        const QRect pg = parent->geometry();
        move(
            pg.x() + (pg.width() - DialogWidth) / 2,
            pg.y() + (pg.height() - DialogHeight) / 2
        );
    }
}

WelcomeDialog::Action WelcomeDialog::selectedAction() const {
    return _selectedAction;
}

std::filesystem::path WelcomeDialog::selectedFile() const {
    return _selectedFile;
}

void WelcomeDialog::createEmpty() {
    _selectedAction = Action::CreateEmpty;
    accept();
}

void WelcomeDialog::browseForFile() {
    const QString path = QFileDialog::getOpenFileName(
        this,
        "Open Asset",
        QString(),
        QString::fromStdString(std::string(FileFilter))
    );

    if (path.isEmpty()) {
        return;
    }

    _selectedAction = Action::OpenFile;
    _selectedFile = std::filesystem::path(path.toStdString());
    accept();
}

void WelcomeDialog::openRecentFile(const std::filesystem::path& path) {
    _selectedAction = Action::OpenFile;
    _selectedFile = path;
    accept();
}

void WelcomeDialog::buildUi() {
    QBoxLayout* root = new QVBoxLayout(this);
    root->setContentsMargins(32, 32, 32, 32);
    root->setSpacing(0);

    // Title
    QLabel* title = new QLabel("AssetBuilder", this);
    title->setObjectName("welcome-title");
    title->setAlignment(Qt::AlignCenter);
    root->addWidget(title);

    // Subtitle
    QLabel* sub = new QLabel("Visual editor for OpenSpace .jasset files", this);
    sub->setObjectName("welcome-subtitle");
    sub->setAlignment(Qt::AlignCenter);
    root->addWidget(sub);

    root->addSpacing(32);

    // Action buttons
    QBoxLayout* buttons = new QHBoxLayout();
    buttons->setSpacing(16);

    QPushButton* newButton = new QPushButton("New Asset", this);
    newButton->setObjectName("primary");
    newButton->setFixedHeight(40);
    newButton->setToolTip("Create a new blank Scene Graph Node asset");
    connect(newButton, &QPushButton::clicked, this, &WelcomeDialog::createEmpty);

    QPushButton* openButton = new QPushButton("Open Asset\u2026", this);
    openButton->setObjectName("secondary");
    openButton->setFixedHeight(40);
    openButton->setToolTip("Open an existing .jasset file");
    connect(openButton, &QPushButton::clicked, this, &WelcomeDialog::browseForFile);

    buttons->addWidget(newButton);
    buttons->addWidget(openButton);
    root->addLayout(buttons);

    // Recent files
    QSettings settings;
    const QStringList recents = settings.value("recentFiles").toStringList();

    // Filter to files that still exist
    std::vector<std::filesystem::path> validRecents;
    for (const QString& r : recents) {
        std::filesystem::path p(r.toStdWString());
        if (std::filesystem::exists(p)) {
            validRecents.push_back(p);
        }
    }

    // Create the recent file buttons
    // They have the structure: <filename> <folder filepath>
    if (!validRecents.empty()) {
        root->addSpacing(20);

        QLabel* recentLabel = new QLabel("Recent", this);
        recentLabel->setObjectName("section-header");
        root->addWidget(recentLabel);

        for (const std::filesystem::path& path : validRecents) {
            const QString filename = QString::fromStdWString(path.filename().wstring());
            const QString dir = QString::fromStdWString(path.parent_path().wstring());

            QPushButton* button = new QPushButton(this);
            button->setFlat(true);
            button->setText(filename + "  \u2014  " + dir);
            button->setToolTip(QString::fromStdWString(path.wstring()));
            connect(
                button, &QPushButton::clicked,
                this, [this, path]() { openRecentFile(path); }
            );
            root->addWidget(button);
        }
    }

    root->addStretch();
}
