/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "backgroundimage.h"

#include <ghoul/filesystem/filesystem.h>
#include <QPainter>
#include <QPixmap>
#include <random>

BackgroundImage::BackgroundImage(QRect size, const std::filesystem::path& syncFolder,
                                 QWidget* parent)
    : QLabel(parent)
{
    setGeometry(size);

    // Set a backup image in case no other images have been downloaded yet
    setPixmap(QPixmap(":/images/launcher-background.png"));

    std::filesystem::path imagePath = syncFolder / "http" / "launcher_images";

    if (!std::filesystem::exists(imagePath)) {
        return;
    }

    namespace fs = std::filesystem;

    // First, we iterate through all folders in the launcher_images sync folder and we get
    // the folder with the highest number
    struct {
        fs::directory_entry path;
        int version = -1;
    } latest;
    for (const fs::directory_entry& p : fs::directory_iterator(imagePath)) {
        if (!p.is_directory()) {
            continue;
        }
        const std::string versionStr = p.path().stem().string();
        // All folder names in the sync folder should only be a digit, so we should be
        // find to just convert it here
        const int version = std::stoi(versionStr);

        if (version > latest.version) {
            latest.version = version;
            latest.path = p;
        }
    }

    if (latest.version == -1) {
        // The sync folder existed, but nothing was in there. Kinda weird, but still
        return;
    }

    // Now we know which folder to use, we will pick an image at random
    std::vector<std::filesystem::path> files = ghoul::filesystem::walkDirectory(
        latest.path,
        ghoul::filesystem::Recursive::No,
        ghoul::filesystem::Sorted::No,
        [](const std::filesystem::path& p) {
            return p.extension() == ".png" && p.filename() != "overlay.png";
        }
    );
    std::random_device rd;
    std::mt19937 g(rd());
    std::shuffle(files.begin(), files.end(), g);

    // There better be at least one file left, but just in in case
    if (!files.empty()) {
        // Take the selected image and overpaint the overlay increasing the contrast
        std::string image = files.front().string();
        QPixmap pixmap = QPixmap(QString::fromStdString(image));
        QPainter painter = QPainter(&pixmap);
        painter.setOpacity(0.7);
        QPixmap overlay = QPixmap(QString::fromStdString(
            std::format("{}/overlay.png", latest.path.path())
        ));
        painter.drawPixmap(pixmap.rect(), overlay);
        setPixmap(pixmap);
    }
}
