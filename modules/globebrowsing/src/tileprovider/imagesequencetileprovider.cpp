/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/tileprovider/imagesequencetileprovider.h>

#include <modules/globebrowsing/src/tileprovider/defaulttileprovider.h>

namespace {
    constexpr const char* KeyFilePath = "FilePath";
    
    constexpr openspace::properties::Property::PropertyInfo IndexInfo = {
        "Index",
        "Index",
        "The index into the list of images that is used to pick the currently displayed "
        "image"
    };

    constexpr openspace::properties::Property::PropertyInfo CurrentImageInfo = {
        "CurrentImage",
        "Current Image",
        "The read-only value of the currently selected image"
    };

    constexpr openspace::properties::Property::PropertyInfo FolderPathInfo = {
        "FolderPath",
        "Folder Path",
        "The path that is used to look for images for this image provider. The path must "
        "point to an existing folder that contains images"
    };
} // namespace

namespace openspace::globebrowsing {

ImageSequenceTileProvider::ImageSequenceTileProvider(const ghoul::Dictionary& dictionary)
    : index(IndexInfo, 0)
    , currentImage(CurrentImageInfo)
    , folderPath(FolderPathInfo)
    , initDict(dictionary)
{
    ZoneScoped

    if (dictionary.hasValue<int>(IndexInfo.identifier)) {
        index = dictionary.value<int>(IndexInfo.identifier);
    }
    index.setMinValue(0);
    index.onChange([this]() { isImageDirty = true; });
    addProperty(index);

    folderPath.setReadOnly(true);
    addProperty(folderPath);

    folderPath = dictionary.value<std::string>(FolderPathInfo.identifier);
    addProperty(folderPath);

    reset();
}

Tile ImageSequenceTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    if (currentTileProvider) {
        return currentTileProvider->tile(tileIndex);
    }
    else {
        return Tile();
    }
}

Tile::Status ImageSequenceTileProvider::tileStatus(const TileIndex& index) {
    if (currentTileProvider) {
        return currentTileProvider->tileStatus(index);
    }
    else {
        return Tile::Status::Unavailable;
    }
}

TileDepthTransform ImageSequenceTileProvider::depthTransform() {
    if (currentTileProvider) {
        return currentTileProvider->depthTransform();
    }
    else {
        return { 1.f, 0.f };
    }
}

void ImageSequenceTileProvider::update() {
    if (isImageDirty && !imagePaths.empty() &&
        index >= 0 && index < imagePaths.size())
    {
        if (currentTileProvider) {
            currentTileProvider->deinitialize();
        }

        std::string p = imagePaths[index].string();
        currentImage = p;
        initDict.setValue(KeyFilePath, p);
        currentTileProvider = std::make_unique<DefaultTileProvider>(initDict);
        currentTileProvider->initialize();
        isImageDirty = false;
    }

    if (currentTileProvider) {
        currentTileProvider->update();
    }
}

void ImageSequenceTileProvider::reset() {
    namespace fs = std::filesystem;
    std::string path = folderPath;
    imagePaths.clear();
    for (const fs::directory_entry& p : fs::directory_iterator(path)) {
        if (p.is_regular_file()) {
            imagePaths.push_back(p.path());
        }
    }

    index = 0;
    index.setMaxValue(static_cast<int>(imagePaths.size() - 1));

    if (currentTileProvider) {
        currentTileProvider->reset();
    }
}

int ImageSequenceTileProvider::maxLevel() {
    return currentTileProvider ? currentTileProvider->maxLevel() : 0;
}

float ImageSequenceTileProvider::noDataValueAsFloat() {
    if (currentTileProvider) {
        return currentTileProvider->noDataValueAsFloat();
    }
    else {
        return std::numeric_limits<float>::min();
    }
}

} // namespace openspace::globebrowsing
