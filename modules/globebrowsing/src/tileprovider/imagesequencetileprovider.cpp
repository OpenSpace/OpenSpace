/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <openspace/documentation/documentation.h>
#include <optional>

namespace {
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

    struct [[codegen::Dictionary(ImageSequenceTileProvider)]] Parameters {
        // [[codegen::verbatim(IndexInfo.description)]]
        std::optional<int> index;

        // [[codegen::verbatim(FolderPathInfo.description)]]
        std::filesystem::path folderPath [[codegen::directory()]];
    };
#include "imagesequencetileprovider_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation ImageSequenceTileProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_imagesequencetileprovider");
}

ImageSequenceTileProvider::ImageSequenceTileProvider(const ghoul::Dictionary& dictionary)
    : _index(IndexInfo, 0)
    , _currentImage(CurrentImageInfo)
    , _folderPath(FolderPathInfo)
    , _initDict(dictionary)
{
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _index = p.index.value_or(_index);
    _index.setMinValue(0);
    _index.onChange([this]() { _isImageDirty = true; });
    addProperty(_index);

    _currentImage.setReadOnly(true);
    addProperty(_currentImage);

    _folderPath = p.folderPath.string();
    addProperty(_folderPath);

    reset();
}

Tile ImageSequenceTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped

    return _currentTileProvider ? _currentTileProvider->tile(tileIndex) : Tile();
}

Tile::Status ImageSequenceTileProvider::tileStatus(const TileIndex& index) {
    return _currentTileProvider ?
        _currentTileProvider->tileStatus(index) :
        Tile::Status::Unavailable;
}

TileDepthTransform ImageSequenceTileProvider::depthTransform() {
    if (_currentTileProvider) {
        return _currentTileProvider->depthTransform();
    }
    else {
        return { 1.f, 0.f };
    }
}

void ImageSequenceTileProvider::update() {
    if (_isImageDirty && !_imagePaths.empty() &&
        _index >= 0 && _index < static_cast<int>(_imagePaths.size()))
    {
        if (_currentTileProvider) {
            _currentTileProvider->deinitialize();
        }

        std::string p = _imagePaths[_index].string();
        _currentImage = p;
        _initDict.setValue("FilePath", p);
        _currentTileProvider = std::make_unique<DefaultTileProvider>(_initDict);
        _currentTileProvider->initialize();
        _isImageDirty = false;
    }

    if (_currentTileProvider) {
        _currentTileProvider->update();
    }
}

void ImageSequenceTileProvider::reset() {
    namespace fs = std::filesystem;
    std::string path = _folderPath;
    _imagePaths.clear();
    for (const fs::directory_entry& p : fs::directory_iterator(path)) {
        if (p.is_regular_file()) {
            _imagePaths.push_back(p.path());
        }
    }

    _index = 0;
    _index.setMaxValue(static_cast<int>(_imagePaths.size() - 1));

    if (_currentTileProvider) {
        _currentTileProvider->reset();
    }
}

int ImageSequenceTileProvider::maxLevel() {
    return _currentTileProvider ? _currentTileProvider->maxLevel() : 0;
}

float ImageSequenceTileProvider::noDataValueAsFloat() {
    return _currentTileProvider ?
        _currentTileProvider->noDataValueAsFloat() :
        std::numeric_limits<float>::min();
}

} // namespace openspace::globebrowsing
