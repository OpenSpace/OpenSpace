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

#ifndef __OPENSPACE_MODULE_FITSFILEREADER___WSAFITSHELPER___H__
#define __OPENSPACE_MODULE_FITSFILEREADER___WSAFITSHELPER___H__

#include <ghoul/opengl/texture.h>
#include <filesystem>
#include <memory>
#include <utility>
#include <valarray>

namespace CCfits {
    class FITS;
    class PHDU;
    class ExtHDU;
} // namespace CCfits

namespace openspace {

template <typename T>
struct ImageData {
    std::valarray<T> contents;
    int width;
    int height;
};

/**
 * Load image from a FITS file into a texture.
 *
 * \param path The path to the FITS file
 * \param layerIndex The index of the layer to load from the FITS file
 * \param minMax The minimum and maximum value range in which to cap the data between
          Values outside of range will be overexposed
   \return The texture created from the layer in the file with the set min-max range
 */
std::unique_ptr<ghoul::opengl::Texture> loadTextureFromFits(
    const std::filesystem::path& path, size_t layerIndex,
    const std::pair<float, float>& minMax);

void readFitsHeader(const std::filesystem::path& path);

/**
 * Get the number of data layers in a FITS file.
 *
 * \param path The path to the FITS file
 * \return The number of layers in the FITS file
 */
int nLayers(const std::filesystem::path& path);

template <typename T, typename U>
std::shared_ptr<ImageData<T>> readImageInternal(U& image);

} // namespace openspace

#endif // __OPENSPACE_MODULE_FITSFILEREADER___WSAFITSHELPER___H__
