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

#ifndef __OPENSPACE_CORE___COLORMAPLOADER___H__
#define __OPENSPACE_CORE___COLORMAPLOADER___H__

#include <openspace/data/dataloader.h>
#include <ghoul/opengl/texture.h>
#include <filesystem>
#include <memory>

namespace openspace::dataloader::colormap {

ColorMap loadCmapFile(std::filesystem::path path);

/**
 * Loads a 1-dimensional color map texture from a file and returns it as a texture object.
 *
 * This function supports two types of colormap formats:
 * - `.cmap` files: Custom color map format parsed by `loadCmapFile`
 * - Standard image formats: Any 1D texture format supported by `ghoul::io::texture::loadTexture`
 *
 * \param filename The path to the color map file to load. Must have a valid file
 *                 extension
 * \param samplerSettings Optional sampler settings to apply to the texture
 *
 * \return A unique pointer to the loaded OpenGL texture
 *
 * \throw ghoul::io::texture::MissingReaderException if the file extension is not
 *        supported
 * \pre The filename must have a file extension
 */
std::unique_ptr<ghoul::opengl::Texture> loadColorMapTexture(
    const std::filesystem::path& filename,
    ghoul::opengl::Texture::SamplerInit samplerSettings = {});

} // namespace openspace::dataloader::colormap

#endif // __OPENSPACE_CORE___COLORMAPLOADER___H__
