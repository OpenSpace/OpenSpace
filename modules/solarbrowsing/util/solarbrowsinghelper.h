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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___SOLARBROWSINGHELPER___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___SOLARBROWSINGHELPER___H__

#include <modules/solarbrowsing/util/structs.h>

namespace openspace {
    class TransferFunction;

namespace solarbrowsing {

void loadTransferFunctions(const std::filesystem::path& dir,
    std::unordered_map<InstrumentName, std::shared_ptr<TransferFunction>>& _tfMap);

/**
 * Loads image metadata from all image sequence directories contained in
 * \p rootDir.
 *
 * Reuse previously generated cache files where possible. Any directory without a valid
 * cache is processed directly and its metadata is reconstructed from the contained image
 * files. Newly generated metadata is written back to disk for future runs.
 *
 * The resulting metadata is grouped by instrument and inserted into
 * \p imageMetadataMap.
 *
 * \param rootDir The root directory containing image sequence subdirectories
 *
 * \return The metadata map that will be populated with all discovered image metadata
 */
ImageMetadataMap loadImageMetadata(const std::filesystem::path& rootDir);


DecodedImageData loadDecodedDataFromCache(const std::filesystem::path& path,
    const ImageMetadata* metadata, unsigned int imageSize
);

void saveDecodedDataToCache(const std::filesystem::path& path,
    const DecodedImageData& data, bool verboseMode
);


} //namespace openspace

} // namespace openspace::solarbrowsing

#endif // !__OPENSPACE_MODULE_SOLARBROWSING___SOLARBROWSINGHELPER___H__
