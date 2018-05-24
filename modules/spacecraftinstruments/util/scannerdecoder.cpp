/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/spacecraftinstruments/util/scannerdecoder.h>

#include <ghoul/misc/dictionary.h>

namespace openspace {

ScannerDecoder::ScannerDecoder(const ghoul::Dictionary& dictionary) : _type("SCANNER") {
    for (size_t k = 0; k < dictionary.size(); k++) {
        _spiceIDs.push_back(dictionary.value<std::string>(std::to_string(k + 1)));
    }
}
const std::string& ScannerDecoder::decoderType() const {
    return _type;
}

const std::vector<std::string>& ScannerDecoder::spiceIDs() const {
    return _spiceIDs;
}

void ScannerDecoder::setStopCommand(std::string stopCommand) {
    _abort = std::move(stopCommand);
}

} // namespace openspace
