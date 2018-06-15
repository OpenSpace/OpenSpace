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

#include <modules/spacecraftinstruments/util/instrumentdecoder.h>

#include <ghoul/misc/assert.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

namespace {
    constexpr const char* _loggerCat  = "InstrumentDecoder";
    constexpr const char* KeyDetector = "DetectorType";
    constexpr const char* KeySpice    = "Spice";
    constexpr const char* KeyStopCommand = "StopCommand";
} // namespace

namespace openspace {

InstrumentDecoder::InstrumentDecoder(const ghoul::Dictionary& dictionary) {
    if (dictionary.hasKeyAndValue<std::string>(KeyDetector)) {
        _type = dictionary.value<std::string>(KeyDetector);
        std::for_each(
            _type.begin(),
            _type.end(),
            [](char& in) { in = static_cast<char>(toupper(in)); }
        );
    }
    else {
        ghoul_assert(false, "Instrument has not provided detector type");
        throw ghoul::RuntimeError("Instrument has not provided detector type");
    }

    if (dictionary.hasKeyAndValue<std::string>(KeyStopCommand) && _type == "SCANNER"){
        dictionary.getValue(KeyStopCommand, _stopCommand);
    } else {
        LWARNING("Scanner must provide stop command, please check mod file.");
    }

    if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeySpice)) {
        ghoul::Dictionary spiceDictionary = dictionary.value<ghoul::Dictionary>(KeySpice);

        _spiceIDs.resize(spiceDictionary.size());
        for (size_t i = 0; i < _spiceIDs.size(); ++i) {
            std::string id = spiceDictionary.value<std::string>(std::to_string(i + 1));
            _spiceIDs[i] = std::move(id);
        }
    }
    else {
        ghoul_assert(false, "Instrument did not provide spice ids");
        throw ghoul::RuntimeError("Instrument has not provided detector type");
    }
}

const std::string& InstrumentDecoder::stopCommand() {
    return _stopCommand;
}

const std::string& InstrumentDecoder::decoderType() const {
    return _type;
}

const std::vector<std::string>& InstrumentDecoder::translations() const {
    return _spiceIDs;
}

} // namespace openspace
