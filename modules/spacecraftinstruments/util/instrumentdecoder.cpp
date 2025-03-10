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

#include <modules/spacecraftinstruments/util/instrumentdecoder.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat  = "InstrumentDecoder";

    struct [[codegen::Dictionary(InstrumentDecoder)]] Parameters {
        std::string detectorType;
        std::optional<std::string> stopCommand;
        std::vector<std::string> spice;
    };
#include "instrumentdecoder_codegen.cpp"
} // namespace

namespace openspace {

InstrumentDecoder::InstrumentDecoder(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _type = ghoul::toUpperCase(p.detectorType);

    if (p.stopCommand.has_value() && _type == "SCANNER") {
        _stopCommand = *p.stopCommand;
    }
    else {
        LDEBUG("Scanner must provide stop command, please check asset file");
    }

    _spiceIDs = p.spice;
}

const std::string& InstrumentDecoder::stopCommand() {
    return _stopCommand;
}

std::string_view InstrumentDecoder::decoderType() const {
    return _type;
}

const std::vector<std::string>& InstrumentDecoder::translations() const {
    return _spiceIDs;
}

} // namespace openspace
