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

#include <modules/spacecraftinstruments/util/sequenceparser.h>

#include <openspace/engine/globals.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/logging/logmanager.h>

namespace openspace {

std::map<std::string, ImageSubset>& SequenceParser::subsetMap() {
    return _subsetMap;
}

const std::vector<std::pair<std::string, TimeRange>>&
SequenceParser::instrumentTimes() const
{
    return _instrumentTimes;
}
const std::vector<std::pair<double, std::string>>& SequenceParser::targetTimes() const {
    return _targetTimes;
}

const std::vector<double>& SequenceParser::captureProgression() const {
    return _captureProgression;
}

std::map<std::string, std::unique_ptr<Decoder>>& SequenceParser::translations() {
    return _fileTranslation;
}

} // namespace openspace
