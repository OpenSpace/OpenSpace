/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <modules/newhorizons/util/decoder.h>
#include <fstream>
#include <iterator>
#include <iomanip>
#include <limits>

#include <modules/newhorizons/util/instrumenttimesparser.h>

namespace {
    const std::string _loggerCat = "InstrumentTimesParser";

    const std::string PlaybookIdentifierName = "InstrumentTimesParser";
}

namespace openspace {

InstrumentTimesParser::InstrumentTimesParser(
    const std::string& name,
    const std::string& sequenceSource,
    ghoul::Dictionary& parserDict)
    : _name(name)
    , _fileName(sequenceSource) 
    , _pattern("\".+\" \".+\"")
{

    
}


bool InstrumentTimesParser::create() {
    auto imageComparer = [](const Image &a, const Image &b)->bool{
        return a.startTime < b.startTime;
    };
    auto targetComparer = [](const std::pair<double, std::string> &a,
        const std::pair<double, std::string> &b)->bool{
        return a.first < b.first;
    };
    std::string previousTarget;
    std::string lblName = "";

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(_fileName, RawPath::Yes);
    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load Label Directory '" << sequenceDir.path() << "'");
        return false;
    }

    using Recursive = ghoul::filesystem::Directory::Recursive;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::No);
    
    
    sendPlaybookInformation(PlaybookIdentifierName);
    return true;
}


}