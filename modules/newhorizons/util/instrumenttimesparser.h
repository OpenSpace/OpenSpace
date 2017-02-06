/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_NEWHORIZONS___INSTRUMENTTIMESPARSER___H__
#define __OPENSPACE_MODULE_NEWHORIZONS___INSTRUMENTTIMESPARSER___H__

#include <modules/newhorizons/util/sequenceparser.h>

#include <regex>

namespace openspace {

class InstrumentTimesParser : public SequenceParser {
public:
    InstrumentTimesParser(
        const std::string& name,
        const std::string& sequenceSource,
        ghoul::Dictionary& parserDict);

    bool create() override;

private:
    std::regex _pattern;

    std::map<std::string, std::vector<std::string>> _instrumentFiles;

    std::string _name;
    std::string _fileName;
    std::string _spacecraft;
    //std::map<std::string, std::unique_ptr<Decoder>> _fileTranslation;
    std::vector<std::string> _specsOfInterest;

    std::string _target;
    std::string _detectorType;
    std::string _sequenceID;
    bool _badDecoding;
};

} // namespace openspace

#endif //__OPENSPACE_MODULE_NEWHORIZONS___INSTRUMENTTIMESPARSER___H__
