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

#ifndef __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___LABELPARSER___H__
#define __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___LABELPARSER___H__

#include <modules/spacecraftinstruments/util/sequenceparser.h>

namespace openspace {

class LabelParser : public SequenceParser {
public:
    LabelParser(std::string name, std::string fileName,
        const ghoul::Dictionary& translationDictionary);

    bool create() override;

    // temporary need to figure this out
    //std::map<std::string, Decoder*> translations() { return _fileTranslation; };

private:
    void createImage(Image& image, double startTime, double stopTime,
        std::vector<std::string> instr, std::string target, std::string path);

    std::string encode(const std::string& line) const;
    std::string decode(const std::string& line);

    bool augmentWithSpice(Image& image, std::string spacecraft,
        std::vector<std::string> payload, std::vector<std::string> potentialTargets);

    std::string _name;
    std::string _fileName;
    std::string _spacecraft;
    std::vector<std::string> _specsOfInterest;

    std::string _target;
    std::string _instrumentID;
    std::string _instrumentHostID;
    std::string _detectorType;
    std::string _sequenceID;
    bool _badDecoding = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___LABELPARSER___H__
