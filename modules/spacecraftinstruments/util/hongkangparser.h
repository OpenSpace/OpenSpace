/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___HONGKANGPARSER___H__
#define __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___HONGKANGPARSER___H__

#include <modules/spacecraftinstruments/util/sequenceparser.h>

#include <filesystem>

namespace openspace {

class HongKangParser : public SequenceParser {
public:
    HongKangParser(std::string name, std::filesystem::path fileName,
        std::string spacecraft, const ghoul::Dictionary& translationDictionary,
        std::vector<std::string> potentialTargets);

    bool create() override;
    std::string findPlaybookSpecifiedTarget(std::string line);

private:
    std::filesystem::path _defaultCaptureImage;
    double _metRef = 299180517;

    std::string _name;
    std::filesystem::path _fileName;
    std::string _spacecraft;
    std::vector<std::string> _potentialTargets;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___HONGKANGPARSER___H__
