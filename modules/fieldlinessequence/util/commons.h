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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___COMMONS___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___COMMONS___H__

#include <string>

namespace openspace::fls { // (F)ield(L)ines(S)equence

struct ExtraVariable {
    enum VariableType {
        numerical,
        text
    };
    VariableType type;
    std::string name;
    std::string unit;
    // probably only need two of these + maybe one should be a property?
    glm::vec2 absolutMinMaxRange;
    //glm::vec2 displayedMinMaxRange;
    //glm::vec2 selectedRange;

    //std::string unit(std::string n) {
    //    return somewhereelse::lookUpUnit(n);
    //}

    //std::string name(std::string u) {
    //    return somewhereelse::lookUpName(u);
    //}
};

enum class Model : int {
    Batsrus = 0,
    Enlil,
    Pfss,
    Invalid
};


Model stringToModel(const std::string& s);

constexpr float AuToMeter = 149597870700.f; // Astronomical Units
constexpr float ReToMeter = 6371000.f; // Earth radius
constexpr float RsToMeter = 695700000.f; // Sun radius

} // namespace openspace::fls

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___COMMONS___H__
