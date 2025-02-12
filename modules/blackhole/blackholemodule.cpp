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

#include <modules/blackhole/blackholemodule.h>

namespace {
    constexpr std::string_view _loggerCat = "BlackHoleModule";

    struct [[codegen::Dictionary(BlackHoleModule)]] Parameters {
        std::optional<int> rs [[codegen::greater(0)]];
    };

#include "blackholemodule_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation BlackHoleModule::Documentation() {
    return codegen::doc<Parameters>("module_black_hole");
}

BlackHoleModule::BlackHoleModule()
    : OpenSpaceModule(Name)
{}

BlackHoleModule::~BlackHoleModule() {}

void BlackHoleModule::internalInitialize(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
}

void BlackHoleModule::internalDeinitializeGL() {}

std::vector<documentation::Documentation> BlackHoleModule::documentations() const {
    return {
    };
}
} // namespace openspace
