/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/sonification/include/modesonification.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>

namespace {
    constexpr std::string_view _loggerCat = "ModeSonification";

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        ModeSonificationInfo =
    {
       "ModeSonification",
       "Surround Mode Sonification",
       "Sonification that alters all other sonificatoins based on the current surround "
       "mode"
    };

} // namespace

namespace openspace {

ModeSonification::ModeSonification(const std::string& ip, int port)
    : SonificationBase(ModeSonificationInfo, ip, port)
{
    _mode = SonificationModule::SurroundMode::Horizontal;
}

void ModeSonification::update(const Camera*) {
    if (!_enabled) {
        return;
    }

    SonificationModule* module = global::moduleEngine->module<SonificationModule>();
    if (!module) {
        LERROR("Could not find the SonificationModule");
        return;
    }

    SonificationModule::SurroundMode mode = module->surroundMode();

    if (_mode != mode) {
        _mode = mode;

        std::string label = "/Mode";
        std::vector<OscDataType> data(1);
        data[0] = static_cast<int>(_mode);

        _connection->send(label, data);
    }
}

void ModeSonification::stop() {}

} // namespace openspace
