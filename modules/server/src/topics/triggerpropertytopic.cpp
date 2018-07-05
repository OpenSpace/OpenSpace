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

#include <modules/server/include/topics/triggerpropertytopic.h>

#include <openspace/json.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* PropertyKey = "property";
    //constexpr const char* ValueKey = "value";
    constexpr const char* _loggerCat = "TriggerPropertyTopic";
} // namespace

namespace openspace {

void TriggerPropertyTopic::handleJson(const nlohmann::json& json) {
    try {
        const std::string& propertyKey = json.at(PropertyKey).get<std::string>();

        properties::Property* prop = property(propertyKey);
        if (prop) {
            LDEBUG("Triggering " + propertyKey);
            prop->set("poke");
        }
        else {
            LWARNING("Could not find property " + propertyKey);
        }
    }
    catch (const std::out_of_range& e) {
        LERROR("Could not poke property -- key or value is missing in payload");
        LERROR(e.what());
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR("Could not poke property -- runtime error:");
        LERROR(e.what());
    }
}

bool TriggerPropertyTopic::isDone() const {
    return true;
}

} // namespace openspace
