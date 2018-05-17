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

#include <openspace/query/query.h>
#include <openspace/properties/property.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <ghoul/logging/logmanager.h>
#include <modules/server/include/triggerpropertytopic.h>

namespace {
const std::string PropertyKey = "property";
const std::string ValueKey = "value";
const std::string _loggerCat = "TriggerPropertyTopic";
}

namespace openspace {

void TriggerPropertyTopic::handleJson(nlohmann::json json) {
    try {
        auto propertyKey = json.at(PropertyKey).get<std::string>();

        auto prop = property(propertyKey);
        if (prop != nullptr) {
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

}
