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

#include <openspace/query/query.h>
#include <openspace/properties/property.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <ghoul/logging/logmanager.h>
#include <modules/server/include/setpropertytopic.h>

namespace {
const std::string PropertyKey = "property";
const std::string ValueKey = "value";
const std::string _loggerCat = "SetPropertyTopic";
const std::string SpecialKeyTime = "__time";
}

namespace openspace {

void SetPropertyTopic::handleJson(nlohmann::json json) {
    try {
        auto propertyKey = json.at(PropertyKey).get<std::string>();
        auto value = json.at(ValueKey).get<std::string>();

        if (propertyKey == SpecialKeyTime) {
            setTime(value);
        }
        else {
            auto prop = property(propertyKey);
            if (prop != nullptr) {
                LDEBUG("Setting " + propertyKey + " to " + value + ".");
                if (!prop->setStringValue(value)) {
                    LERROR("Failed!");
                }
            }
        }
    }
    catch (std::out_of_range& e) {
        LERROR("Could not set property -- key or value is missing in payload");
        LERROR(e.what());
    }
    /*catch (ghoul::RuntimeError e) {
        LERROR("Could not set property -- runtime error:");
        LERROR(e.what());
    }*/
}

void SetPropertyTopic::setTime(const std::string& timeValue) {
    OsEng.timeManager().time().setTime(timeValue);
}

}
