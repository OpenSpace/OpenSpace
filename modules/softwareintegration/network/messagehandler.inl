/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include <openspace/rendering/renderable.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/scalar/intproperty.h>

#include <ghoul/logging/logmanager.h>

namespace openspace::softwareintegration::messagehandler {

namespace {

template<typename T>
bool handleEnumValue(
    const std::vector<std::byte>& message,
    size_t& offset,
    const std::string& identifier,
    const simp::DataKey& dataKey,
    const std::string& propertyName
) {
    int32_t newValue;
    try {
        simp::readValue(message, offset, newValue);
    }
    catch (const simp::SimpError& err) {
        LERRORC("MessageHandler", std::format(
            "Error when parsing int32_t in DATA.{} message: {}",
            simp::getStringFromDataKey(dataKey), err.message
        ));
        return false;
    }

    try {
        static_cast<T>(newValue);
    }
    catch (const std::exception& err) {
        LERRORC("MessageHandler", std::format(
            "Error when casting {} to {} in DATA.{} message: {}",
            newValue, typeid(T).name(), simp::getStringFromDataKey(dataKey), err.what()
        ));
        return false;
    }

    auto setEnumCallback = [identifier, propertyName, newValue] {
        // Get renderable
        auto r = renderable(identifier);
        if (!r) return;

        // Get property
        auto property = r->property(propertyName);
        if (!property) return;

        // Update int property of renderable
        properties::FloatProperty* prop = static_cast<properties::FloatProperty*>(property);

        int currentValue = int(prop->value());
        if (newValue != currentValue) {
            global::scriptEngine->queueScript(
                std::format(
                    "openspace.setPropertyValueSingle('Scene.{}.Renderable.{}', {});",
                    identifier, propertyName, ghoul::to_string(newValue)
                )
            );
        }
    };
    addCallback(
        identifier,
        {
            setEnumCallback,
            {},
            std::format("Callback on property {}", propertyName), 
        }
    );

    return true;
}

} // namespace


} // openspace::softwareintegration::messagehandler
