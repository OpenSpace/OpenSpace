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

#include <openspace/scene/lightsource.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    constexpr const char* KeyType = "Type";

    constexpr const char* KeyIdentifier = "Identifier";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Whether the light source is enabled or not"
    };
} // namespace

namespace openspace {

bool LightSource::isEnabled() const {
    return _enabled;
}

documentation::Documentation LightSource::Documentation() {
    using namespace openspace::documentation;

    return {
        "Light Source",
        "core_light_source",
        {
            {
                KeyType,
                new StringAnnotationVerifier("Must name a valid LightSource type"),
                Optional::No,
                "The type of the light source that is described in this element. "
                "The available types of light sources depend on the configuration "
                "of the application and can be written to disk on "
                "application startup into the FactoryDocumentation."
            },
            {
                KeyIdentifier,
                new StringVerifier,
                Optional::No,
                "The identifier of the light source."
            },
            {
                EnabledInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                EnabledInfo.description
            }
        }
    };
}

std::unique_ptr<LightSource> LightSource::createFromDictionary(
    const ghoul::Dictionary& dictionary)
{
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "LightSource");

    const std::string timeFrameType = dictionary.value<std::string>(KeyType);

    auto factory = FactoryManager::ref().factory<LightSource>();
    std::unique_ptr<LightSource> result = factory->create(timeFrameType, dictionary);

    const std::string identifier = dictionary.value<std::string>(KeyIdentifier);
    result->setIdentifier(identifier);

    return result;
}

LightSource::LightSource()
    : properties::PropertyOwner({ "LightSource" })
    , _enabled(EnabledInfo, true)
{
    addProperty(_enabled);
}

LightSource::LightSource(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "LightSource" })
    , _enabled(EnabledInfo, true)
{
    if (dictionary.hasValue<std::string>(EnabledInfo.identifier)) {
        _enabled = dictionary.value<bool>(EnabledInfo.identifier);
    }

    addProperty(_enabled);
}

bool LightSource::initialize() {
    return true;
}

} // namespace openspace
