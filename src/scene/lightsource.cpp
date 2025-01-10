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

#include <openspace/scene/lightsource.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/factorymanager.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/templatefactory.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "Whether the light source is enabled or not.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(LightSource)]] Parameters {
        // The type of the light source that is described in this element. The available
        // types of light sources depend on the configuration of the application and can
        // be written to disk on application startup into the FactoryDocumentation
        std::string type [[codegen::annotation("Must name a valid LightSource type")]];

        // The identifier of the light source
        std::string identifier [[codegen::identifier()]];

        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;
    };
#include "lightsource_codegen.cpp"
} // namespace

namespace openspace {

bool LightSource::isEnabled() const {
    return _enabled;
}

documentation::Documentation LightSource::Documentation() {
    return codegen::doc<Parameters>("core_light_source");
}

std::unique_ptr<LightSource> LightSource::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    ghoul::TemplateFactory<LightSource>* factory =
        FactoryManager::ref().factory<LightSource>();
    LightSource* source = factory->create(p.type, dictionary);
    source->setIdentifier(p.identifier);

    source->_type = p.type;
    return std::unique_ptr<LightSource>(source);
}

LightSource::LightSource()
    : properties::PropertyOwner({ "LightSource", "Light Source" })
    , _enabled(EnabledInfo, true)
{
    addProperty(_enabled);
}

LightSource::LightSource(const ghoul::Dictionary& dictionary)
    : LightSource()
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _enabled = p.enabled.value_or(_enabled);
}

bool LightSource::initialize() {
    return true;
}

} // namespace openspace
