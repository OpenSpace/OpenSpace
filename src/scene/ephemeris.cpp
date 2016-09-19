/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/scene/ephemeris.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/documentation/verifier.h>

namespace {
    const std::string _loggerCat = "Ephemeris";
    const std::string KeyType = "Type";
}

namespace openspace {

Documentation Ephemeris::Documentation() {
    using namespace openspace::documentation;

    return{
        "Transformation Translation",
        "core_transform_translation",
        {
            {
                KeyType,
                new StringAnnotationVerifier("Must name a valid Translation type"),
                "The type of translation that is described in this element. "
                "The available types of translations depend on the "
                "configuration of the application and can be written to disk "
                "on application startup into the FactoryDocumentation.",
                Optional::No
            }
        }
    };
}

Ephemeris* Ephemeris::createFromDictionary(const ghoul::Dictionary& dictionary) {
    if (!dictionary.hasValue<std::string>(KeyType)) {
        LERROR("Ephemeris did not have key '" << KeyType << "'");
        return nullptr;
    }

    std::string ephemerisType;
    dictionary.getValue(KeyType, ephemerisType);
    ghoul::TemplateFactory<Ephemeris>* factory
          = FactoryManager::ref().factory<Ephemeris>();
    Ephemeris* result = factory->create(ephemerisType, dictionary);
    if (result == nullptr) {
        LERROR("Failed creating Ephemeris object of type '" << ephemerisType << "'");
        return nullptr;
    }

    return result;
}

Ephemeris::Ephemeris() {}
    
Ephemeris::Ephemeris(const ghoul::Dictionary& dictionary) {}
    
Ephemeris::~Ephemeris() {}
    
bool Ephemeris::initialize() {
    return true;
}
    
void Ephemeris::update(const UpdateData& data) {}

} // namespace openspace
