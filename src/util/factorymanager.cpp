/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/util/factorymanager.h>

#include <sstream>

namespace openspace {

FactoryManager* FactoryManager::_manager = nullptr;

FactoryManager::FactoryNotFoundError::FactoryNotFoundError(std::string t)
    : ghoul::RuntimeError("Could not find TemplateFactory for type '" + t + "'")
    , type(std::move(t))
{
    ghoul_assert(!type.empty(), "Type must not be empty");
}

FactoryManager::FactoryManager()
    : DocumentationGenerator(
        "Factory Documentation",
        "factory",
        {
            { "factoryTemplate", "${WEB}/documentation/factory.hbs" }
        }
    )
{}

void FactoryManager::initialize() {
    ghoul_assert(!_manager, "Factory Manager must not have been initialized");

    _manager = new FactoryManager;
}

void FactoryManager::deinitialize() {
    ghoul_assert(_manager, "Factory Manager must have been initialized");

    delete _manager;
    _manager = nullptr;
}

bool FactoryManager::isInitialized() {
    return _manager != nullptr;
}

FactoryManager& FactoryManager::ref() {
    ghoul_assert(_manager, "Factory Manager must have been initialized");

    return *_manager;
}

void FactoryManager::addFactory(std::unique_ptr<ghoul::TemplateFactoryBase> f,
                                std::string name)
{
    ghoul_assert(f, "Factory must not be nullptr");

    _factories.push_back({ std::move(f), std::move(name) });
}

std::string FactoryManager::generateJson() const {
    std::stringstream json;

    json << "[";
    for (const FactoryInfo& factoryInfo : _factories) {
        json << "{";
        json << "\"name\": \"" << factoryInfo.name << "\",";
        json << "\"classes\": [";

        ghoul::TemplateFactoryBase* f = factoryInfo.factory.get();
        const std::vector<std::string>& registeredClasses = f->registeredClasses();
        for (const std::string& c : registeredClasses) {
            json << "\"" << c << "\"";
            if (&c != &registeredClasses.back()) {
                json << ",";
            }
        }

        json << "]}";
        if (&factoryInfo != &_factories.back()) {
            json << ",";
        }
    }

    json << "]";

    // I did not check the output of this for correctness ---abock
    return json.str();
}

}  // namespace openspace
