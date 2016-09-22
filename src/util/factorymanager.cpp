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

#include <openspace/util/factorymanager.h>

#include <ghoul/misc/assert.h>

#include <fstream>

namespace openspace {

FactoryManager* FactoryManager::_manager = nullptr;
    
FactoryManager::FactoryNotFoundError::FactoryNotFoundError(std::string t)
    : ghoul::RuntimeError("Could not find TemplateFactory for type '" + t + "'")
    , type(std::move(t))
{
    ghoul_assert(!type.empty(), "Type must not be empty");
}

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

void FactoryManager::addFactory(std::unique_ptr<ghoul::TemplateFactoryBase> factory,
                                std::string name
) {
    ghoul_assert(factory, "Factory must not be nullptr");
    _factories.push_back({ std::move(factory), std::move(name) });
}

void FactoryManager::writeDocumentation(const std::string& file, const std::string& type) {
    if (type == "text") {
        std::ofstream f;
        f.exceptions(~std::ofstream::goodbit);
        f.open(file);

        for (const FactoryInfo& factoryInfo : _factories) {
            f << factoryInfo.name << '\n';

            ghoul::TemplateFactoryBase* factory = factoryInfo.factory.get();
            for (const std::string& c : factory->registeredClasses()) {
                f << '\t' << c << '\n';
            }
            f << "\n\n";
        }

    }
    else if (type == "html") {

#ifdef JSON
        std::stringstream json;

        json << "[";
        for (const FactoryInfo& factoryInfo : _factories) {
            json << "{";
            json << "\"name\": \"" << factoryInfo.name << "\",";
            json << "\"classes\": [";

            ghoul::TemplateFactoryBase* factory = factoryInfo.factory.get();
            for (const std::string& c : factory->registeredClasses()) {
                json << "\"" << c << "\",";
            }

            json << "]},";
        }

        json << "]";

        // I did not check the output of this for correctness ---abock
        std::string jsonText = json.str();
#else
        std::ofstream f;
        f.exceptions(~std::ofstream::goodbit);
        f.open(file);

        std::stringstream html;
        html << "<html>\n"
             << "\t<head>\n"
             << "\t\t<title>Factories</title>\n"
             << "\t</head>\n"
             << "<body>\n"
             << "<table cellpadding=3 cellspacing=0 border=1>\n"
             << "\t<caption>Factories</caption>\n\n"
             << "\t<thead>\n"
             << "\t\t<tr>\n"
             << "\t\t\t<th>Type</th>\n"
             << "\t\t\t<th>Object</th>\n"
             << "\t\t</tr>\n"
             << "\t</thead>\n"
             << "\t<tbody>\n";

        for (const FactoryInfo& factoryInfo : _factories) {
            html << "\t\t<tr>\n"
                 << "\t\t\t<td colspan=2>" << factoryInfo.name << "</td>\n";
                
            ghoul::TemplateFactoryBase* factory = factoryInfo.factory.get();
            for (const std::string& c : factory->registeredClasses()) {
                html << "\t\t\t<tr><td></td><td>" << c << "</td></tr>\n";
            }
            html << "\t<tr><td style=\"line-height: 10px;\" colspan=2></td></tr>\n";

        }

        html << "\t</tbody>\n"
             << "</table>\n"
             << "</html>;";

        f << html.str();
#endif

    }
}

}  // namespace openspace
