/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/util/constants.h>
#include <openspace/util/factorymanager.h>

namespace {
const std::string _loggerCat = "Renderable";
}

namespace openspace {

Renderable* Renderable::createFromDictionary(const ghoul::Dictionary& dictionary)
{
    std::string name;
    dictionary.getValue(constants::scenegraphnode::keyName, name);

    if (!dictionary.hasValue<std::string>(constants::renderable::keyType)) {
        LERROR("Renderable '" << name << "' did not have key '"
                              << constants::renderable::keyType << "'");
        return nullptr;
    }
    std::string renderableType;
    dictionary.getValue(constants::renderable::keyType, renderableType);
    ghoul::TemplateFactory<Renderable>* factory
          = FactoryManager::ref().factory<Renderable>();
    Renderable* result = factory->create(renderableType, dictionary);
    if (result == nullptr) {
        LERROR("Failed to create a Renderable object of type '" << renderableType << "'");
        return nullptr;
    }

    return result;
}

Renderable::Renderable(const ghoul::Dictionary& dictionary)
{
//    std::string name;
//    dictionary.getValue(constants::scenegraphnode::keyName, name);
    setName("renderable");
}

Renderable::~Renderable()
{
}

void Renderable::setBoundingSphere(const PowerScaledScalar& boundingSphere)
{
    boundingSphere_ = boundingSphere;
}

const PowerScaledScalar& Renderable::getBoundingSphere()
{
    return boundingSphere_;
}

void Renderable::update()
{
}

}  // namespace openspace