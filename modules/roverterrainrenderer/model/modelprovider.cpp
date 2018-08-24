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

#include <modules/roverterrainrenderer/model/modelprovider.h>
#include <openspace/util/factorymanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/logging/logmanager.h>

namespace {
    const std::string _loggerCat = "ModelProvider";
    const std::string Type = "Type";
}

namespace openspace {
std::unique_ptr<ModelProvider> ModelProvider::createFromDictionary(const ghoul::Dictionary& dictionary) {
    std::string type = "ModelProvider";
    dictionary.getValue(Type, type);
    auto factory = FactoryManager::ref().factory<ModelProvider>();
    std::unique_ptr<ModelProvider> mp = factory->create(type, dictionary);

    return mp;
}

ModelProvider::ModelProvider(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner(openspace::properties::PropertyOwner::PropertyOwnerInfo{ "modelProvider", "modelProvider" })
{}

void ModelProvider::initialize() {
 return;
}

} // namespace openspace
