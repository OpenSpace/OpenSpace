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

#ifndef SCENEGRAPHNODE_INL
#define SCENEGRAPHNODE_INL

#include <util/factorymanager.h>

namespace openspace {

template<class T>
bool safeInitializeWithDictionary(T** object, const std::string& key,
ghoul::Dictionary* dictionary, const std::string& path = "") {
    if(dictionary->hasKey(key)) {
        ghoul::Dictionary tmpDictionary;
        if(dictionary->getValue(key, tmpDictionary)) {
            std::string renderableType;
            if(tmpDictionary.getValue("Type", renderableType)) {
                ghoul::TemplateFactory<T>* factory = FactoryManager::ref().factoryByType<T>();
                T* tmp = factory->create(renderableType);
                if(tmp != nullptr) {
                    if ( ! tmpDictionary.hasKey("Path") && path != "") {
                        tmpDictionary.setValue("Path", path);
                    }
                    if(tmp->initializeWithDictionary(&tmpDictionary)) {
                        *object = tmp;
                        return true;
                    } else {
                        delete tmp;
                    }
                }
            }
        }
    }
    return false;
}

} // namespace openspace

#endif