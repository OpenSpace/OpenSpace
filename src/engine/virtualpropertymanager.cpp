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

#include <openspace/engine/virtualpropertymanager.h>

#include <openspace/properties/property.h>
#include <algorithm>

namespace openspace {

// The VirtualPropertyManager cannot have an identifier (and thus cannot be part of
// another PropertyOwner) as otherwise the regex-as-name trick would no longer work. I
// don't particular like this implementation, but it's what we got for now;  I'm open to
// replacing it with a better mechanism
VirtualPropertyManager::VirtualPropertyManager()
    : properties::PropertyOwner({ "" })
{}

void VirtualPropertyManager::addProperty(std::unique_ptr<properties::Property> prop) {
    // PropertyOwner does not take the ownership of the pointer
    properties::PropertyOwner::addProperty(prop.get());

    // So we store the pointer locally instead
    _properties.push_back(std::move(prop));
}

void VirtualPropertyManager::removeProperty(properties::Property* prop) {
    properties::PropertyOwner::removeProperty(prop);
    _properties.erase(
        std::remove_if(
            _properties.begin(),
            _properties.end(),
            [prop](const std::unique_ptr<properties::Property>& p) {
                return p.get() == prop;
            }
        ),
        _properties.end()
    );
}

} // namespace openspace
