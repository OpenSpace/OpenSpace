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

#include <openspace/properties/propertyowner.h>

#include <algorithm>
#include <assert.h>

namespace openspace {
namespace properties {

namespace {
    const std::string _loggerCat = "PropertyOwner";

    bool propertyLess(Property* lhs, Property* rhs) {
        return lhs->identifier() < rhs->identifier();
    }
}

PropertyOwner::~PropertyOwner() {}

const std::vector<Property*>& PropertyOwner::properties() const {
    return _properties;
}

Property* PropertyOwner::property(const std::string& id) const {
    assert(std::is_sorted(_properties.begin(), _properties.end(), propertyLess));

    std::vector<Property*>::const_iterator it
          = std::lower_bound(_properties.begin(), _properties.end(), id,
                             [](Property* prop, const std::string& str) {
              return prop->identifier() < str;
          });

    if (it == _properties.end() || (*it)->identifier() != id)
        return nullptr;
    else
        return *it;
}

void PropertyOwner::setPropertyGroupName(std::string groupID, std::string name) {
    _groupNames[std::move(groupID)] = std::move(name);
}

const std::string& PropertyOwner::propertyGroupName(const std::string& groupID) const {
    auto it = _groupNames.find(groupID);
    if (it == _groupNames.end())
        return groupID;
    else
        return it->second;
}

void PropertyOwner::addProperty(Property* prop) {
    assert(prop != nullptr);
    assert(std::is_sorted(_properties.begin(), _properties.end(), propertyLess));

    if (prop->identifier().empty()) {
        LERROR("No property identifier specified");
        return;
    }

    // See if we can find the identifier of the property to add in the properties list
    std::vector<Property*>::iterator it
        = std::lower_bound(_properties.begin(), _properties.end(), prop->identifier(),
        [](Property* prop, const std::string& str) {
        return prop->identifier() < str;
    });

    // If we found the property identifier, we need to bail out
    if (it != _properties.end() && (*it)->identifier() == prop->identifier()) {
        LERROR("Property identifier '" << prop->identifier()
                                       << "' already present in PropertyOwner");
        return;
    }
    else {
        // Otherwise we have found the correct position to add it in
        _properties.insert(it, prop);
        prop->setPropertyOwner(this);
    }
}

void PropertyOwner::addProperty(Property& prop) {
    addProperty(&prop);
}

void PropertyOwner::removeProperty(Property* prop) {
    assert(prop != nullptr);

    // See if we can find the identifier of the property to add in the properties list
    std::vector<Property*>::iterator it
        = std::lower_bound(_properties.begin(), _properties.end(), prop->identifier(),
        [](Property* prop, const std::string& str) {
        return prop->identifier() < str;
    });

    // If we found the property identifier, we can delete it
    if (it != _properties.end() && (*it)->identifier() == prop->identifier()) {
        (*it)->setPropertyOwner(nullptr);
        _properties.erase(it);
    }
    else
        LERROR("Property with identifier '" << prop->identifier()
                                            << "' not found for removal.");
}

void PropertyOwner::removeProperty(Property& prop) {
    removeProperty(&prop);
}

} // namespace properties
} // namespace openspace
