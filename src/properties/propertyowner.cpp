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

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/property.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>

#include <algorithm>

namespace openspace::properties {

namespace {
    const char* _loggerCat = "PropertyOwner";

    bool propertyLess(Property* lhs, Property* rhs) {
        return lhs->identifier() < rhs->identifier();
    }

    bool subOwnerLess(PropertyOwner* lhs, PropertyOwner* rhs) {
        return lhs->name() < rhs->name();
    }
} // namespace


PropertyOwner::PropertyOwner(PropertyOwnerInfo info)
    : _name(std::move(info.name))
    , _description(std::move(info.description))
    , _owner(nullptr)
{}

PropertyOwner::~PropertyOwner() {
    _properties.clear();
    _subOwners.clear();
}

std::vector<Property*> PropertyOwner::properties() const {
    return _properties;
}

std::vector<Property*> PropertyOwner::propertiesRecursive() const {
    std::vector<Property*> props = properties();

    for (const PropertyOwner* owner : _subOwners) {
        std::vector<Property*> p = owner->propertiesRecursive();
        props.insert(props.end(), p.begin(), p.end());
    }

    return props;
}

Property* PropertyOwner::property(const std::string& id) const {
    std::vector<Property*>::const_iterator it = std::find_if(
        _properties.begin(),
        _properties.end(),
        [&id](Property* prop) { return prop->identifier() == id; }
    );

    if (it == _properties.end() || (*it)->identifier() != id) {
        // if we do not own the searched property, it must consist of a concatenated
        // name and we can delegate it to a subowner
        const size_t ownerSeparator = id.find(URISeparator);
        if (ownerSeparator == std::string::npos) {
            // if we do not own the property and there is no separator, it does not exist
            return nullptr;
        }
        else {
            const std::string ownerName = id.substr(0, ownerSeparator);
            const std::string propertyName = id.substr(ownerSeparator + 1);
            
            PropertyOwner* owner = propertySubOwner(ownerName);
            if (owner == nullptr) {
                return nullptr;
            }
            else {
                // Recurse into the subOwner
                return owner->property(propertyName);
            }
        }
    }
    else {
        return *it;
    }
}
    
bool PropertyOwner::hasProperty(const std::string& id) const {
    return property(id) != nullptr;
}
    
std::vector<PropertyOwner*> PropertyOwner::propertySubOwners() const {
    return _subOwners;
}

PropertyOwner* PropertyOwner::propertySubOwner(const std::string& name) const {
    std::vector<PropertyOwner*>::const_iterator it = std::find_if(
        _subOwners.begin(),
        _subOwners.end(),
        [&name](PropertyOwner* owner) { return owner->name() == name;  }
    );
    
    if (it == _subOwners.end() || (*it)->name() != name) {
        return nullptr;
    }
    else {
        return *it;
    }
}
    
bool PropertyOwner::hasPropertySubOwner(const std::string& name) const {
    return propertySubOwner(name) != nullptr;
}

void PropertyOwner::setPropertyGroupName(std::string groupID, std::string name) {
    _groupNames[std::move(groupID)] = std::move(name);
}
    
std::string PropertyOwner::propertyGroupName(const std::string& groupID) const {
    auto it = _groupNames.find(groupID);
    if (it == _groupNames.end()) {
        return groupID;
    }
    else {
        return it->second;
    }
}

void PropertyOwner::addProperty(Property* prop) {
    ghoul_assert(prop != nullptr, "prop must not be nullptr");

    if (prop->identifier().empty()) {
        LERROR("No property identifier specified");
        return;
    }
    // See if we can find the identifier of the property to add in the properties list
    std::vector<Property*>::const_iterator it = std::find_if(
        _properties.begin(),
        _properties.end(),
        [id = prop->identifier()](Property* prop) { return prop->identifier() == id; }
    );

    // If we found the property identifier, we need to bail out
    if (it != _properties.end() && (*it)->identifier() == prop->identifier()) {
        LERROR("Property identifier '" << prop->identifier() <<
            "' already present in PropertyOwner '" << name() << "'");
        return;
    } else {
        // Otherwise we still have to look if there is a PropertyOwner with the same name
        const bool hasOwner = hasPropertySubOwner(prop->identifier());
        if (hasOwner) {
            LERROR("Property identifier '" << prop->identifier() << "' already names a "
                << "registed PropertyOwner");
            return;
        }                    
        else {
            _properties.push_back(prop);
            prop->setPropertyOwner(this);
        }
    }
}

void PropertyOwner::addProperty(Property& prop) {
    addProperty(&prop);
}
    
void PropertyOwner::addPropertySubOwner(openspace::properties::PropertyOwner* owner) {
    ghoul_assert(owner != nullptr, "owner must not be nullptr");
    ghoul_assert(!owner->name().empty(), "PropertyOwner must have a name");
    
    // See if we can find the name of the propertyowner to add using the lower bound
    std::vector<PropertyOwner*>::const_iterator it = std::find_if(
        _subOwners.begin(),
        _subOwners.end(),
        [name = owner->name()](PropertyOwner* owner) { return owner->name() == name;  }
    );
    
    // If we found the propertyowner's name, we need to bail out
    if (it != _subOwners.end() && (*it)->name() == owner->name()) {
        LERROR("PropertyOwner '" << owner->name() <<
            "' already present in PropertyOwner '" << name() << "'");
        return;
    } else {
        // We still need to check if the PropertyOwners name is used in a Property
        const bool hasProp = hasProperty(owner->name());
        if (hasProp) {
            LERROR("PropertyOwner '" << owner->name() << "'s name already names a "
                 << "Property");
            return;
        }
        else {
            _subOwners.push_back(owner);
            owner->setPropertyOwner(this);
        }
    }
}
    
void PropertyOwner::addPropertySubOwner(openspace::properties::PropertyOwner& owner) {
    addPropertySubOwner(&owner);
}

void PropertyOwner::removeProperty(Property* prop) {
    ghoul_assert(prop != nullptr, "prop must not be nullptr");

    // See if we can find the identifier of the property to add in the properties list
    std::vector<Property*>::const_iterator it = std::find_if(
        _properties.begin(),
        _properties.end(),
        [id = prop->identifier()](Property* prop) { return prop->identifier() == id; }
    );

    // If we found the property identifier, we can delete it
    if (it != _properties.end() && (*it)->identifier() == prop->identifier()) {
        (*it)->setPropertyOwner(nullptr);
        _properties.erase(it);
    } else {
        LERROR("Property with identifier '" << prop->identifier() <<
            "' not found for removal.");
    }
}

void PropertyOwner::removeProperty(Property& prop) {
    removeProperty(&prop);
}
    
void PropertyOwner::removePropertySubOwner(openspace::properties::PropertyOwner* owner) {
    ghoul_assert(owner != nullptr, "owner must not be nullptr");
    
    // See if we can find the name of the propertyowner to add
    std::vector<PropertyOwner*>::const_iterator it = std::find_if(
        _subOwners.begin(),
        _subOwners.end(),
        [name = owner->name()](PropertyOwner* owner) { return owner->name() == name;  }
    );
    
    // If we found the propertyowner, we can delete it
    if (it != _subOwners.end() && (*it)->name() == owner->name()) {
        _subOwners.erase(it);
    } else {
        LERROR("PropertyOwner with name '" << owner->name() <<
            "' not found for removal.");
    }
}
    
void PropertyOwner::removePropertySubOwner(openspace::properties::PropertyOwner& owner) {
    removePropertySubOwner(&owner);
}

void PropertyOwner::setName(std::string name) {
    _name = std::move(name);
}

std::string PropertyOwner::name() const {
    return _name;
}

void PropertyOwner::setDescription(std::string description) {
    _description = std::move(description);
}

std::string PropertyOwner::description() const {
    return _description;
}

std::vector<std::string> PropertyOwner::tags() const {
    return _tags;
}

void PropertyOwner::addTag(std::string tag) {
    _tags.push_back(std::move(tag));
}

} // namespace openspace::properties
