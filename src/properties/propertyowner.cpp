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

#include <openspace/properties/propertyowner.h>

#include <ghoul/logging/logmanager.h>

#include <algorithm>
#include <assert.h>

namespace openspace {
namespace properties {

namespace {
const std::string _loggerCat = "PropertyOwner";

bool propertyLess(Property* lhs, Property* rhs)
{
    return lhs->identifier() < rhs->identifier();
}
    
bool subOwnerLess(PropertyOwner* lhs, PropertyOwner* rhs) {
    return lhs->name() < rhs->name();
}
    
}

PropertyOwner::PropertyOwner()
    : _name("")
    , _owner(nullptr)
{
}

PropertyOwner::~PropertyOwner() {
    _properties.clear();
    _subOwners.clear();
}

const std::vector<Property*>& PropertyOwner::properties() const {
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
    assert(std::is_sorted(_properties.begin(), _properties.end(), propertyLess));

    // As the _properties list is sorted, just finding the lower bound is sufficient
    std::vector<Property*>::const_iterator it
          = std::lower_bound(_properties.begin(), _properties.end(), id,
                             [](Property* prop, const std::string& str) {
              return prop->identifier() < str;
          });

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
    else
        return *it;
}
    
bool PropertyOwner::hasProperty(const std::string& id) const {
    return property(id) != nullptr;
}
    
const std::vector<PropertyOwner*>& PropertyOwner::propertySubOwners() const {
    return _subOwners;
}

PropertyOwner* PropertyOwner::propertySubOwner(const std::string& name) const {
    assert(std::is_sorted(_subOwners.begin(), _subOwners.end(), subOwnerLess));
    
    // As the _subOwners list is sorted, getting the lower bound is sufficient
    std::vector<PropertyOwner*>::const_iterator it
    = std::lower_bound(_subOwners.begin(), _subOwners.end(), name,
                       [](PropertyOwner* owner, const std::string& str) {
                           return owner->name() < str;
                       });
    
    if (it == _subOwners.end() || (*it)->name() != name)
        return nullptr;
    else
        return *it;
}
    
bool PropertyOwner::hasPropertySubOwner(const std::string& name) const {
    return propertySubOwner(name) != nullptr;
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

void PropertyOwner::addProperty(Property* prop)
{
    assert(prop != nullptr);
    assert(std::is_sorted(_properties.begin(), _properties.end(), propertyLess));
    assert(std::is_sorted(_subOwners.begin(), _subOwners.end(), subOwnerLess));

    if (prop->identifier().empty()) {
        LERROR("No property identifier specified");
        return;
    }

    // See if we can find the identifier of the property to add in the properties list
    // The _properties list is sorted, so getting the lower bound is sufficient
    std::vector<Property*>::iterator it
          = std::lower_bound(_properties.begin(), _properties.end(), prop->identifier(),
                             [](Property* prop, const std::string& str) {
              return prop->identifier() < str;
          });

    // If we found the property identifier, we need to bail out
    if (it != _properties.end() && (*it)->identifier() == prop->identifier()) {
        LERROR("Property identifier '" << prop->identifier()
                                       << "' already present in PropertyOwner '"
                                       << name() << "'");
        return;
    } else {
        // Otherwise we still have to look if there is a PropertyOwner with the same name
        const bool hasOwner = hasPropertySubOwner(prop->identifier());
        if (hasOwner) {
            LERROR("Property identifier '" << prop->identifier() << "' already names a"
                << "registed PropertyOwner");
            return;
        }                    
        else {
            // now have found the correct position to add it in
            _properties.insert(it, prop);
            prop->setPropertyOwner(this);
        }
    }
}

void PropertyOwner::addProperty(Property& prop) {
    addProperty(&prop);
}
    
void PropertyOwner::addPropertySubOwner(openspace::properties::PropertyOwner* owner) {
    assert(owner != nullptr);
    assert(std::is_sorted(_properties.begin(), _properties.end(), propertyLess));
    assert(std::is_sorted(_subOwners.begin(), _subOwners.end(), subOwnerLess));
    
    if (owner->name().empty()) {
        LERROR("PropertyOwner did not have a name");
        return;
    }
    
    // See if we can find the name of the propertyowner to add using the lower bound
    std::vector<PropertyOwner*>::iterator it
    = std::lower_bound(_subOwners.begin(), _subOwners.end(), owner->name(),
                       [](PropertyOwner* owner, const std::string& str) {
                           return owner->name() < str;
                       });
    
    // If we found the propertyowner's name, we need to bail out
    if (it != _subOwners.end() && (*it)->name() == owner->name()) {
        LERROR("PropertyOwner '" << owner->name()
               << "' already present in PropertyOwner '" << name() << "'");
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
            // Otherwise we have found the correct position to add it in
            _subOwners.insert(it, owner);
            owner->setPropertyOwner(this);
        }
    }
    
}
    
void PropertyOwner::addPropertySubOwner(openspace::properties::PropertyOwner& owner) {
    addPropertySubOwner(&owner);
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
    } else
        LERROR("Property with identifier '" << prop->identifier()
                                            << "' not found for removal.");
}

void PropertyOwner::removeProperty(Property& prop) {
    removeProperty(&prop);
}
    
void PropertyOwner::removePropertySubOwner(openspace::properties::PropertyOwner* owner) {
    assert(owner != nullptr);
    
    // See if we can find the name of the propertyowner to add
    std::vector<PropertyOwner*>::iterator it
    = std::lower_bound(_subOwners.begin(), _subOwners.end(), owner->name(),
                       [](PropertyOwner* owner, const std::string& str) {
                           return owner->name() < str;
                       });
    
    // If we found the propertyowner, we can delete it
    if (it != _subOwners.end() && (*it)->name() == owner->name()) {
        _subOwners.erase(it);
    } else
        LERROR("PropertyOwner with name '" << owner->name()
               << "' not found for removal.");
}
    
void PropertyOwner::removePropertySubOwner(openspace::properties::PropertyOwner& owner) {
    removePropertySubOwner(&owner);
}

void PropertyOwner::setName(std::string name) {
    _name = std::move(name);
}

const std::string& PropertyOwner::name() const {
    return _name;
}

} // namespace properties
} // namespace openspace
