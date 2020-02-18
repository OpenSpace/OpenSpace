/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <openspace/scene/scene.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/invariants.h>
#include <algorithm>
#include <numeric>

namespace {
    constexpr const char* _loggerCat = "PropertyOwner";
} // namespace

namespace openspace::properties {

PropertyOwner::PropertyOwner(PropertyOwnerInfo info)
    : DocumentationGenerator(
        "Property Owners",
        "propertyOwners",
        {
            { "propertyOwnersTemplate","${WEB}/documentation/propertyowners.hbs" },
            { "propertyTemplate","${WEB}/documentation/property.hbs" },
            { "propertylistTemplate","${WEB}/documentation/propertylist.hbs" }
        }
    )
    , _identifier(std::move(info.identifier))
    , _guiName(std::move(info.guiName))
    , _description(std::move(info.description))
{
    ghoul_precondition(
        _identifier.find_first_of("\t\n ") == std::string::npos,
        "Identifier must contain any whitespaces"
    );
    ghoul_precondition(
        _identifier.find_first_of('.') == std::string::npos,
        "Identifier must contain any whitespaces"
    );
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

Property* PropertyOwner::property(const std::string& uri) const {
    auto it = std::find_if(
        _properties.begin(),
        _properties.end(),
        [&uri](Property* prop) { return prop->identifier() == uri; }
    );

    if (it == _properties.end() || (*it)->identifier() != uri) {
        // if we do not own the searched property, it must consist of a concatenated
        // name and we can delegate it to a subowner
        const size_t ownerSeparator = uri.find(URISeparator);
        if (ownerSeparator == std::string::npos) {
            // if we do not own the property and there is no separator, it does not exist
            return nullptr;
        }
        else {
            const std::string ownerName = uri.substr(0, ownerSeparator);
            const std::string propertyName = uri.substr(ownerSeparator + 1);

            PropertyOwner* owner = propertySubOwner(ownerName);
            if (!owner) {
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

bool PropertyOwner::hasProperty(const std::string& uri) const {
    return property(uri) != nullptr;
}

bool PropertyOwner::hasProperty(const Property* prop) const {
    ghoul_precondition(prop != nullptr, "prop must not be nullptr");

    std::vector<Property*>::const_iterator it = std::find(
        _properties.begin(), _properties.end(), prop
    );

    return it != _properties.end();
}

const std::vector<PropertyOwner*>& PropertyOwner::propertySubOwners() const {
    return _subOwners;
}

PropertyOwner* PropertyOwner::propertySubOwner(const std::string& identifier) const {
    std::vector<PropertyOwner*>::const_iterator it = std::find_if(
        _subOwners.begin(),
        _subOwners.end(),
        [&identifier](PropertyOwner* owner) { return owner->identifier() == identifier; }
    );

    if (it == _subOwners.end() || (*it)->identifier() != identifier) {
        return nullptr;
    }
    else {
        return *it;
    }
}

bool PropertyOwner::hasPropertySubOwner(const std::string& identifier) const {
    return propertySubOwner(identifier) != nullptr;
}

void PropertyOwner::setPropertyGroupName(std::string groupID, std::string identifier) {
    _groupNames[std::move(groupID)] = std::move(identifier);
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
    ghoul_precondition(prop != nullptr, "prop must not be nullptr");

    if (prop->identifier().empty()) {
        LERROR("No property identifier specified");
        return;
    }
    // See if we can find the identifier of the property to add in the properties list
    std::vector<Property*>::const_iterator it = std::find_if(
        _properties.begin(),
        _properties.end(),
        [id = prop->identifier()](Property* p) { return p->identifier() == id; }
    );

    // If we found the property identifier, we need to bail out
    if (it != _properties.end() && (*it)->identifier() == prop->identifier()) {
        LERROR(fmt::format(
            "Property identifier '{}' already present in PropertyOwner '{}'",
            prop->identifier(),
            identifier()
        ));
        return;
    }
    else {
        // Otherwise we still have to look if there is a PropertyOwner with the same name
        const bool hasOwner = hasPropertySubOwner(prop->identifier());
        if (hasOwner) {
            LERROR(fmt::format(
                "Property identifier '{}' already names a registered PropertyOwner",
                prop->identifier()
            ));
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
    ghoul_precondition(owner != nullptr, "owner must not be nullptr");
    ghoul_precondition(
        !owner->identifier().empty(),
        "PropertyOwner must have an identifier"
    );

    // See if we can find the name of the propertyowner to add using the lower bound
    std::vector<PropertyOwner*>::const_iterator it = std::find_if(
        _subOwners.begin(),
        _subOwners.end(),
        [identifier = owner->identifier()](PropertyOwner* o) {
            return o->identifier() == identifier;
        }
    );

    // If we found the propertyowner's name, we need to bail out
    if (it != _subOwners.end() && (*it)->identifier() == owner->identifier()) {
        LERROR(fmt::format(
            "PropertyOwner '{}' already present in PropertyOwner '{}'",
            owner->identifier(),
            identifier()
        ));
        return;
    }
    else {
        // We still need to check if the PropertyOwners name is used in a Property
        const bool hasProp = hasProperty(owner->identifier());
        if (hasProp) {
            LERROR(fmt::format(
                "PropertyOwner '{}'s name already names a Property", owner->identifier()
            ));
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
    ghoul_precondition(prop != nullptr, "prop must not be nullptr");

    // See if we can find the identifier of the property to add in the properties list
    std::vector<Property*>::const_iterator it = std::find_if(
        _properties.begin(),
        _properties.end(),
        [id = prop->identifier()](Property* p) { return p->identifier() == id; }
    );

    // If we found the property identifier, we can delete it
    if (it != _properties.end() && (*it)->identifier() == prop->identifier()) {
        (*it)->setPropertyOwner(nullptr);
        _properties.erase(it);
    }
    else {
        LERROR(fmt::format(
            "Property with identifier '{}' not found for removal", prop->identifier()
        ));
    }
}

void PropertyOwner::removeProperty(Property& prop) {
    removeProperty(&prop);
}

void PropertyOwner::removePropertySubOwner(openspace::properties::PropertyOwner* owner) {
    ghoul_precondition(owner != nullptr, "owner must not be nullptr");

    // See if we can find the name of the propertyowner to add
    std::vector<PropertyOwner*>::const_iterator it = std::find_if(
        _subOwners.begin(),
        _subOwners.end(),
        [identifier = owner->identifier()](PropertyOwner* o) {
            return o->identifier() == identifier;
        }
    );

    // If we found the propertyowner, we can delete it
    if (it != _subOwners.end() && (*it)->identifier() == owner->identifier()) {
        _subOwners.erase(it);
    }
    else {
        LERROR(fmt::format(
            "PropertyOwner with name '{}' not found for removal", owner->identifier()
        ));
    }
}

void PropertyOwner::removePropertySubOwner(openspace::properties::PropertyOwner& owner) {
    removePropertySubOwner(&owner);
}

void PropertyOwner::setIdentifier(std::string identifier) {
    ghoul_precondition(
        _identifier.find_first_of("\t\n ") == std::string::npos,
        "Identifier must contain any whitespaces"
    );
    ghoul_precondition(
        _identifier.find_first_of('.') == std::string::npos,
        "Identifier must contain any whitespaces"
    );

    _identifier = std::move(identifier);
}

const std::string& PropertyOwner::identifier() const {
    return _identifier;
}

void PropertyOwner::setGuiName(std::string guiName) {
    _guiName = std::move(guiName);
}

const std::string& PropertyOwner::guiName() const {
    return _guiName.empty() ? _identifier : _guiName;
}

void PropertyOwner::setDescription(std::string description) {
    _description = std::move(description);
}

const std::string& PropertyOwner::description() const {
    return _description;
}

const std::vector<std::string>& PropertyOwner::tags() const {
    return _tags;
}

void PropertyOwner::addTag(std::string tag) {
    _tags.push_back(std::move(tag));
}

void PropertyOwner::removeTag(const std::string& tag) {
    _tags.erase(std::remove(_tags.begin(), _tags.end(), tag), _tags.end());
}

std::string PropertyOwner::generateJson() const {
    std::function<std::string(properties::PropertyOwner*)> createJson =
        [&createJson](properties::PropertyOwner* owner) -> std::string
    {
        constexpr const char* replStr = R"("{}": "{}")";

        std::stringstream json;
        json << "{";
        json << fmt::format(replStr, "name", owner->identifier()) << ",";

        json << "\"properties\": [";
        const std::vector<properties::Property*>& properties = owner->properties();
        for (properties::Property* p : properties) {
            json << "{";
            json << fmt::format(replStr, "id", p->identifier()) << ",";
            json << fmt::format(replStr, "type", p->className()) << ",";
            json << fmt::format(
                replStr, "fullyQualifiedId", p->fullyQualifiedIdentifier()
            ) << ",";
            json << fmt::format(replStr, "guiName", p->guiName()) << ",";
            json << fmt::format(replStr, "description", escapedJson(p->description()));
            json << "}";
            if (p != properties.back()) {
                json << ",";
            }
        }
        json << "],";

        json << "\"propertyOwners\": [";
        auto propertyOwners = owner->propertySubOwners();
        for (properties::PropertyOwner* o : propertyOwners) {
            json << createJson(o);
            if (o != propertyOwners.back()) {
                json << ",";
            }
        }
        json << "]";
        json << "}";

        return json.str();
    };


    std::stringstream json;
    json << "[";
    std::vector<PropertyOwner*> subOwners = propertySubOwners();
    if (!subOwners.empty()) {
        json << std::accumulate(
            std::next(subOwners.begin()),
            subOwners.end(),
            createJson(*subOwners.begin()),
            [createJson](std::string a, PropertyOwner* n) {
            //TODO figure out how to ignore scene when its not the root
            //right now will be done on client side
            return a + "," + createJson(n);
        }
        );
    }

    json << "]";

    return json.str();
}

} // namespace openspace::properties
