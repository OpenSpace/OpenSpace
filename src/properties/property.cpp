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

#include <openspace/properties/property.h>

#include <openspace/properties/propertyowner.h>

#include <ghoul/lua/ghoul_lua.h>

namespace openspace {
namespace properties {

namespace {
    const char* MetaDataKeyGuiName = "guiName";
    const char* MetaDataKeyGroup = "Group";
    const char* MetaDataKeyVisibility = "Visibility";
    const char* MetaDataKeyReadOnly = "isReadOnly";

    const char* _metaDataKeyViewPrefix = "view.";
}

const char* Property::ViewOptions::Color = "color";
const char* Property::ViewOptions::LightPosition = "lightPosition";

const char* Property::IdentifierKey = "Identifier";
const char* Property::NameKey = "Name";
const char* Property::TypeKey = "Type";
const char* Property::MetaDataKey = "MetaData";

Property::Property(std::string identifier, std::string guiName, Visibility visibility)
    : _owner(nullptr)
    , _identifier(std::move(identifier))
{
    ghoul_assert(!_identifier.empty(), "Identifier must not be empty");
    ghoul_assert(!guiName.empty(), "guiName must not be empty");

    setVisibility(visibility);
    _metaData.setValue(MetaDataKeyGuiName, std::move(guiName));
}

Property::~Property() {}

const std::string& Property::identifier() const {
    return _identifier;
}

std::string Property::fullyQualifiedIdentifier() const {
    std::string identifier = _identifier;
    PropertyOwner* currentOwner = owner();
    while (currentOwner) {
        std::string ownerId = currentOwner->name();
        if (!ownerId.empty()) {
            identifier = ownerId + "." + identifier;
        }
        currentOwner = currentOwner->owner();
    }
    return identifier;
}

ghoul::any Property::get() const {
    return ghoul::any();
}

bool Property::getLuaValue(lua_State*) const {
    return false;
}

void Property::set(ghoul::any) {}

bool Property::setLuaValue(lua_State*) {
    return false;
}

const std::type_info& Property::type() const {
    return typeid(void);
}

int Property::typeLua() const {
    return LUA_TNIL;
}

bool Property::getStringValue(std::string&) const {
    return false;
}

bool Property::setStringValue(std::string) {
    return false;
}

std::string Property::guiName() const {
    std::string result;
    _metaData.getValue(MetaDataKeyGuiName, result);
    return result;
}

std::string Property::description() const {
    return "return {" + generateBaseDescription() + "}";
}

void Property::setGroupIdentifier(std::string groupId) {
    _metaData.setValue(MetaDataKeyGroup, std::move(groupId));
}

std::string Property::groupIdentifier() const {
    std::string result;
    _metaData.getValue(MetaDataKeyGroup, result);
    return result;
}

void Property::setVisibility(Visibility visibility) {
    _metaData.setValue(
        MetaDataKeyVisibility,
        static_cast<std::underlying_type_t<Visibility>>(visibility)
    );
}

Property::Visibility Property::visibility() const {
    return
        static_cast<Visibility>(
            _metaData.value<std::underlying_type_t<Visibility>>(MetaDataKeyVisibility)
        );
}

void Property::setReadOnly(bool state) {
    _metaData.setValue(MetaDataKeyReadOnly, state);
}

void Property::setViewOption(std::string option, bool value) {
    _metaData.setValue(
        _metaDataKeyViewPrefix + option,
        value,
        ghoul::Dictionary::CreateIntermediate::Yes
    );
}

bool Property::viewOption(const std::string& option, bool defaultValue) const {
    bool v = defaultValue;
    _metaData.getValue(_metaDataKeyViewPrefix + option, v);
    return v;
}

const ghoul::Dictionary& Property::metaData() const {
    return _metaData;
}

void Property::onChange(std::function<void()> callback) {
    _onChangeCallback = std::move(callback);
}

PropertyOwner* Property::owner() const {
    return _owner;
}

void Property::setPropertyOwner(PropertyOwner* owner) {
    _owner = owner;
}

void Property::notifyListener() {
    if (_onChangeCallback) {
        _onChangeCallback();
    }
}

std::string Property::generateBaseDescription() const {
    return
        std::string(TypeKey) + " = \"" + className() + "\", " +
        IdentifierKey + " = \"" + fullyQualifiedIdentifier() + "\", " +
        NameKey + " = \"" + guiName() + "\", " +
        generateMetaDataDescription() + ", " + 
        generateAdditionalDescription();
}

std::string Property::generateMetaDataDescription() const {
    static const std::map<Visibility, std::string> VisibilityConverter = {
        { Visibility::All, "All" },
        { Visibility::Developer, "Developer" },
        { Visibility::User, "User" },
        { Visibility::Hidden, "Hidden" }
    };
    Visibility visibility = _metaData.value<Visibility>(MetaDataKeyVisibility);
    bool isReadOnly = _metaData.value<bool>(MetaDataKeyReadOnly);

    std::string vis = VisibilityConverter.at(visibility);

    return
        std::string(MetaDataKey) + " = {" +
        MetaDataKeyGroup +   " = '" + groupIdentifier() + "'," +
        MetaDataKeyVisibility + " = " + vis + "," +
        MetaDataKeyReadOnly +" = " + (isReadOnly ? "true" : "false") + "}";
}

std::string Property::generateAdditionalDescription() const {
    return "";
}

} // namespace properties
} // namespace openspace
