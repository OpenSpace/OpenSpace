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

#include <openspace/properties/property.h>

#include <openspace/properties/propertyowner.h>

#include <ghoul/lua/ghoul_lua.h>

namespace openspace {
namespace properties {

namespace {
    const std::string _loggerCat = "Property";
    const std::string _metaDataKeyGuiName = "guiName";
    const std::string _metaDataKeyGroup = "Group";
    const std::string _metaDataKeyVisible = "isVisible";
    const std::string _metaDataKeyReadOnly = "isReadOnly";

    const std::string _metaDataKeyViewPrefix = "view.";
}

const std::string Property::ViewOptions::Color = "color";
const std::string Property::ViewOptions::LightPosition = "lightPosition";
const std::string Property::ViewOptions::PowerScaledCoordinate = "powerScaledCoordinate";
const std::string Property::ViewOptions::PowerScaledScalar = "powerScaledScalar";

const std::string Property::IdentifierKey = "Identifier";
const std::string Property::NameKey = "Name";
const std::string Property::TypeKey = "Type";
const std::string Property::MetaDataKey = "MetaData";

Property::Property(std::string identifier, std::string guiName)
    : _owner(nullptr)
    , _identifier(std::move(identifier))
{
    if (_identifier.empty())
        LWARNING("Property identifier is empty");
    if (guiName.empty())
        LWARNING("Property GUI name is empty");

    setVisible(true);
    _metaData.setValue(_metaDataKeyGuiName, std::move(guiName));
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
        if (!ownerId.empty())
            identifier = ownerId + "." + identifier;
        currentOwner = currentOwner->owner();
    }
    return identifier;
}

ghoul::any Property::get() const {
    return ghoul::any();
}

bool Property::getLuaValue(lua_State* state) const {
    return false;
}

void Property::set(ghoul::any value) {}

bool Property::setLuaValue(lua_State* state) {
    return false;
}

const std::type_info& Property::type() const {
    return typeid(void);
}

int Property::typeLua() const {
    return LUA_TNIL;
}

bool Property::getStringValue(std::string& value) const {
    return false;
}

bool Property::setStringValue(std::string value) {
    return false;
}

std::string Property::guiName() const {
    std::string result;
    _metaData.getValue(_metaDataKeyGuiName, result);
    return result;
}

std::string Property::description() const {
    return "return {" + generateBaseDescription() + "}";
}

void Property::setGroupIdentifier(std::string groupId) {
    _metaData.setValue(_metaDataKeyGroup, std::move(groupId));
}

std::string Property::groupIdentifier() const {
    std::string result;
    _metaData.getValue(_metaDataKeyGroup, result);
    return result;
}

void Property::setVisible(bool state) {
    _metaData.setValue(_metaDataKeyVisible, state);
}

bool Property::isVisible() const {
    bool visible = true;
    _metaData.getValue(_metaDataKeyVisible, visible);
    return visible;
}

void Property::setReadOnly(bool state) {
    _metaData.setValue(_metaDataKeyReadOnly, state);
}

void Property::setViewOption(std::string option, bool value) {
    _metaData.setValue(
        _metaDataKeyViewPrefix + option,
        value,
        ghoul::Dictionary::CreateIntermediate::Yes
    );
}

const ghoul::Dictionary& Property::metaData() const {
    return _metaData;
}

void Property::onChange(std::function<void()> callback) {
    _onChangeCallback = std::move(callback);
}

PropertyOwner* Property::owner() const
{
    return _owner;
}

void Property::setPropertyOwner(PropertyOwner* owner)
{
    _owner = owner;
}

void Property::notifyListener() {
    if (_onChangeCallback)
        _onChangeCallback();
}

std::string Property::generateBaseDescription() const {
    return
        TypeKey + " = \"" + className() + "\", " +
        IdentifierKey + " = \"" + fullyQualifiedIdentifier() + "\", " +
        NameKey + " = \"" + guiName() + "\", " +
        generateMetaDataDescription() + ", " + 
        generateAdditionalDescription();
}

std::string Property::generateMetaDataDescription() const {
    bool isVisible, isReadOnly;
    _metaData.getValue(_metaDataKeyVisible, isVisible);
    _metaData.getValue(_metaDataKeyReadOnly, isReadOnly);

    return
        MetaDataKey + " = {" +
            _metaDataKeyGroup +   " = '" + groupIdentifier() + "'," +
            _metaDataKeyVisible + " = " + (isVisible  ? "true" : "false") + "," +
            _metaDataKeyReadOnly +" = " + (isReadOnly ? "true" : "false") + "}";
}

std::string Property::generateAdditionalDescription() const {
    return "";
}

} // namespace properties
} // namespace openspace
