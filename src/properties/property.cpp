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

#include <openspace/properties/property.h>

#include <openspace/properties/propertyowner.h>

#include <ghoul/lua/ghoul_lua.h>

namespace openspace {
namespace properties {

namespace {
	const std::string _loggerCat = "Property";
	const std::string _metaDataKeyGuiName = "guiName";
    const std::string _metaDataKeyGroup = "group";
    const std::string _metaDataKeyVisible = "isVisible";
    const std::string _metaDataKeyReadOnly = "isReadOnly";

	const std::string _metaDataKeyViewPrefix = "view.";
}

const std::string Property::ViewOptions::Color = "color";
const std::string Property::ViewOptions::LightPosition = "lightPosition";
const std::string Property::ViewOptions::PowerScaledCoordinate = "powerScaledCoordinate";
const std::string Property::ViewOptions::PowerScaledScalar = "powerScaledScalar";
 

Property::Property(std::string identifier, std::string guiName)
    : _identifier(std::move(identifier))
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
		identifier = ownerId + "." + identifier;
		currentOwner = currentOwner->owner();
	}
	return identifier;
}

boost::any Property::get() const {
    return boost::any();
}

bool Property::getLua(lua_State* state) const {
	return true;
}

void Property::set(boost::any value) {
}

bool Property::setLua(lua_State* state) {
	return false;
}

const std::type_info& Property::type() const {
    return typeid(void);
}

int Property::typeLua() const {
	return LUA_TNIL;
}

std::string Property::guiName() const {
	std::string result;
	_metaData.getValue(_metaDataKeyGuiName, result);
    return std::move(result);
}

void Property::setGroupIdentifier(std::string groupId) {
    _metaData.setValue(_metaDataKeyGroup, std::move(groupId));
}

std::string Property::groupIdentifier() const {
	std::string result;
	_metaData.getValue(_metaDataKeyGroup, result);
    return std::move(result);
}

void Property::setVisible(bool state) {
    _metaData.setValue(_metaDataKeyVisible, state);
}

void Property::setReadOnly(bool state) {
    _metaData.setValue(_metaDataKeyReadOnly, state);
}

void Property::setViewOption(std::string option, bool value) {
    _metaData.setValue(_metaDataKeyViewPrefix + option, value, true);
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

} // namespace properties
} // namespace openspace
