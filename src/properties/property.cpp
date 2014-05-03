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

#include "openspace/properties/property.h"

namespace openspace {
namespace properties {

namespace {
    const std::string _metaDataKeyGroup = "group";
    const std::string _metaDataKeyVisible = "isVisible";
    const std::string _metaDataKeyReadOnly = "isReadOnly";
}

const std::string Property::ViewOptions::Color = "color";
const std::string Property::ViewOptions::LightPosition = "lightPosition";
const std::string Property::ViewOptions::PowerScaledCoordinate = "powerScaledCoordinate";
const std::string Property::ViewOptions::PowerScaledScalar = "powerScaledScalar";
 

Property::Property(std::string identifier, std::string guiName)
    : _identifier(std::move(identifier))
    , _guiName(std::move(guiName))
{
    setVisible(true);
}

Property::~Property() {}

const std::string& Property::identifier() const {
    return _identifier;
}

boost::any Property::get() const {
    return boost::any();
}

void Property::set(boost::any value) {}

const std::type_info& Property::type() const {
    return typeid(void);
}

const std::string& Property::guiName() const {
    return _guiName;
}

void Property::setGroupIdentifier(std::string groupId) {
    _metaData.setValue(_metaDataKeyGroup, std::move(groupId));
}

std::string Property::groupIdentifier() const {
    std::string result = "";
    _metaData.getValue(_metaDataKeyGroup, result);
    return std::move(result);
}

void Property::setVisible(bool state) {
    _metaData.setValue(_metaDataKeyVisible, state);
}

bool Property::isVisible() const {
    bool result = false;
    _metaData.getValue(_metaDataKeyVisible, result);
    return result;
}

void Property::setReadOnly(bool state) {
    _metaData.setValue(_metaDataKeyReadOnly, state);
}

bool Property::isReadOnly() const {
    bool result = false;
    _metaData.getValue(_metaDataKeyReadOnly, result);
    return result;
}

void Property::setViewOption(std::string option, bool value) {
    _metaData.setValue("view." + option, value, true);
}

bool Property::viewOption(const std::string& option) const {
    bool result = false;
    _metaData.getValue("view." + option, result);
    return result;
}

const ghoul::Dictionary& Property::metaData() const {
    return _metaData;
}

void Property::onChange(std::function<void()> callback) {
    _onChangeCallbacks.emplace_back(std::move(callback));

}

PropertyOwner* Property::owner() const
{
    return _owner;
}

void Property::setPropertyOwner(PropertyOwner* owner)
{
    _owner = owner;
}

} // namespace properties
} // namespace openspace
