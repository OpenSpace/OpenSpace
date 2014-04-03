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

Property::Property(const std::string& identifier, const std::string& guiName)
    : _identifier(identifier)
    , _guiName(guiName)
    , _groupId("")
    , _isVisible(true)
    , _isReadOnly(false)
{}

Property::~Property() {}

//std::string Property::className() const {
//    return classNameHelper(this);
//    //return PropertyDelegate<Property>::className();
//}

const std::string& Property::identifier() const {
    return _identifier;
}

boost::any Property::get() const {
    return boost::any();
}

void Property::set(const boost::any& value) {}

const std::type_info& Property::type() const {
    return typeid(void);
}

const std::string& Property::guiName() const {
    return _guiName;
}

void Property::setGroupIdentifier(const std::string& groupId) {
    _groupId = groupId;
}

const std::string& Property::groupIdentifier() const {
    return _groupId;
}

void Property::setVisible(bool state) {
    _isVisible = state;
}

bool Property::isVisible() const {
    return _isVisible;
}

void Property::setReadOnly(bool state) {
    _isReadOnly = state;
}

bool Property::isReadOnly() const {
    return _isReadOnly;
}

} // namespace properties
} // namespace openspace
