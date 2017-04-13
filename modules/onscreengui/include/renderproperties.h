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

#ifndef __OPENSPACE_MODULE_ONSCREENGUI___RENDERPROPERTIES___H__
#define __OPENSPACE_MODULE_ONSCREENGUI___RENDERPROPERTIES___H__

#include <ghoul/misc/boolean.h>

#include <string>

namespace openspace {

namespace properties {
    class Property;
} // namespace properties

using IsRegularProperty = ghoul::Boolean;

void executeScript(const std::string& id, const std::string& value,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderBoolProperty(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderOptionProperty(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderSelectionProperty(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderStringProperty(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderDoubleProperty(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderIntProperty(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderIVec2Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderIVec3Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderIVec4Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderFloatProperty(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderVec2Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderVec3Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderVec4Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderDVec2Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderDVec3Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderDVec4Property(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

void renderTriggerProperty(properties::Property* prop, const std::string& ownerName,
    IsRegularProperty isRegular = IsRegularProperty::Yes);

} // namespace openspace

#endif // __OPENSPACE_MODULE_ONSCREENGUI___RENDERPROPERTIES___H__
