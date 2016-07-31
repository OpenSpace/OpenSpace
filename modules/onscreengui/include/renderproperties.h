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

#ifndef __RENDERPROPERTIES_H__
#define __RENDERPROPERTIES_H__

#include <string>

namespace openspace {

namespace properties {
    class Property;
}

void executeScript(const std::string& id, const std::string& value);
void renderBoolProperty(properties::Property* prop, const std::string& ownerName);
void renderOptionProperty(properties::Property* prop, const std::string& ownerName);
void renderSelectionProperty(properties::Property* prop, const std::string& ownerName);
void renderStringProperty(properties::Property* prop, const std::string& ownerName);
void renderIntProperty(properties::Property* prop, const std::string& ownerName);
void renderIVec2Property(properties::Property* prop, const std::string& ownerName);
void renderIVec3Property(properties::Property* prop, const std::string& ownerName);
void renderIVec4Property(properties::Property* prop, const std::string& ownerName);
void renderFloatProperty(properties::Property* prop, const std::string& ownerName);
void renderVec2Property(properties::Property* prop, const std::string& ownerName);
void renderVec3Property(properties::Property* prop, const std::string& ownerName);
void renderVec4Property(properties::Property* prop, const std::string& ownerName);
void renderTriggerProperty(properties::Property* prop, const std::string& ownerName);

} // namespace openspace

#endif __RENDERPROPERTIES_H__
