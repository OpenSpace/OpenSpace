/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_IMGUI___RENDERPROPERTIES___H__
#define __OPENSPACE_MODULE_IMGUI___RENDERPROPERTIES___H__

#include <ghoul/misc/boolean.h>
#include <string>

namespace openspace {

class Property;

BooleanType(ShowToolTip);

void renderBoolProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderOptionProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderSelectionProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderStringProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderIntListProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderDoubleListProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderStringListProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderDoubleProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderIntProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderIVec2Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderIVec3Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderIVec4Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderFloatProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderVec2Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderVec3Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderVec4Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderDVec2Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderDVec3Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderDVec4Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderDMat2Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderDMat3Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderDMat4Property(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

void renderTriggerProperty(Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, float tooltipDelay = 1.f);

} // namespace openspace

#endif // __OPENSPACE_MODULE_IMGUI___RENDERPROPERTIES___H__
