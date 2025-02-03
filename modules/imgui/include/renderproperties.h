/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

namespace openspace::properties { class Property; }

namespace openspace {

BooleanType(ShowToolTip);

void executeSetPropertyScript(const std::string& id, const std::string& value);

void renderBoolProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderOptionProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderSelectionProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderStringProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderIntListProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderDoubleListProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderStringListProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderDoubleProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderIntProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderIVec2Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderIVec3Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderIVec4Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderFloatProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderVec2Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderVec3Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderVec4Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderDVec2Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderDVec3Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderDVec4Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderDMat2Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderDMat3Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderDMat4Property(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

void renderTriggerProperty(properties::Property* prop, const std::string& ownerName,
    ShowToolTip showTooltip = ShowToolTip::Yes, double tooltipDelay = 1.0);

} // namespace openspace

#endif // __OPENSPACE_MODULE_IMGUI___RENDERPROPERTIES___H__
