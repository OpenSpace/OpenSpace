/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/imgui/include/guicomponent.h>

namespace {
    const openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this object will be visible or not."
    };

    const openspace::properties::Property::PropertyInfo CollapsedInfo = {
        "Collapsed",
        "Is Collapsed",
        "This setting determines whether this window is collapsed or not."
    };
} // namespace

namespace openspace::gui {

GuiComponent::GuiComponent(std::string identifier, std::string guiName)
    : properties::PropertyOwner({ std::move(identifier), std::move(guiName) })
    , _isEnabled(EnabledInfo, false)
    , _isCollapsed(CollapsedInfo, false)
{
    addProperty(_isEnabled);
    addProperty(_isCollapsed);
}

bool GuiComponent::isEnabled() const {
    return _isEnabled;
}

void GuiComponent::setEnabled(bool enabled) {
    _isEnabled = enabled;
}

void GuiComponent::setShowHelpTooltip(bool showHelpTooltip) {
    _showHelpTooltip = showHelpTooltip;
}

void GuiComponent::setShowHelpTooltipDelay(double delay) {
    _tooltipDelay = delay;
}

void GuiComponent::initialize() {}

void GuiComponent::initializeGL() {}

void GuiComponent::deinitialize() {}

void GuiComponent::deinitializeGL() {}

} // namespace openspace::gui
