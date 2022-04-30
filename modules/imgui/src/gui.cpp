/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
//
//#include <modules/imgui/include/gui.h>
//
//
//
//namespace {
//
//
//    constexpr openspace::properties::Property::PropertyInfo ShowHelpInfo = {
//        "ShowHelpText",
//        "Show tooltip help",
//        "If this value is enabled these kinds of tooltips are shown for most properties "
//        "explaining what impact they have on the visuals."
//    };
//
//    constexpr openspace::properties::Property::PropertyInfo HelpTextDelayInfo = {
//        "HelpTextDelay",
//        "Tooltip Delay (in s)",
//        "This value determines the delay in seconds after which the tooltip is shown."
//    };
//} // namespace
//
//namespace openspace::gui {
//
//void CaptionText(const char* text) {
//    ImGui::PushFont(captionFont);
//    ImGui::Text("%s", text);
//    ImGui::PopFont();
//}
//
//GUI::GUI()
//    : GuiComponent("Main")
//    , _sceneProperty("SceneProperties", "Scene", GuiPropertyComponent::UseTreeLayout::Yes)
//    , _property("Property", "Settings")
//    , _showHelpText(ShowHelpInfo, true)
//    , _helpTextDelay(HelpTextDelayInfo, 1.0, 0.0, 10.0)
//{
//    for (GuiComponent* comp : _components) {
//        addPropertySubOwner(comp);
//    }
//    _spaceTime.setEnabled(true);
//
//    {
//        auto showHelpTextFunc = [this]() {
//            for (GuiComponent* comp : _components) {
//                comp->setShowHelpTooltip(_showHelpText);
//            }
//        };
//        showHelpTextFunc();
//        _showHelpText.onChange(std::move(showHelpTextFunc));
//        addProperty(_showHelpText);
//    }
//
//    {
//        auto helpTextDelayFunc = [this](){
//            for (GuiComponent* comp : _components) {
//                comp->setShowHelpTooltipDelay(_helpTextDelay);
//            }
//        };
//        helpTextDelayFunc();
//        _helpTextDelay.onChange(std::move(helpTextDelayFunc));
//        addProperty(_helpTextDelay);
//    }
//}
//
//GUI::~GUI() {} // NOLINT
//
//} // namespace openspace::gui
