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

#include <modules/onscreengui/include/guitimecomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/time.h>

#include "imgui.h"

namespace openspace {
namespace gui {

GuiTimeComponent::GuiTimeComponent()
    : GuiComponent("Time")
{}

void GuiTimeComponent::render() {
    float deltaTime = static_cast<float>(Time::ref().deltaTime());
    
    bool changed = ImGui::SliderFloat("Delta Time", &deltaTime, -5000.f, 5000.f);
    if (changed) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(deltaTime) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

} // gui
} // openspace
