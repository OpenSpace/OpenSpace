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

#include <openspace/gui/guitimecomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/time.h>

#include <ghoul/misc/assert.h>
#include "imgui.h"

namespace {
	const std::string _loggerCat = "GuiTimeComponent";
}

namespace openspace {
namespace gui {

void GuiTimeComponent::render() {
    float deltaTime = Time::ref().deltaTime();
    

    bool changed = ImGui::SliderFloat("Delta Time", &deltaTime, -100.f, 100.f);
    if (changed)
        OsEng.scriptEngine()->queueScript("openspace.time.setDeltaTime(" + std::to_string(deltaTime) + ")");


    //char dateBuffer[512] = {};
    //ImGui::InputText("Date", dateBuffer, 512);
    //bool pressed = ImGui::Button("Set Date");
    //if (pressed)
    //    OsEng.scriptEngine()->queueScript("openspace.time.setTime('" + std::string(dateBuffer) + "')");

    //const SceneGraphNode* currentFocus = OsEng.interactionHandler()->focusNode();

    //std::vector<SceneGraphNode*> nodes = OsEng.renderEngine()->sceneGraph()->allSceneGraphNodes();
    //std::sort(nodes.begin(), nodes.end(), [](SceneGraphNode* lhs, SceneGraphNode* rhs) { return lhs->name() < rhs->name(); });
    //auto it = std::find(nodes.begin(), nodes.end(), currentFocus);
    //ghoul_assert(it != nodes.end(), "Focus node not found");

    //std::string nodeNames = "";
    //for (SceneGraphNode* n : nodes) 
    //    nodeNames += n->name() + '\0';


    //int position = it - nodes.begin();

    //bool result = ImGui::Combo("Origin", &position, nodeNames.c_str());

    //if (result) {
    //    LINFO("openspace.setOrigin('" + nodes[position]->name() + "');");
    //    OsEng.scriptEngine()->queueScript("openspace.setOrigin('" + nodes[position]->name() + "');");
    //}

}

} // gui
} // openspace
