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

#include <modules/onscreengui/include/guiorigincomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/misc/assert.h>

#include "imgui.h"

namespace openspace {
namespace gui {

GuiOriginComponent::GuiOriginComponent()
    : GuiComponent("Origin")
{}

void GuiOriginComponent::render() {
    SceneGraphNode* currentFocus = OsEng.interactionHandler().focusNode();

    std::vector<SceneGraphNode*> nodes =
        OsEng.renderEngine().scene()->allSceneGraphNodes();

    std::sort(
        nodes.begin(),
        nodes.end(),
        [](SceneGraphNode* lhs, SceneGraphNode* rhs) {
            return lhs->name() < rhs->name();
        }
    );
    std::string nodeNames = "";
    for (SceneGraphNode* n : nodes) {
        nodeNames += n->name() + '\0';
    }

    auto iCurrentFocus = std::find(nodes.begin(), nodes.end(), currentFocus);
    ghoul_assert(iCurrentFocus != nodes.end(), "Focus node not found");
    int currentPosition = static_cast<int>(std::distance(iCurrentFocus, nodes.begin()));

    bool hasChanged = ImGui::Combo("Origin", &currentPosition, nodeNames.c_str());
    if (hasChanged) {
        OsEng.scriptEngine().queueScript(
            "openspace.setPropertyValue('Interaction.origin', '" +
            nodes[currentPosition]->name() + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
}

} // gui
} // openspace
