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

#include <modules/onscreengui/include/guiorigincomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/interaction/interactionhandler.h>
#include <ghoul/misc/assert.h>
#include "imgui.h"

namespace {
	const std::string _loggerCat = "GuiOriginComponent";
}

namespace openspace {
namespace gui {

void GuiOriginComponent::render() {
    const SceneGraphNode* currentFocus = OsEng.interactionHandler()->focusNode();

    std::vector<SceneGraphNode*> nodes = OsEng.renderEngine()->scene()->allSceneGraphNodes();
    std::sort(nodes.begin(), nodes.end(), [](SceneGraphNode* lhs, SceneGraphNode* rhs) { return lhs->name() < rhs->name(); });
    auto it = std::find(nodes.begin(), nodes.end(), currentFocus);
    ghoul_assert(it != nodes.end(), "Focus node not found");

    std::string nodeNames = "";
    for (SceneGraphNode* n : nodes) 
        nodeNames += n->name() + '\0';


    int position = static_cast<int>(std::distance(it, nodes.begin()));

    bool result = ImGui::Combo("Origin", &position, nodeNames.c_str());

    if (result) {
        LINFO("openspace.setOrigin('" + nodes[position]->name() + "');");
        OsEng.scriptEngine()->queueScript("openspace.setOrigin('" + nodes[position]->name() + "');");
    }

}

} // gui
} // openspace

    
 //   ImGui::Begin("Properties", &_isEnabled, size, 0.5f);


 //   //ImGui::ShowUserGuide();
 //   ImGui::Spacing();

 //   for (const auto& p : _propertiesByOwner) {
 //       if (ImGui::CollapsingHeader(p.first.c_str())) {
 //           for (properties::Property* prop : p.second) {
 //               if (_boolProperties.find(prop) != _boolProperties.end()) {
 //                   renderBoolProperty(prop, p.first);
 //                   continue;
 //               }

 //               if (_intProperties.find(prop) != _intProperties.end()) {
 //                   renderIntProperty(prop, p.first);
 //                   continue;
 //               }

 //               if (_floatProperties.find(prop) != _floatProperties.end()) {
 //                   renderFloatProperty(prop, p.first);
 //                   continue;
 //               }

 //               if (_vec2Properties.find(prop) != _vec2Properties.end()) {
 //                   renderVec2Property(prop, p.first);
 //                   continue;
 //               }

 //               if (_vec3Properties.find(prop) != _vec3Properties.end()) {
 //                   renderVec3Property(prop, p.first);
 //                   continue;
 //               }

 //               if (_vec4Properties.find(prop) != _vec4Properties.end()) {
 //                   renderVec4Property(prop, p.first);
 //                   continue;
 //               }

 //               if (_optionProperties.find(prop) != _optionProperties.end()) {
 //                   renderOptionProperty(prop, p.first);
 //                   continue;
 //               }

 //               if (_triggerProperties.find(prop) != _triggerProperties.end()) {
 //                   renderTriggerProperty(prop, p.first);
 //                   continue;
 //               }

 //               if (_selectionProperties.find(prop) != _selectionProperties.end()) {
 //                   renderSelectionProperty(prop, p.first);
 //                   continue;
 //               }

 //               if (_stringProperties.find(prop) != _stringProperties.end()) {
 //                   renderStringProperty(prop, p.first);
 //                   continue;
 //               }
 //           }
 //       }
 //   }

	//ImGui::End();
//}
//
//GuiPropertyComponent::PropertyType GuiPropertyComponent::toPropertyType(
//	const std::string& name) const
//{
//	if (name == "BoolProperty")
//		return PropertyType::BoolProperty;
//	if (name == "IntProperty")
//		return PropertyType::IntProperty;
//	if (name == "FloatProperty")
//		return PropertyType::FloatProperty;
//	if (name == "Vec2Property")
//		return PropertyType::Vec2Property;
//	if (name == "Vec3Property")
//		return PropertyType::Vec3Property;
//	if (name == "StringProperty")
//		return PropertyType::StringProperty;
//	if (name == "OptionProperty")
//		return PropertyType::OptionProperty;
//	if (name == "SelectionProperty")
//		return PropertyType::SelectionProperty;
//	if (name == "TriggerProperty")
//		return PropertyType::TriggerProperty;
//
//	LWARNING("Unsupported property type '" << name << "'");
//	return PropertyType::InvalidPropertyType;
//}
//
//void GuiPropertyComponent::renderProperty(const PropertyInfo& info) const {
//	switch (info.type) {
//		case PropertyType::BoolProperty:
//		{
//			//	BoolProperty* p = static_cast<BoolProperty*>(prop);
//			//	std::string name = p->guiName();
//
//			//	BoolProperty::ValueType value = *p;
//			//	ImGui::Checkbox((ownerName + "." + name).c_str(), &value);
//			//	p->set(value);
//
//
//			
//
//			break;
//		}
//		default:
//            LERROR("Missing case statement: {" << int(info.type) << "," << info.identifier << "}");
//			//ghoul_assert(false, "Missing case statement");
//		}
//
//}

