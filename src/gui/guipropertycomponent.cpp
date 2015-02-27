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

#include <openspace/gui/guipropertycomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>

#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vectorproperty.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>
#include "imgui.h"

namespace {
	const std::string _loggerCat = "GuiPropertyComponent";
	const ImVec2 size = ImVec2(350, 500);

	using namespace openspace::properties;

    void executeScript(const std::string& id, const std::string& value) {
        std::string script =
            "openspace.setPropertyValue('" + id + "', " + value + ");";
        OsEng.scriptEngine()->queueScript(script);
    }

    void renderBoolProperty(Property* prop, const std::string& ownerName) {
        BoolProperty* p = static_cast<BoolProperty*>(prop);
        std::string name = p->guiName();

        BoolProperty::ValueType value = *p;
        ImGui::Checkbox((ownerName + "." + name).c_str(), &value);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), value ? "true": "false");
    }

    void renderOptionProperty(Property* prop, const std::string& ownerName) {
        OptionProperty* p = static_cast<OptionProperty*>(prop);
        std::string name = p->guiName();

        int value = *p;
        std::vector<OptionProperty::Option> options = p->options();
        for (const OptionProperty::Option& o : options) {
            ImGui::RadioButton((ownerName + "." + name).c_str(), &value, o.value);
            ImGui::SameLine();
            ImGui::Text(o.description.c_str());
        }
        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
    }

    void renderSelectionProperty(Property* prop, const std::string& ownerName) {
        SelectionProperty* p = static_cast<SelectionProperty*>(prop);
        std::string name = p->guiName();

        if (ImGui::CollapsingHeader((ownerName + "." + name).c_str())) {
            const std::vector<SelectionProperty::Option>& options = p->options();
            std::vector<int> newSelectedIndices;

            std::vector<int> selectedIndices = p->value();

            for (int i = 0; i < options.size(); ++i) {
                std::string description = options[i].description;
                bool selected = std::find(selectedIndices.begin(), selectedIndices.end(), i) != selectedIndices.end();
                ImGui::Checkbox(description.c_str(), &selected);

                if (selected)
                    newSelectedIndices.push_back(i);
            }

            if (newSelectedIndices != p->value()) {
                std::string parameters = "{";
                for (int i : newSelectedIndices)
                    parameters += std::to_string(i) + ",";
                parameters += "}";
                executeScript(p->fullyQualifiedIdentifier(), parameters);
            }
        }
    }

    void renderStringProperty(Property* prop, const std::string& ownerName) {
        StringProperty* p = static_cast<StringProperty*>(prop);
        std::string name = p->guiName();

        static const int bufferSize = 256;
        static char buffer[bufferSize];
        strcpy(buffer, p->value().c_str());
        ImGui::InputText((ownerName + "." + name).c_str(), buffer, bufferSize);
        std::string newValue(buffer);

        if (newValue != p->value() && FileSys.fileExists(newValue))
            executeScript(p->fullyQualifiedIdentifier(), "'" + newValue + "'");
    }

    void renderIntProperty(Property* prop, const std::string& ownerName) {
        IntProperty* p = static_cast<IntProperty*>(prop);
        std::string name = p->guiName();

        IntProperty::ValueType value = *p;
        ImGui::SliderInt((ownerName + "." + name).c_str(), &value, p->minValue(), p->maxValue());

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));
    }

    void renderFloatProperty(Property* prop, const std::string& ownerName) {
        FloatProperty* p = static_cast<FloatProperty*>(prop);
        std::string name = p->guiName();

        FloatProperty::ValueType value = *p;
        ImGui::SliderFloat((ownerName + "." + name).c_str(), &value, p->minValue(), p->maxValue());

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(), std::to_string(value));

    }

    void renderVec2Property(Property* prop, const std::string& ownerName) {
        Vec2Property* p = static_cast<Vec2Property*>(prop);
        std::string name = p->guiName();

        Vec2Property::ValueType value = *p;

        ImGui::SliderFloat2((ownerName + "." + name).c_str(), &value.x, std::min(p->minValue().x, p->minValue().y), std::max(p->maxValue().x, p->maxValue().y));

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," + std::to_string(value.y) + "}");
    }

    void renderVec3Property(Property* prop, const std::string& ownerName) {
        Vec3Property* p = static_cast<Vec3Property*>(prop);
        std::string name = p->guiName();

        Vec3Property::ValueType value = *p;

        ImGui::SliderFloat3((ownerName + "." + name).c_str(), &value.x, p->minValue().x, p->maxValue().x);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
                  std::to_string(value.y) + "," +
                  std::to_string(value.z) + "}");
    }

    void renderVec4Property(Property* prop, const std::string& ownerName) {
        Vec4Property* p = static_cast<Vec4Property*>(prop);
        std::string name = p->guiName();

        Vec4Property::ValueType value = *p;

        ImGui::SliderFloat4((ownerName + "." + name).c_str(), &value.x, p->minValue().x, p->maxValue().x);

        if (value != p->value())
            executeScript(p->fullyQualifiedIdentifier(),
            "{" + std::to_string(value.x) + "," +
                  std::to_string(value.y) + "," +
                  std::to_string(value.z) + "," +
                  std::to_string(value.w) + "}");
    }

    void renderTriggerProperty(Property* prop, const std::string& ownerName) {
        std::string name = prop->guiName();
        bool pressed = ImGui::Button((ownerName + "." + name).c_str());
        if (pressed)
            executeScript(prop->fullyQualifiedIdentifier(), "0");
    }

	//void renderBoolProperty(Property* prop, const std::string& ownerName) {
	//	BoolProperty* p = static_cast<BoolProperty*>(prop);
	//	std::string name = p->guiName();

	//	BoolProperty::ValueType value = *p;
	//	ImGui::Checkbox((ownerName + "." + name).c_str(), &value);
	//	p->set(value);
	//}

	//void renderOptionProperty(Property* prop, const std::string& ownerName) {
	//	OptionProperty* p = static_cast<OptionProperty*>(prop);
	//	std::string name = p->guiName();

	//	int value = *p;
	//	std::vector<OptionProperty::Option> options = p->options();
	//	for (const OptionProperty::Option& o : options) {
	//		ImGui::RadioButton((ownerName + "." + name).c_str(), &value, o.value);
	//		ImGui::SameLine();
	//		ImGui::Text(o.description.c_str());
	//	}
	//	p->set(value);
	//}

	//void renderSelectionProperty(Property* prop, const std::string& ownerName) {
	//	SelectionProperty* p = static_cast<SelectionProperty*>(prop);
	//	std::string name = p->guiName();

	//	if (ImGui::CollapsingHeader((ownerName + "." + name).c_str())) {
	//		const std::vector<SelectionProperty::Option>& options = p->options();
	//		std::vector<int> newSelectedIndices;

	//		std::vector<int> selectedIndices = p->value();

	//		for (int i = 0; i < options.size(); ++i) {
	//			std::string description = options[i].description;
	//			bool selected = std::find(selectedIndices.begin(), selectedIndices.end(), i) != selectedIndices.end();
	//			ImGui::Checkbox(description.c_str(), &selected);

	//			if (selected)
	//				newSelectedIndices.push_back(i);
	//		}

	//		p->setValue(std::move(newSelectedIndices));
	//	}
	//}

	//void renderIntProperty(Property* prop, const std::string& ownerName) {
	//	IntProperty* p = static_cast<IntProperty*>(prop);
	//	std::string name = p->guiName();

	//	IntProperty::ValueType value = *p;
	//	ImGui::SliderInt((ownerName + "." + name).c_str(), &value, p->minValue(), p->maxValue());
	//	p->set(value);
	//}

	//void renderFloatProperty(Property* prop, const std::string& ownerName) {
	//	FloatProperty* p = static_cast<FloatProperty*>(prop);
	//	std::string name = p->guiName();

	//	FloatProperty::ValueType value = *p;
	//	ImGui::SliderFloat((ownerName + "." + name).c_str(), &value, p->minValue(), p->maxValue());
	//	p->set(value);
	//}

	//void renderVec2Property(Property* prop, const std::string& ownerName) {
	//	Vec2Property* p = static_cast<Vec2Property*>(prop);
	//	std::string name = p->guiName();

	//	Vec2Property::ValueType value = *p;

	//	ImGui::SliderFloat2((ownerName + "." + name).c_str(), &value.x, p->minValue().x, p->maxValue().x);
	//	p->set(value);
	//}


	//void renderVec3Property(Property* prop, const std::string& ownerName) {
	//	Vec3Property* p = static_cast<Vec3Property*>(prop);
	//	std::string name = p->guiName();

	//	Vec3Property::ValueType value = *p;

	//	ImGui::SliderFloat3((ownerName + "." + name).c_str(), &value.x, p->minValue().x, p->maxValue().x);
	//	p->set(value);
	//}

	//void renderTriggerProperty(Property* prop, const std::string& ownerName) {
	//	std::string name = prop->guiName();
	//	bool pressed = ImGui::Button((ownerName + "." + name).c_str());
	//	if (pressed)
	//		prop->set(0);
	//}

}

namespace openspace {
namespace gui {

//void GuiPropertyComponent::registerProperty(properties::Property* prop) {
	//registerProperty(prop->description());
	//using namespace properties;

	//std::string className = prop->className();

	//if (className == "BoolProperty")
	//	_boolProperties.insert(prop);
	//else if (className == "IntProperty")
	//	_intProperties.insert(prop);
	//else if (className == "FloatProperty")
	//	_floatProperties.insert(prop);
	//else if (className == "StringProperty")
	//	_stringProperties.insert(prop);
	//else if (className == "Vec2Property")
	//	_vec2Properties.insert(prop);
	//else if (className == "Vec3Property")
	//	_vec3Properties.insert(prop);
	//else if (className == "OptionProperty")
	//	_optionProperty.insert(prop);
	//else if (className == "TriggerProperty")
	//	_triggerProperty.insert(prop);
	//else if (className == "SelectionProperty")
	//	_selectionProperty.insert(prop);
	//else {
	//	LWARNING("Class name '" << className << "' not handled in GUI generation");
	//	return;
	//}

	//std::string fullyQualifiedId = prop->fullyQualifiedIdentifier();
	//size_t pos = fullyQualifiedId.find('.');
	//std::string owner = fullyQualifiedId.substr(0, pos);

	//auto it = _propertiesByOwner.find(owner);
	//if (it == _propertiesByOwner.end())
	//	_propertiesByOwner[owner] = { prop };
	//else
	//	it->second.push_back(prop);

//}

void GuiPropertyComponent::registerProperty(properties::Property* prop) {
        //void GuiPropertyComponent::registerProperty(const std::string& propertyDescription) {
    using namespace properties;

    std::string className = prop->className();

    if (className == "BoolProperty")
        _boolProperties.insert(prop);
    else if (className == "IntProperty")
        _intProperties.insert(prop);
    else if (className == "FloatProperty")
        _floatProperties.insert(prop);
    else if (className == "StringProperty")
        _stringProperties.insert(prop);
    else if (className == "Vec2Property")
        _vec2Properties.insert(prop);
    else if (className == "Vec3Property")
        _vec3Properties.insert(prop);
    else if (className == "Vec4Property")
        _vec4Properties.insert(prop);
    else if (className == "OptionProperty")
        _optionProperties.insert(prop);
    else if (className == "TriggerProperty")
        _triggerProperties.insert(prop);
    else if (className == "SelectionProperty")
        _selectionProperties.insert(prop);
    else {
        LWARNING("Class name '" << className << "' not handled in GUI generation");
        return;
    }

    std::string fullyQualifiedId = prop->fullyQualifiedIdentifier();
    size_t pos = fullyQualifiedId.find('.');
    std::string owner = fullyQualifiedId.substr(0, pos);

    auto it = _propertiesByOwner.find(owner);
    if (it == _propertiesByOwner.end())
        _propertiesByOwner[owner] = { prop };
    else
        it->second.push_back(prop);

	//ghoul::Dictionary dictionary;
	//ghoul::lua::loadDictionaryFromString(propertyDescription, dictionary);

	//handleProperty(dictionary);
}

void GuiPropertyComponent::handleProperty(const ghoul::Dictionary& dictionary) {
	//static const std::string TypeKey = "Type";
	//static const std::string IdentifierKey = "Identifier";
	//static const std::string NameKey = "Name";
	//static const std::string GroupKey = "MetaData.Group";

	//ghoul_assert(
	//	dictionary.hasKeyAndValue<std::string>(TypeKey), "Missing Type key"
	//);
	//ghoul_assert(
	//	dictionary.hasKeyAndValue<std::string>(IdentifierKey), "Missing Identifier key"
	//);
	//ghoul_assert(
	//	dictionary.hasKeyAndValue<std::string>(NameKey), "Missing Name key"
	//);
	//ghoul_assert(
	//	dictionary.hasKeyAndValue<std::string>(GroupKey), "Missing Group key"
	//);

	//std::string typeString = dictionary.value<std::string>(TypeKey);
	//std::string identifier = dictionary.value<std::string>(IdentifierKey);
	//std::string name = dictionary.value<std::string>(NameKey);
	//std::string group = dictionary.value<std::string>(GroupKey);

	//PropertyType type = toPropertyType(typeString);
	//
	//size_t pos = identifier.find('.');
	//std::string owner = identifier.substr(0, pos);

	//PropertyInfo info = { type, identifier, name, group };

	//auto it = std::find_if(_properties.begin(), _properties.end(),
	//	[owner](const Property& prop) {
	//		return prop.owner == owner;
	//});
	//if (it == _properties.end()) {
	//	Property p;
	//	p.owner = owner;
	//	p.properties = {info};
	//	_properties.push_back(p);
	//}
	//else
	//	it->properties.push_back(std::move(info));
}

void GuiPropertyComponent::render() {
	ImGui::Begin("Properties", &_isEnabled, size, 0.5f);

    if (ImGui::CollapsingHeader("OnScreen GUI")) {
        glm::vec2& pos = OsEng.renderEngine()->_onScreenInformation._position;
        Vec2Property::ValueType value = pos;
        ImGui::SliderFloat2("Position", &value.x, -1.f, 1.f);
        pos = value;
     
        float& size = OsEng.renderEngine()->_onScreenInformation._size;
        float fValue = size;
        ImGui::SliderFloat("Size", &fValue, 0.f, 36.f);
        size = fValue;

        int& node = OsEng.renderEngine()->_onScreenInformation._node;
        int iValue = node;
        ImGui::SliderInt("Node#", &iValue, 0, 30);
        node = iValue;
    }

    ImGui::Spacing();

    for (const auto& p : _propertiesByOwner) {
        if (ImGui::CollapsingHeader(p.first.c_str())) {
            for (properties::Property* prop : p.second) {
                if (_boolProperties.find(prop) != _boolProperties.end()) {
                    renderBoolProperty(prop, p.first);
                    continue;
                }

                if (_intProperties.find(prop) != _intProperties.end()) {
                    renderIntProperty(prop, p.first);
                    continue;
                }

                if (_floatProperties.find(prop) != _floatProperties.end()) {
                    renderFloatProperty(prop, p.first);
                    continue;
                }

                if (_vec2Properties.find(prop) != _vec2Properties.end()) {
                    renderVec2Property(prop, p.first);
                    continue;
                }

                if (_vec3Properties.find(prop) != _vec3Properties.end()) {
                    renderVec3Property(prop, p.first);
                    continue;
                }

                if (_vec4Properties.find(prop) != _vec4Properties.end()) {
                    renderVec4Property(prop, p.first);
                    continue;
                }

                if (_optionProperties.find(prop) != _optionProperties.end()) {
                    renderOptionProperty(prop, p.first);
                    continue;
                }

                if (_triggerProperties.find(prop) != _triggerProperties.end()) {
                    renderTriggerProperty(prop, p.first);
                    continue;
                }

                if (_selectionProperties.find(prop) != _selectionProperties.end()) {
                    renderSelectionProperty(prop, p.first);
                    continue;
                }

                if (_stringProperties.find(prop) != _stringProperties.end()) {
                    renderStringProperty(prop, p.first);
                    continue;
                }
            }
        }
    }

	//for (const Property& prop : _properties) {
	//	if (ImGui::CollapsingHeader(prop.owner.c_str())) {
	//		for (const PropertyInfo& info : prop.properties) {
	//			renderProperty(info);
	//		}
	//	}
 //	}



	//for (const auto& p : _propertiesByOwner) {
	//	if (ImGui::CollapsingHeader(p.first.c_str())) {
	//		for (properties::Property* prop : p.second) {
	//			if (_boolProperties.find(prop) != _boolProperties.end()) {
	//				renderBoolProperty(prop, p.first);
	//				continue;
	//			}

	//			if (_intProperties.find(prop) != _intProperties.end()) {
	//				renderIntProperty(prop, p.first);
	//				continue;
	//			}

	//			if (_floatProperties.find(prop) != _floatProperties.end()) {
	//				renderFloatProperty(prop, p.first);
	//				continue;
	//			}

	//			if (_vec2Properties.find(prop) != _vec2Properties.end()) {
	//				renderVec2Property(prop, p.first);
	//				continue;
	//			}

	//			if (_vec3Properties.find(prop) != _vec3Properties.end()) {
	//				renderVec3Property(prop, p.first);
	//				continue;
	//			}

	//			if (_optionProperty.find(prop) != _optionProperty.end()) {
	//				renderOptionProperty(prop, p.first);
	//				continue;
	//			}

	//			if (_triggerProperty.find(prop) != _triggerProperty.end()) {
	//				renderTriggerProperty(prop, p.first);
	//				continue;
	//			}

	//			if (_selectionProperty.find(prop) != _selectionProperty.end()) {
	//				renderSelectionProperty(prop, p.first);
	//				continue;
	//			}
	//		}
	//	}
	//}

	ImGui::End();
}

GuiPropertyComponent::PropertyType GuiPropertyComponent::toPropertyType(
	const std::string& name) const
{
	if (name == "BoolProperty")
		return PropertyType::BoolProperty;
	if (name == "IntProperty")
		return PropertyType::IntProperty;
	if (name == "FloatProperty")
		return PropertyType::FloatProperty;
	if (name == "Vec2Property")
		return PropertyType::Vec2Property;
	if (name == "Vec3Property")
		return PropertyType::Vec3Property;
	if (name == "StringProperty")
		return PropertyType::StringProperty;
	if (name == "OptionProperty")
		return PropertyType::OptionProperty;
	if (name == "SelectionProperty")
		return PropertyType::SelectionProperty;
	if (name == "TriggerProperty")
		return PropertyType::TriggerProperty;

	LWARNING("Unsupported property type '" << name << "'");
	return PropertyType::InvalidPropertyType;
}

void GuiPropertyComponent::renderProperty(const PropertyInfo& info) const {
	switch (info.type) {
		case PropertyType::BoolProperty:
		{
			//	BoolProperty* p = static_cast<BoolProperty*>(prop);
			//	std::string name = p->guiName();

			//	BoolProperty::ValueType value = *p;
			//	ImGui::Checkbox((ownerName + "." + name).c_str(), &value);
			//	p->set(value);


			

			break;
		}
		default:
            LERROR("Missing case statement: {" << int(info.type) << "," << info.identifier << "}");
			//ghoul_assert(false, "Missing case statement");
		}

}

} // gui
} // openspace
