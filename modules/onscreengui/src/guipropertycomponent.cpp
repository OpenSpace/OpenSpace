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

#include <modules/onscreengui/include/guipropertycomponent.h>
#include <modules/onscreengui/include/renderproperties.h>

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
}

namespace openspace {
namespace gui {

//void GuiPropertyComponent::registerProperty(properties::Property* prop) {
    //registerProperty(prop->description());
    //using namespace properties;

    //std::string className = prop->className();

    //if (className == "BoolProperty")
    //    _boolProperties.insert(prop);
    //else if (className == "IntProperty")
    //    _intProperties.insert(prop);
    //else if (className == "FloatProperty")
    //    _floatProperties.insert(prop);
    //else if (className == "StringProperty")
    //    _stringProperties.insert(prop);
    //else if (className == "Vec2Property")
    //    _vec2Properties.insert(prop);
    //else if (className == "Vec3Property")
    //    _vec3Properties.insert(prop);
    //else if (className == "OptionProperty")
    //    _optionProperty.insert(prop);
    //else if (className == "TriggerProperty")
    //    _triggerProperty.insert(prop);
    //else if (className == "SelectionProperty")
    //    _selectionProperty.insert(prop);
    //else {
    //    LWARNING("Class name '" << className << "' not handled in GUI generation");
    //    return;
    //}

    //std::string fullyQualifiedId = prop->fullyQualifiedIdentifier();
    //size_t pos = fullyQualifiedId.find('.');
    //std::string owner = fullyQualifiedId.substr(0, pos);

    //auto it = _propertiesByOwner.find(owner);
    //if (it == _propertiesByOwner.end())
    //    _propertiesByOwner[owner] = { prop };
    //else
    //    it->second.push_back(prop);

//}

void GuiPropertyComponent::registerProperty(properties::Property* prop, properties::Property* sibling) {
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
    if (it == _propertiesByOwner.end()){
        _propertiesByOwner[owner] = { prop };
    } else {
        std::vector<properties::Property*>::iterator position = std::find(it->second.begin(), it->second.end(), sibling);
        if (position != it->second.end()){
            it->second.insert(++position, prop);       
        } else {
            it->second.push_back(prop);
        }
    }

    //ghoul::Dictionary dictionary;
    //ghoul::lua::loadDictionaryFromString(propertyDescription, dictionary);

    //handleProperty(dictionary);
}

void GuiPropertyComponent::unregisterProperty(properties::Property* prop) {

    using namespace properties;

    std::string className = prop->className();

    if (className == "BoolProperty")
        _boolProperties.erase(prop);
    else if (className == "IntProperty")
        _intProperties.erase(prop);
    else if (className == "FloatProperty")
        _floatProperties.erase(prop);
    else if (className == "StringProperty")
        _stringProperties.erase(prop);
    else if (className == "Vec2Property")
        _vec2Properties.erase(prop);
    else if (className == "Vec3Property")
        _vec3Properties.erase(prop);
    else if (className == "Vec4Property")
        _vec4Properties.erase(prop);
    else if (className == "OptionProperty")
        _optionProperties.erase(prop);
    else if (className == "TriggerProperty")
        _triggerProperties.erase(prop);
    else if (className == "SelectionProperty")
        _selectionProperties.erase(prop);
    else {
        LWARNING("Class name '" << className << "' not handled in GUI generation");
        return;
    }

    std::string fullyQualifiedId = prop->fullyQualifiedIdentifier();
    size_t pos = fullyQualifiedId.find('.');
    std::string owner = fullyQualifiedId.substr(0, pos);

    auto it = _propertiesByOwner.find(owner);
    if (it == _propertiesByOwner.end()){
        LWARNING("Cannot find owner for " + className);
    }
    else{
        std::vector<properties::Property*>::iterator position = std::find(it->second.begin(), it->second.end(), prop);
        if (position != it->second.end())
            it->second.erase(position);
    }
}

void GuiPropertyComponent::unregisterProperties(std::string owner){
    auto it = _propertiesByOwner.find(owner);
    if(it != _propertiesByOwner.end()){
        for(auto prop : it->second){
            std::string className = prop->className();
            if (className == "BoolProperty")
                _boolProperties.erase(prop);
            else if (className == "IntProperty")
                _intProperties.erase(prop);
            else if (className == "FloatProperty")
                _floatProperties.erase(prop);
            else if (className == "StringProperty")
                _stringProperties.erase(prop);
            else if (className == "Vec2Property")
                _vec2Properties.erase(prop);
            else if (className == "Vec3Property")
                _vec3Properties.erase(prop);
            else if (className == "Vec4Property")
                _vec4Properties.erase(prop);
            else if (className == "OptionProperty")
                _optionProperties.erase(prop);
            else if (className == "TriggerProperty")
                _triggerProperties.erase(prop);
            else if (className == "SelectionProperty")
                _selectionProperties.erase(prop);
        }
        it->second.clear();
        _propertiesByOwner.erase(it);
    }
}

void GuiPropertyComponent::handleProperty(const ghoul::Dictionary& dictionary) {
    //static const std::string TypeKey = "Type";
    //static const std::string IdentifierKey = "Identifier";
    //static const std::string NameKey = "Name";
    //static const std::string GroupKey = "MetaData.Group";

    //ghoul_assert(
    //    dictionary.hasKeyAndValue<std::string>(TypeKey), "Missing Type key"
    //);
    //ghoul_assert(
    //    dictionary.hasKeyAndValue<std::string>(IdentifierKey), "Missing Identifier key"
    //);
    //ghoul_assert(
    //    dictionary.hasKeyAndValue<std::string>(NameKey), "Missing Name key"
    //);
    //ghoul_assert(
    //    dictionary.hasKeyAndValue<std::string>(GroupKey), "Missing Group key"
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
    //    [owner](const Property& prop) {
    //        return prop.owner == owner;
    //});
    //if (it == _properties.end()) {
    //    Property p;
    //    p.owner = owner;
    //    p.properties = {info};
    //    _properties.push_back(p);
    //}
    //else
    //    it->properties.push_back(std::move(info));
}

void GuiPropertyComponent::render() {
    ImGui::Begin("Properties", &_isEnabled, size, 0.5f);

    // if (ImGui::CollapsingHeader("OnScreen GUI")) {
    //     glm::vec2& pos = OsEng.renderEngine()._onScreenInformation._position;
    //     Vec2Property::ValueType value = pos;
    //     ImGui::SliderFloat2("Position", &value.x, -1.f, 1.f);
    //     pos = value;
     
    //     unsigned int& size = OsEng.renderEngine()._onScreenInformation._size;
    //     int sizeValue = static_cast<int>(size);
    //     ImGui::SliderInt("Size", &sizeValue, 0, 36);
    //     size = static_cast<unsigned int>(sizeValue);

    //     int& node = OsEng.renderEngine()._onScreenInformation._node;
    //     int iValue = node;
    //     ImGui::SliderInt("Node#", &iValue, 0, 30);
    //     node = iValue;
    // }

    ImGui::Spacing();

    for (const auto& p : _propertiesByOwner) {
        if (ImGui::CollapsingHeader(p.first.c_str())) {
            for (properties::Property* prop : p.second) {
                if (_boolProperties.find(prop) != _boolProperties.end()) {
                    renderBoolProperty(prop);
                    continue;
                }

                if (_intProperties.find(prop) != _intProperties.end()) {
                    renderIntProperty(prop);
                    continue;
                }

                if (_floatProperties.find(prop) != _floatProperties.end()) {
                    renderFloatProperty(prop);
                    continue;
                }

                if (_vec2Properties.find(prop) != _vec2Properties.end()) {
                    renderVec2Property(prop);
                    continue;
                }

                if (_vec3Properties.find(prop) != _vec3Properties.end()) {
                    renderVec3Property(prop);
                    continue;
                }

                if (_vec4Properties.find(prop) != _vec4Properties.end()) {
                    renderVec4Property(prop);
                    continue;
                }

                if (_optionProperties.find(prop) != _optionProperties.end()) {
                    renderOptionProperty(prop);
                    continue;
                }

                if (_triggerProperties.find(prop) != _triggerProperties.end()) {
                    renderTriggerProperty(prop);
                    continue;
                }

                if (_selectionProperties.find(prop) != _selectionProperties.end()) {
                    renderSelectionProperty(prop);
                    continue;
                }

                if (_stringProperties.find(prop) != _stringProperties.end()) {
                    renderStringProperty(prop);
                    continue;
                }
            }
        }
    }

    //for (const Property& prop : _properties) {
    //    if (ImGui::CollapsingHeader(prop.owner.c_str())) {
    //        for (const PropertyInfo& info : prop.properties) {
    //            renderProperty(info);
    //        }
    //    }
 //    }



    //for (const auto& p : _propertiesByOwner) {
    //    if (ImGui::CollapsingHeader(p.first.c_str())) {
    //        for (properties::Property* prop : p.second) {
    //            if (_boolProperties.find(prop) != _boolProperties.end()) {
    //                renderBoolProperty(prop, p.first);
    //                continue;
    //            }

    //            if (_intProperties.find(prop) != _intProperties.end()) {
    //                renderIntProperty(prop, p.first);
    //                continue;
    //            }

    //            if (_floatProperties.find(prop) != _floatProperties.end()) {
    //                renderFloatProperty(prop, p.first);
    //                continue;
    //            }

    //            if (_vec2Properties.find(prop) != _vec2Properties.end()) {
    //                renderVec2Property(prop, p.first);
    //                continue;
    //            }

    //            if (_vec3Properties.find(prop) != _vec3Properties.end()) {
    //                renderVec3Property(prop, p.first);
    //                continue;
    //            }

    //            if (_optionProperty.find(prop) != _optionProperty.end()) {
    //                renderOptionProperty(prop, p.first);
    //                continue;
    //            }

    //            if (_triggerProperty.find(prop) != _triggerProperty.end()) {
    //                renderTriggerProperty(prop, p.first);
    //                continue;
    //            }

    //            if (_selectionProperty.find(prop) != _selectionProperty.end()) {
    //                renderSelectionProperty(prop, p.first);
    //                continue;
    //            }
    //        }
    //    }
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
            //    BoolProperty* p = static_cast<BoolProperty*>(prop);
            //    std::string name = p->guiName();

            //    BoolProperty::ValueType value = *p;
            //    ImGui::Checkbox((ownerName + "." + name).c_str(), &value);
            //    p->set(value);


            

            break;
        }
        default:
            LERROR("Missing case statement: {" << int(info.type) << "," << info.identifier << "}");
            //ghoul_assert(false, "Missing case statement");
        }

}

} // gui
} // openspace
