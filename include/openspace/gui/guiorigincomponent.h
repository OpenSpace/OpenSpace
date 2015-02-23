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

#ifndef __GUIORIGINCOMPONENT_H__
#define __GUIORIGINCOMPONENT_H__

#include <openspace/gui/guicomponent.h>

namespace openspace {

namespace gui {

class GuiOriginComponent : public GuiComponent {
public:
    void render() override;
};

//
//class GuiPropertyComponent : public GuiComponent {
//public:
//	//void registerProperty(const std::string& propertyDescription);
//    void registerProperty(properties::Property* prop);
//	void render();
//
//protected:
//	enum class PropertyType {
//		BoolProperty = 0,
//		IntProperty,
//		FloatProperty,
//		Vec2Property,
//		Vec3Property,
//		StringProperty,
//		OptionProperty,
//		SelectionProperty,
//		TriggerProperty,
//		InvalidPropertyType
//	};
//
//	struct PropertyInfo {
//		PropertyType type;
//		std::string identifier;
//		std::string name;
//		std::string group;
//	};
//	typedef std::string PropertyOwner;
//
//	struct Property {
//		PropertyOwner owner;
//		std::vector<PropertyInfo> properties;
//	};
//
//	void handleProperty(const ghoul::Dictionary& value);
//
//	PropertyType toPropertyType(const std::string& name) const;
//
//	void renderProperty(const PropertyInfo& info) const;
//
//    std::set<properties::Property*> _boolProperties;
//    std::set<properties::Property*> _intProperties;
//    std::set<properties::Property*> _floatProperties;
//    std::set<properties::Property*> _vec2Properties;
//    std::set<properties::Property*> _vec3Properties;
//    std::set<properties::Property*> _vec4Properties;
//    std::set<properties::Property*> _stringProperties;
//    std::set<properties::Property*> _optionProperties;
//    std::set<properties::Property*> _selectionProperties;
//    std::set<properties::Property*> _triggerProperties;
//    std::map<std::string, std::vector<properties::Property*>> _propertiesByOwner;
//
//	//std::vector<Property> _properties;
//};

} // namespace gui
} // namespace openspace

#endif // __GUIORIGINCOMPONENT_H__
