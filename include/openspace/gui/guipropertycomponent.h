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

#ifndef __GUIPROPERTYCOMPONENT_H__
#define __GUIPROPERTYCOMPONENT_H__

#include <openspace/gui/guicomponent.h>

#include <map>
#include <set>
#include <vector>

namespace openspace {

namespace properties {
	class Property;
}

namespace gui {

class GuiPropertyComponent : public GuiComponent {
public:
	void registerProperty(properties::Property* prop);
	void render();

protected:
	std::set<properties::Property*> _boolProperties;
	std::set<properties::Property*> _intProperties;
	std::set<properties::Property*> _floatProperties;
	std::set<properties::Property*> _vec2Properties;
	std::set<properties::Property*> _vec3Properties;
	std::set<properties::Property*> _stringProperties;
	std::set<properties::Property*> _optionProperty;
	std::set<properties::Property*> _selectionProperty;
	std::set<properties::Property*> _triggerProperty;

	std::map<std::string, std::vector<properties::Property*>> _propertiesByOwner;
};

} // namespace gui
} // namespace openspace

#endif // __GUIPROPERTYCOMPONENT_H__
