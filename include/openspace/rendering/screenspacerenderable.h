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
 
#ifndef __SCREENSPACERENDERABLE_H__
#define __SCREENSPACERENDERABLE_H__
#include <ghoul/opengl/programobject.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/stringproperty.h>

namespace openspace {

class ScreenSpaceRenderable : public properties::PropertyOwner {
public:
	ScreenSpaceRenderable();
	~ScreenSpaceRenderable();

	virtual void render() = 0;
	virtual bool initialize() = 0;
	virtual bool deinitialize() = 0;
	virtual void update() = 0;
	virtual bool isReady() const = 0;
	bool isEnabled() const;

protected:
	void createPlane();

	properties::BoolProperty _enabled;
	properties::BoolProperty _flatScreen;
	properties::Vec3Property _position;
	properties::FloatProperty _scale;
	properties::StringProperty _texturePath;

	GLuint _quad;
	GLuint _vertexPositionBuffer;
	std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
};
}  // namespace openspace
#endif  // __SCREENSPACERENDERABLE_H__