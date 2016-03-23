/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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

#ifndef __DATASURFACE_H__
#define __DATASURFACE_H__
#include <openspace/properties/propertyowner.h>
#include <memory>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/scene/scenegraphnode.h>

namespace openspace{
class DataSurface : public properties::PropertyOwner{
public:
	DataSurface(std::shared_ptr<KameleonWrapper> kw, std::string path);
	~DataSurface();

	virtual bool initialize();
	virtual bool deinitialize();

	virtual void render();
	// virtual void update();

protected:
	void setPscUniforms(ghoul::opengl::ProgramObject* program, const Camera* camera, const PowerScaledCoordinate& position);

	std::shared_ptr<KameleonWrapper> _kw;
	properties::StringProperty _path;
	std::unique_ptr<ghoul::opengl::ProgramObject> _shader;

	glm::vec4 _pscOffset;
	glm::vec4 _modelScale;
	SceneGraphNode* _parent;
};

}//namespace openspace
#endif