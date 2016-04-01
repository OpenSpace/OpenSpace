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

#ifndef __ISWACYGNET_H__
#define __ISWACYGNET_H__

#define _USE_MATH_DEFINES
#include <math.h>

#include <openspace/properties/propertyowner.h>
#include <memory>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/scene/scenegraphnode.h>
#include <modules/onscreengui/include/gui.h>
#include <ghoul/opengl/texture.h>
#include <modules/iswa/util/iswamanager.h>

namespace openspace{
class ISWACygnet : public properties::PropertyOwner{
public:
	ISWACygnet();
	~ISWACygnet();

	virtual bool initialize();
	virtual bool deinitialize();

	virtual void render();
	virtual void update();

	bool enabled(){return _enabled.value();}
	virtual bool isReady() = 0;
	
protected:
	void setPscUniforms(ghoul::opengl::ProgramObject* program, const Camera* camera, const PowerScaledCoordinate& position);
	virtual void setParent() = 0;
	void registerProperties();

	properties::BoolProperty _enabled;
	properties::IntProperty  _cygnetId;
	properties::FloatProperty _updateInterval;

	std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
	std::unique_ptr<ghoul::opengl::Texture> _texture;

	SceneGraphNode* _parent;
	glm::vec4 _pscOffset;
	glm::vec4 _modelScale;

	glm::dmat3 _stateMatrix;
	std::string _frame; 
	std::string _var;

	double _time;
	double _lastUpdateTime = 0;
	std::map<std::string, std::string> _month;

	std::string _fileExtension;
	std::string _path;

	float _openSpaceUpdateInterval;

	int _id;
};

}//namespace openspace
#endif