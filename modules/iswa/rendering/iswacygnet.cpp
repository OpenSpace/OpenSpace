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
#include <modules/iswa/rendering/iswacygnet.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>

namespace openspace{

ISWACygnet::ISWACygnet(std::shared_ptr<Metadata> data)
	: _enabled("enabled", "Is Enabled", true)
	, _updateInterval("updateInterval", "Update Interval", 3, 1, 10)
	, _delete("delete", "Delete")
	, _shader(nullptr)
	, _texture(nullptr)
	, _data(data)
	, _memorybuffer("")
{
	addProperty(_enabled);
	addProperty(_updateInterval);
	addProperty(_delete);

	_delete.onChange([this](){ISWAManager::ref().deleteISWACygnet(name());});
}

ISWACygnet::~ISWACygnet(){}


void ISWACygnet::setPscUniforms(
	ghoul::opengl::ProgramObject* program, 
	const Camera* camera,
	const PowerScaledCoordinate& position) 
{
	program->setUniform("campos", camera->position().vec4());
	program->setUniform("objpos", position.vec4());
	program->setUniform("camrot", camera->viewRotationMatrix());
	program->setUniform("scaling", camera->scaling());
}

void ISWACygnet::registerProperties(){
	OsEng.gui()._iSWAproperty.registerProperty(&_enabled);
	OsEng.gui()._iSWAproperty.registerProperty(&_updateInterval);
	OsEng.gui()._iSWAproperty.registerProperty(&_delete);
}

void ISWACygnet::unregisterProperties(){
	OsEng.gui()._iSWAproperty.unregisterProperties(name());
}

void ISWACygnet::setParent(){
	_parent = OsEng.renderEngine().scene()->sceneGraphNode(_data->parent);
}

}//namespace openspac