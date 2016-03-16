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
#include <modules/base/rendering/screenspaceframebuffer.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <modules/onscreengui/include/gui.h>
#include <openspace/util/camera.h>

#include <openspace/rendering/renderer.h>
#include <openspace/rendering/abufferrenderer.h>
#include <openspace/rendering/framebufferrenderer.h>

namespace openspace {
ScreenSpaceFramebuffer::ScreenSpaceFramebuffer() 
	:ScreenSpaceRenderable()
	,_framebuffer(nullptr)
{
	_id = id();
	setName("ScreenSpaceFramebuffer" + std::to_string(_id));
	OsEng.gui()._property.registerProperty(&_enabled);
	OsEng.gui()._property.registerProperty(&_flatScreen);
	OsEng.gui()._property.registerProperty(&_euclideanPosition);
	OsEng.gui()._property.registerProperty(&_sphericalPosition);
	OsEng.gui()._property.registerProperty(&_depth);
	OsEng.gui()._property.registerProperty(&_scale);

	if(_useEuclideanCoordinates){
		_euclideanPosition.onChange([this](){
			_sphericalPosition.set(toSpherical(_euclideanPosition.value()));
		});
		_sphericalPosition.onChange([this](){});
	}else{
		_euclideanPosition.onChange([this](){
			_sphericalPosition.set(toSpherical(_euclideanPosition.value()));
		});
	}
}

ScreenSpaceFramebuffer::~ScreenSpaceFramebuffer(){}

bool ScreenSpaceFramebuffer::initialize(){
	_framebuffer = std::make_unique<ghoul::opengl::FramebufferObject>();
	ghoul::opengl::Texture texture = ghoul::opengl::Texture(glm::uvec3(512, 512, 10));
	_framebuffer->attachTexture(&texture, GL_COLOR_ATTACHMENT0);
	return true;
}

bool ScreenSpaceFramebuffer::deinitialize(){
	return true;
}

void ScreenSpaceFramebuffer::render(){
	GLint defaultFBO = _framebuffer->getActiveObject();
	_framebuffer->activate();
	// _shader->activate();


	// _shader->deactivate();
	_framebuffer->deactivate();
	glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
}

void ScreenSpaceFramebuffer::update(){}
bool ScreenSpaceFramebuffer::isReady() const {}

int ScreenSpaceFramebuffer::id(){
		static int id = 0;
		return id++;
}
} //namespace openspace