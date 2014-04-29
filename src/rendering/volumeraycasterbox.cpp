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

#include <openspace/rendering/volumeraycasterbox.h>

#include <openspace/engine/openspaceengine.h>
#include <sgct.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>


namespace {
    std::string _loggerCat = "VolumeRaycasterBox";
}

using namespace ghoul::opengl;

namespace openspace {

VolumeRaycasterBox::VolumeRaycasterBox() {

}

VolumeRaycasterBox::~VolumeRaycasterBox() {}

bool VolumeRaycasterBox::initialize() {
    _boundingBox = new sgct_utils::SGCTBox(1.0f, sgct_utils::SGCTBox::Regular);
    
    //	------ SETUP SHADER -----------------
	_boxProgram = new ProgramObject("RaycastBoxProgram");
    /*
	ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
                                                  absPath("${BASE_PATH}/shaders/exitpoints.vert"));
	ShaderObject* fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,
                                                    absPath("${BASE_PATH}/shaders/exitpoints.frag"));
               
     
	_boxProgram->attachObject(vertexShader);
	_boxProgram->attachObject(fragmentShader);
	_boxProgram->compileShaderObjects();
	_boxProgram->linkProgramObject();
    */
    OsEng.configurationManager().getValue("RaycastProgram", _boxProgram);
	_MVPLocation = _boxProgram->uniformLocation("modelViewProjection");
    
	//	------ SETUP FBO ---------------------
	_fbo = new FramebufferObject();
	_fbo->activate();
    
    
	size_t x = sgct::Engine::instance()->getActiveXResolution();
	size_t y = sgct::Engine::instance()->getActiveYResolution();
    _dimensions = glm::size2_t(x, y);
    
	_backTexture = new ghoul::opengl::Texture(glm::size3_t(x,y,1));
	_frontTexture = new ghoul::opengl::Texture(glm::size3_t(x,y,1));
    _backTexture->uploadTexture();
    _frontTexture->uploadTexture();
	_fbo->attachTexture(_backTexture, GL_COLOR_ATTACHMENT0);
	_fbo->attachTexture(_frontTexture, GL_COLOR_ATTACHMENT1);
    
	_fbo->deactivate();
    
    return true;
}

void VolumeRaycasterBox::render(glm::mat4 MVP) {
    GLuint activeFBO = ghoul::opengl::FramebufferObject::getActiveObject(); // Save SGCTs main FBO
	_fbo->activate();
	_boxProgram->activate();
	_boxProgram->setUniform(_MVPLocation, MVP);

	//	Draw backface
	glDrawBuffer(GL_COLOR_ATTACHMENT0);
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glClear(GL_COLOR_BUFFER_BIT);
	glEnable(GL_CULL_FACE);
	glCullFace(GL_FRONT);
	_boundingBox->draw();
	glDisable(GL_CULL_FACE);

	//	Draw frontface
	glDrawBuffer(GL_COLOR_ATTACHMENT1);
	glClear(GL_COLOR_BUFFER_BIT);
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glEnable(GL_CULL_FACE);
	glCullFace(GL_BACK);
	_boundingBox->draw();
	glDisable(GL_CULL_FACE);

	_boxProgram->deactivate();
	_fbo->deactivate();
    glBindFramebuffer(GL_FRAMEBUFFER, activeFBO);
}

Texture* VolumeRaycasterBox::backFace() {
	return _backTexture;
}

Texture* VolumeRaycasterBox::frontFace() {
	return _frontTexture;
}

glm::size2_t VolumeRaycasterBox::dimensions() {
    return _dimensions;
}

} /* namespace openspace */
