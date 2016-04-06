// /*****************************************************************************************
//  *                                                                                       *
//  * OpenSpace                                                                             *
//  *                                                                                       *
//  * Copyright (c) 2014-2016                                                               *
//  *                                                                                       *
//  * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
//  * software and associated documentation files (the "Software"), to deal in the Software *
//  * without restriction, including without limitation the rights to use, copy, modify,    *
//  * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
//  * permit persons to whom the Software is furnished to do so, subject to the following   *
//  * conditions:                                                                           *
//  *                                                                                       *
//  * The above copyright notice and this permission notice shall be included in all copies *
//  * or substantial portions of the Software.                                              *
//  *                                                                                       *
//  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
//  * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
//  * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
//  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
//  * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
//  * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
//  ****************************************************************************************/
#include <modules/iswa/rendering/textureplane.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

namespace {
	const std::string _loggerCat = "TexutePlane";
}

namespace openspace {
TexturePlane::TexturePlane(std::shared_ptr<Metadata> data) 
	:CygnetPlane(data)
	,_futureTexture(nullptr)
{
	_id = id();
	setName("TexturePlane" + std::to_string(_id));
	registerProperties();
}

TexturePlane::~TexturePlane(){}

bool TexturePlane::initialize(){
	setParent();
    createPlane();
    createShader();
	updateTexture();
    return isReady();
}

bool TexturePlane::deinitialize(){
	_parent = nullptr;
    unregisterProperties();
    destroyPlane();
    destroyShader();
    _memorybuffer = "";
    // std::remove(absPath(_data->path).c_str());

	return true;
}

void TexturePlane::render(){
	psc position = _parent->worldPosition();
	glm::mat4 transform = glm::mat4(1.0);
	transform = glm::inverse(OsEng.renderEngine().camera()->viewRotationMatrix());

	float textureRatio = (float (_texture->height()/float(_texture->width())));
	transform = glm::scale(transform, glm::vec3(1, textureRatio, 1));

	glm::mat4 rotx = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
	glm::mat4 roty = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, 1, 0));
	// glm::mat4 rot = glm::mat4(1.0);
/*	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
		}
	}*/

	//transform = transform * roty * rotx;
	//position += transform*glm::vec4(-_pscOffset.x, _pscOffset.z, _pscOffset.y, _pscOffset.w); 

	// transform = glm::rotate(transform, _roatation.value()[0], glm::vec3(1,0,0));
	// transform = glm::rotate(transform, _roatation.value()[1], glm::vec3(0,1,0));
	// transform = glm::rotate(transform, _roatation.value()[2], glm::vec3(0,0,1));

	// Activate shader
	_shader->activate();
	glEnable(GL_ALPHA_TEST);
	glDisable(GL_CULL_FACE);
	_shader->setUniform("ViewProjection", OsEng.renderEngine().camera()->viewProjectionMatrix());
	_shader->setUniform("ModelTransform", transform);
	setPscUniforms(_shader.get(), OsEng.renderEngine().camera(), position);

	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_shader->setUniform("texture1", unit);

	glBindVertexArray(_quad);
	glDrawArrays(GL_TRIANGLES, 0, 6);
	glEnable(GL_CULL_FACE);
	
	_shader->deactivate();

}

void TexturePlane::update(){
	if(_planeIsDirty)
		createPlane();

	_time = Time::ref().currentTime();
	_stateMatrix = SpiceManager::ref().positionTransformMatrix("GALACTIC", _data->frame, _time);
    
    float openSpaceUpdateInterval = abs(Time::ref().deltaTime()*_updateInterval);
    if(openSpaceUpdateInterval){
    	if(abs(_time-_lastUpdateTime) >= openSpaceUpdateInterval){
    		updateTexture();
    		_lastUpdateTime = _time;
    	}
    }

	if(_futureTexture && _futureTexture->isFinished){
		loadTexture();
		_futureTexture = nullptr;
	}
}



void TexturePlane::loadTexture() {
	// std::cout << _data->path << std::endl;
	// std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_data->path));
	if(_memorybuffer != ""){
		std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTextureFromMemory(_memorybuffer);
		if (texture) {
			// LDEBUG("Loaded texture from '" << absPath(_data->path) << "'");

			texture->uploadTexture();
			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

	        _texture = std::move(texture);
		}
	}	
}

void TexturePlane::updateTexture(){
	std::shared_ptr<DownloadManager::FileFuture> future = ISWAManager::ref().downloadImageToMemory(_data->id, _memorybuffer);
	// std::shared_ptr<DownloadManager::FileFuture> future = ISWAManager::ref().downloadImage(_data->id, absPath(_data->path));
	if(future){
		_futureTexture = future;
	}
}

int TexturePlane::id(){
		static int id = 0;
		return id++;
}

}// namespace openspace