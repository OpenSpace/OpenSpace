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

#include <modules/iswa/rendering/screenspacecygnet.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem>
#include <openspace/util/time.h>
#include <modules/iswa/util/iswamanager.h>

namespace {
	const std::string _loggerCat = "ScreenSpaceCygnet";
}

namespace openspace {

ScreenSpaceCygnet::ScreenSpaceCygnet(int cygnetId, std::string path)
	: ScreenSpaceRenderable()
	, _updateInterval("updateInterval", "Update Interval", 3, 1, 10)
	, _cygnetId(cygnetId)
	, _path(path)
{
	std::cout << "screenspacecygnet constructor 1" << std::endl;
	_id = id();
	setName("ScreenSpaceCygnet" + std::to_string(_id));
	addProperty(_updateInterval);

	OsEng.gui()._iSWAproperty.registerProperty(&_enabled);
	OsEng.gui()._iSWAproperty.registerProperty(&_useFlatScreen);
	OsEng.gui()._iSWAproperty.registerProperty(&_euclideanPosition);
	OsEng.gui()._iSWAproperty.registerProperty(&_sphericalPosition);
	OsEng.gui()._iSWAproperty.registerProperty(&_depth);
	OsEng.gui()._iSWAproperty.registerProperty(&_scale);
	OsEng.gui()._iSWAproperty.registerProperty(&_alpha);
	OsEng.gui()._iSWAproperty.registerProperty(&_updateInterval);
	OsEng.gui()._iSWAproperty.registerProperty(&_delete);
}

ScreenSpaceCygnet::ScreenSpaceCygnet(std::shared_ptr<Metadata> data)
	: ScreenSpaceRenderable()
	, _updateInterval("updateInterval", "Update Interval", 3, 1, 10)
	, _cygnetId(data->id)
	, _path(data->path)
{
	_id = id();
	setName("ScreenSpaceCygnet" + std::to_string(_id));
	addProperty(_updateInterval);

	OsEng.gui()._iSWAproperty.registerProperty(&_enabled);
	OsEng.gui()._iSWAproperty.registerProperty(&_useFlatScreen);
	OsEng.gui()._iSWAproperty.registerProperty(&_euclideanPosition);
	OsEng.gui()._iSWAproperty.registerProperty(&_sphericalPosition);
	OsEng.gui()._iSWAproperty.registerProperty(&_depth);
	OsEng.gui()._iSWAproperty.registerProperty(&_scale);
	OsEng.gui()._iSWAproperty.registerProperty(&_alpha);
	OsEng.gui()._iSWAproperty.registerProperty(&_updateInterval);
	OsEng.gui()._iSWAproperty.registerProperty(&_delete);

}

ScreenSpaceCygnet::~ScreenSpaceCygnet(){}

bool ScreenSpaceCygnet::initialize(){
	_originalViewportSize = OsEng.windowWrapper().currentWindowResolution();
	createPlane();
	createShaders();
	updateTexture();

	// Setting spherical/euclidean onchange handler
	_useFlatScreen.onChange([this](){
		useEuclideanCoordinates(_useFlatScreen.value());
	});

	return isReady();
}

bool ScreenSpaceCygnet::deinitialize(){
	OsEng.gui()._iSWAproperty.unregisterProperties(name());

	glDeleteVertexArrays(1, &_quad);
	_quad = 0;

	glDeleteBuffers(1, &_vertexPositionBuffer);
	_vertexPositionBuffer = 0;

	_texture = nullptr;

	 RenderEngine& renderEngine = OsEng.renderEngine();
	if (_shader) {
		renderEngine.removeRenderProgram(_shader);
		_shader = nullptr;
	}

	std::remove(absPath(_path).c_str());
	_path = "";
	
	return true;
}

void ScreenSpaceCygnet::render(){

	if(!isReady()) return;
	if(!_enabled) return;

	glm::mat4 rotation = rotationMatrix();
	glm::mat4 translation = translationMatrix();
	glm::mat4 scale = scaleMatrix();
	glm::mat4 modelTransform = rotation*translation*scale;

	draw(modelTransform);
}

void ScreenSpaceCygnet::update(){

	_time = Time::ref().currentTime();

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

bool ScreenSpaceCygnet::isReady() const{
	bool ready = true;
	if (!_shader)
		ready &= false;
	if(!_texture)
		ready &= false;
	return ready;
}

void ScreenSpaceCygnet::updateTexture(){
	std::shared_ptr<DownloadManager::FileFuture> future = ISWAManager::ref().downloadImage(_cygnetId, absPath(_path));
	if(future){
		_futureTexture = future;
	}
}

void ScreenSpaceCygnet::loadTexture() {

	std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_path));

	if (texture) {
		LDEBUG("Loaded texture from '" << absPath(_path) << "'");

		texture->uploadTexture();
		// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
		texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

		_texture = std::move(texture);
	}
}

int ScreenSpaceCygnet::id(){
	static int id = 0;
	return id++;
}

}