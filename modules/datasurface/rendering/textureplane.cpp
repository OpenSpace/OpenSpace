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

#include <modules/datasurface/rendering/textureplane.h>
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



namespace openspace {

TexturePlane::TexturePlane(std::string path) 
	:DataSurface(path)
	,_texture(nullptr)
	, _quad(0)
	, _vertexPositionBuffer(0)
	, _futureTexture(nullptr)
{}


TexturePlane::~TexturePlane(){}


bool TexturePlane::initialize(){
	DataSurface::initialize();
	std::thread t = std::thread(std::bind(&TexturePlane::updateTexture, this));
	t.detach();
	glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    if (_shader == nullptr) {
        // Plane Program

        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("PlaneProgram",
            "${MODULE_DATASURFACE}/shaders/dataplane_vs.glsl",
            "${MODULE_DATASURFACE}/shaders/dataplane_fs.glsl"
            );
        if (!_shader)
            return false;
    }
  
    loadTexture();
  
    return isReady();
}

bool TexturePlane::deinitialize(){
	DataSurface::deinitialize();

	glDeleteVertexArrays(1, &_quad);
	_quad = 0;

	glDeleteBuffers(1, &_vertexPositionBuffer);
	_vertexPositionBuffer = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

	return true;
}


bool TexturePlane::isReady() const {
	bool ready = true;
	if (!_shader)
		ready &= false;
	if(!_texture)
		ready &= false;
	return ready;
};

void TexturePlane::render(){
	psc position = _parent->worldPosition();

	glm::mat4 transform = glm::mat4(1.0);

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
	_stateMatrix = SpiceManager::ref().positionTransformMatrix("GALACTIC", "GSM", Time::ref().currentTime());
	if(_futureTexture && _futureTexture->isFinished){

		_path.set(absPath("${OPENSPACE_DATA}/"+_futureTexture->filePath));
		loadTexture();

		delete _futureTexture; 
		_futureTexture = nullptr;
	}
}

void TexturePlane::setParent(){
	_parent = OsEng.renderEngine().scene()->sceneGraphNode("Earth");
}

void TexturePlane::updateTexture(){
	int imageSize = 1024;
	DownloadManager::FileFuture* future;
	while(true) {
		std::this_thread::sleep_for(std::chrono::milliseconds(6000));
		 future = DlManager.downloadFile(
		 	getiSWAurl(5),
			// std::string("http://placehold.it/" + std::to_string(imageSize) + "x" + std::to_string(imageSize)),
			absPath("${OPENSPACE_DATA}/dataplane.jpg"),
			true,
			[](const DownloadManager::FileFuture& f){
				std::cout<<"download finished"<<std::endl;
			}
		);

		if(future){
			_futureTexture = future;
			imageSize-=1;
		}
	}	
}

void TexturePlane::loadTexture() {

        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_path));

		if (texture) {
			// LDEBUG("Loaded texture from '" << absPath(_path) << "'");

			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);
		}
	
}

void TexturePlane::createPlane() {
    // ============================
    // 		GEOMETRY (quad)
    // ============================
    const GLfloat x = 1.0;//_modelScale.x/2.0;
    const GLfloat y = 1.0;//_modelScale.y/2.0;
    const GLfloat w = 7.0;//_modelScale.w;
    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //	  x      y     z     w     s     t
        -x, -y, 0.0f, w, 0, 1,
        x, y, 0.0f, w, 1, 0,
        -x, y, 0.0f, w, 0, 0,
        -x, -y, 0.0f, w, 0, 1,
        x, -y, 0.0f, w, 1, 1,
        x, y, 0.0f, w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

}// namespace openspace