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

#include <modules/datasurface/rendering/dataplane.h>
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

DataPlane::DataPlane(std::shared_ptr<KameleonWrapper> kw, std::string path) 
	:DataSurface(path)
	, _kw(kw)
	, _texture(nullptr)
	, _quad(0)
	, _vertexPositionBuffer(0)
{	
	_id = id();
	setName("DataPlane" + std::to_string(_id));
	OsEng.gui()._property.registerProperty(&_enabled);
	OsEng.gui()._property.registerProperty(&_cygnetId);
	OsEng.gui()._property.registerProperty(&_path);
	OsEng.gui()._property.registerProperty(&_updateInterval);

	KameleonWrapper::Model model = _kw->model();
	if(	model == KameleonWrapper::Model::BATSRUS){
		_var = "p";
	}else{
		_var = "rho";
	}
	std::cout << name() << std::endl;
}


DataPlane::~DataPlane(){}


bool DataPlane::initialize(){
	DataSurface::initialize();

	_modelScale = _kw->getModelScaleScaled();
	_pscOffset  = _kw->getModelBarycenterOffsetScaled();

	std::cout << _modelScale.x << ", " << _modelScale.y << ", " << _modelScale.z << ", " << _modelScale.w << std::endl;
	std::cout << _pscOffset.x << ", " << _pscOffset.y << ", " << _pscOffset.z << ", " << _pscOffset.w << std::endl;

	_dimensions = glm::size3_t(500,500,1);
	float zSlice = 0.5f;

	_dataSlice = _kw->getUniformSliceValues(std::string(_var), _dimensions, zSlice);

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

bool DataPlane::deinitialize(){
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


bool DataPlane::isReady() const {
	bool ready = true;
	if (!_shader)
		ready &= false;
	if(!_texture)
		ready &= false;
	return ready;
};

void DataPlane::render(){
	// getiSWAurl(1);

	psc position = _parent->worldPosition();

	glm::mat4 transform = glm::mat4(1.0);

	glm::mat4 rotx = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
	glm::mat4 roty = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, -1, 0));
	glm::mat4 rotz = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, 0, 1));

	glm::mat4 rot = glm::mat4(1.0);
	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
		}
	}

	transform = transform * rotz * roty; //BATSRUS
	// transform = transform * roty;

	// transform = glm::rotate(transform, _roatation.value()[0], glm::vec3(1,0,0));
	// transform = glm::rotate(transform, _roatation.value()[1], glm::vec3(0,1,0));
	// transform = glm::rotate(transform, _roatation.value()[2], glm::vec3(0,0,1));

	glm::vec4 v(1,0,0,1);
	glm::vec3 v1 = glm::vec3(transform*v);
	v1 = glm::normalize(v1);

	double  lt;
    glm::vec3 v2 =
    SpiceManager::ref().targetPosition("SUN", "Earth", "GALACTIC", {}, _time, lt);
    v2 = glm::normalize(v2);

    float angle = acos(glm::dot(v1,v2));
    glm::vec3 ref = glm::cross(v1, v2);

    transform = glm::rotate(transform, angle, ref);


    // float angle = acos(glm::dot(sun_vec, (vec))/(glm::length(sun_vec)*glm::length(glm::vec3(vec))));
	position += transform*glm::vec4(_pscOffset.x, _pscOffset.z, _pscOffset.y, _pscOffset.w); 

    // std::cout << sun_vec.x << ", " << sun_vec.y << ", " << sun_vec.z << std::endl;
    // std::cout << vec.x << ", " << vec.y << ", " << vec.z << std::endl;
    // std::cout << angle << std::endl << std::endl;


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

void DataPlane::update(){
	_time = Time::ref().currentTime();
	_stateMatrix = SpiceManager::ref().positionTransformMatrix("GALACTIC", _frame, _time);

}

void DataPlane::setParent(){
	KameleonWrapper::Model model = _kw->model();
	if(	model == KameleonWrapper::Model::BATSRUS ||
		model == KameleonWrapper::Model::OpenGGCM ||
		model == KameleonWrapper::Model::LFM)
	{
		_parent = OsEng.renderEngine().scene()->sceneGraphNode("Earth");
		_frame = "GSM";
	}else if(
		model == KameleonWrapper::Model::ENLIL ||
		model == KameleonWrapper::Model::MAS ||
		model == KameleonWrapper::Model::Adapt3D ||
		model == KameleonWrapper::Model::SWMF)
	{
		_parent = OsEng.renderEngine().scene()->sceneGraphNode("SolarSystem");
		_frame = "GALACTIC";
	}else{
		//Warning!
	}
}



void DataPlane::loadTexture() {
        //std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));
		ghoul::opengl::Texture::FilterMode filtermode = ghoul::opengl::Texture::FilterMode::Linear;
		ghoul::opengl::Texture::WrappingMode wrappingmode = ghoul::opengl::Texture::WrappingMode::ClampToEdge;
		std::unique_ptr<ghoul::opengl::Texture> texture = 
			std::make_unique<ghoul::opengl::Texture>(_dataSlice, _dimensions, ghoul::opengl::Texture::Format::Red, GL_RED, GL_FLOAT, filtermode, wrappingmode);
		if (texture) {
			// std::cout << "texture path: " << absPath(_texturePath) << std::endl;
			// LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");

			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);
		}
	
}

void DataPlane::createPlane() {
    // ============================
    // 		GEOMETRY (quad)
    // ============================
    const GLfloat x = _modelScale.x/2.0;
    const GLfloat y = _modelScale.z/2.0;
    const GLfloat w = _modelScale.w;
    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //	  x      y     z     w     s     t
        -x, -y, 0, w, 0, 1,
         x,  y, 0, w, 1, 0,
        -x,  y, 0, w, 0, 0,
        -x, -y, 0, w, 0, 1,
         x, -y, 0, w, 1, 1,
         x,  y, 0, w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

int DataPlane::id(){
		static int id = 0;
		return id++;
}
}// namespace openspace