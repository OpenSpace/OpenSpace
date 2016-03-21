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

#include <modules/datasurface/rendering/renderabledataplane.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <modules/kameleon/include/kameleonwrapper.h>


namespace openspace {

RenderableDataPlane::RenderableDataPlane(const ghoul::Dictionary& dictionary) 
	: Renderable(dictionary)
	,_texturePath("texture", "Texture", "${OPENSPACE_DATA}/test.png")
	,_size("size", "Size", glm::vec2(0.7,8), glm::vec2(0.f), glm::vec2(10.0f))
	,_roatation("rotation", "Roatation", glm::vec3(0.3,0.4,0.0), glm::vec3(0), glm::vec3(2*M_PI))
	,_origin("origin", "Origin", glm::vec2(0.05, 0.001), glm::vec2(-0.1), glm::vec2(0.1))
	, _shader(nullptr)
	, _texture(nullptr)
	, _quad(0)
	, _vertexPositionBuffer(0)
{
	addProperty(_size);
	addProperty(_texturePath);
	addProperty(_roatation);
	addProperty(_origin);
	_texturePath.onChange(std::bind(&RenderableDataPlane::loadTexture, this));
    _size.onChange([this](){ _planeIsDirty = true; });

}

RenderableDataPlane::~RenderableDataPlane(){
}

bool RenderableDataPlane::initialize() {

	KameleonWrapper kw(absPath("${OPENSPACE_DATA}/Alexa_Halford_062105_2.3df.007200.cdf"));

	_dimensions = glm::size3_t(10,10,1);
	float zSlice = 0.5f;

	float* _dataSlice = kw.getUniformSliceValues(std::string("p"), _dimensions, zSlice);

	for (int i = 0; i < dimensions.x; ++i)
	{
		for (int k = 0; k < dimensions.x; ++k){
			std::cout << _dataSlice[k] << "  ";
		}
		std::cout << std::endl;
	}

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
};
bool RenderableDataPlane::deinitialize() {
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
};

bool RenderableDataPlane::isReady() const {
	bool ready = true;
	if (!_shader)
		ready &= false;
	if(!_texture)
		ready &= false;
	return ready;
};

void RenderableDataPlane::render(const RenderData& data)
{
	float w = (float)_texture->width();
	float h = (float)_texture->height();
	float textureRatio = h/w;

	glm::mat4 transform = glm::mat4(1.0);
	transform = glm::scale(transform, glm::vec3(1.0, textureRatio, 1.0f));
	glm::mat4 translate = glm::translate(glm::mat4(1.0), glm::vec3(_origin.value()[0], _origin.value()[1], 0.0f));
	transform = glm::rotate(transform, _roatation.value()[0], glm::vec3(1,0,0));
	transform = glm::rotate(transform, _roatation.value()[1], glm::vec3(0,1,0));
	transform = glm::rotate(transform, _roatation.value()[2], glm::vec3(0,0,1));

	// Activate shader
	_shader->activate();

	_shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_shader->setUniform("ModelTransform", transform);
	_shader->setUniform("Translate", translate);
	setPscUniforms(_shader.get(), &data.camera, data.position);

	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_shader->setUniform("texture1", unit);

	glBindVertexArray(_quad);
	glDrawArrays(GL_TRIANGLES, 0, 6);

	_shader->deactivate();
};
void RenderableDataPlane::update(const UpdateData& data){
	if (_planeIsDirty)
       createPlane();
};


void RenderableDataPlane::loadTexture() {
	if (_texturePath.value() != "") {
        //std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));
		ghoul::opengl::Texture::FilterMode filtermode = ghoul::opengl::Texture::FilterMode::Linear;
		ghoul::opengl::Texture::WrappingMode wrappingmode = ghoul::opengl::Texture::WrappingMode::ClampToEdge;
		std::unique_ptr<ghoul::opengl::Texture> texture = 
			std::make_unique<ghoul::opengl::Texture>(_dataSlice, _dimensions, ghoul::opengl::Texture::Format::Red, GL_RED, GL_FLOAT, filtermode, wrappingmode);
		if (texture) {
			std::cout << "texture path: " << absPath(_texturePath) << std::endl;
			// LDEBUG("Loaded texture from '" << absPath(_texturePath) << "'");

			texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);
		}
	}
}

void RenderableDataPlane::createPlane() {
    // ============================
    // 		GEOMETRY (quad)
    // ============================
    const GLfloat size = _size.value()[0];
    const GLfloat w = _size.value()[1];
    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //	  x      y     z     w     s     t
        -size, -size, 0.0f, w, 0, 1,
        size, size, 0.0f, w, 1, 0,
        -size, size, 0.0f, w, 0, 0,
        -size, -size, 0.0f, w, 0, 1,
        size, -size, 0.0f, w, 1, 1,
        size, size, 0.0f, w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
    _planeIsDirty = false;
}
}// namespace openspace