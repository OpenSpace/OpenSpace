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

#include <openspace/rendering/renderableplaneprojection.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/util/constants.h>
#include <openspace/util/imagesequencer.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/spicemanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/staticephemeris.h>
#include <openspace/scene/dynamicephemeris.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

namespace {
	const std::string _loggerCat = "RenderablePlaneProjection";
}

namespace openspace {

using namespace constants::renderableplaneprojection;

RenderablePlaneProjection::RenderablePlaneProjection(const ghoul::Dictionary& dictionary)
	: Renderable(dictionary)
	, _shader(nullptr)
	, _programIsDirty(false)
	, _texture(nullptr)
	, _textureIsDirty(false)
	, _quad(0)
	, _vertexPositionBuffer(0)
	, _name("ImagePlane")
	, _texturePath("")
	, _planeIsDirty(false)
{

	dictionary.getValue(keySpacecraft, _spacecraft);
	dictionary.getValue(keyInstrument, _instrument);
	dictionary.getValue(keyMoving, _moving);
	dictionary.getValue(keyName, _name);

	std::string texturePath = "";
	bool success = dictionary.getValue(keyTexture, _texturePath);
	if (success) {
		_texturePath = findPath(_texturePath);
		_textureFile = new ghoul::filesystem::File(_texturePath);
	}

	loadTexture();
}

RenderablePlaneProjection::~RenderablePlaneProjection() {
	delete _textureFile;
}

bool RenderablePlaneProjection::isReady() const {
	bool ready = true;
	if (!_shader)
		ready &= false;
	if (!_texture)
		ready &= false;
	return ready;
}

bool RenderablePlaneProjection::initialize() {
	glGenVertexArrays(1, &_quad); // generate array
	glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
	
	// Plane program
	if (_shader == nullptr)
		OsEng.ref().configurationManager()->getValue("planeProgram", _shader);

	setTarget("JUPITER");
	loadTexture();
	return isReady();
}

bool RenderablePlaneProjection::deinitialize() {
	glDeleteVertexArrays(1, &_quad);
	_quad = 0;
	glDeleteBuffers(1, &_vertexPositionBuffer);
	_vertexPositionBuffer = 0;
	delete _texture;
	return true;
}

void RenderablePlaneProjection::render(const RenderData& data) {
	
	glm::mat4 transform = glm::mat4(1.0);

	for (int i = 0; i < 3; i++){
		for (int j = 0; j < 3; j++){
			transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
		}
	}

	// Activate shader
	_shader->activate();

	_shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
	_shader->setUniform("ModelTransform", transform);
	setPscUniforms(_shader, &data.camera, data.position);

	data.position;
	ghoul::opengl::TextureUnit unit;
	unit.activate();
	_texture->bind();
	_shader->setUniform("texture1", unit);

	glBindVertexArray(_quad);
	glDrawArrays(GL_TRIANGLES, 0, 6);

	_shader->deactivate();

}

void RenderablePlaneProjection::update(const UpdateData& data) {
	

	double time = data.time;
	openspace::SpiceManager::ref().getPositionTransformMatrix(_target.frame, galacticFrame, time, _stateMatrix);
	
	std::string str = openspace::ImageSequencer::ref().getLatestImage();
	std::string tex = _texturePath;

	if (str != tex || _moving || _planeIsDirty) {
		updatePlane(time, str);
	}

	if (_programIsDirty) {
		_shader->rebuildFromFile();
		_programIsDirty = false;
	}

	if (_textureIsDirty) {
		loadTexture();
		_textureIsDirty = false;
	}
}

void RenderablePlaneProjection::loadTexture() {
	if (_texturePath != "") {
		ghoul::opengl::Texture* texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_texturePath));
		if (texture) {
			texture->uploadTexture();
			texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
			if (_texture)
				delete _texture;
			_texture = texture;

			delete _textureFile;
			_textureFile = new ghoul::filesystem::File(_texturePath);
			_textureFile->setCallback([&](const ghoul::filesystem::File&) { _textureIsDirty = true; });
		}
	}
}

void RenderablePlaneProjection::updatePlane(double time, std::string newPath) {
	
	std::string shape, frame;
	std::vector<glm::dvec3> bounds;
	glm::dvec3 boresight;

	int iTime = static_cast<int>(time);
	if (iTime % 2 && !_moving)
		setTarget("JUPITER");
	else if (!_moving)
		setTarget("IO");


	bool found = openspace::SpiceManager::ref().getFieldOfView(_instrument, shape, frame, boresight, bounds);
	if (!found) {
		LERROR("Could not locate instrument");
		return;
	}

	glm::dvec3 vecToTarget;
	double lt;
	psc projection[4];

	SpiceManager::ref().getTargetPosition(_target.body, _spacecraft, galacticFrame, "CN+S", time, vecToTarget, lt);
	// The apparent position, CN+S, makes image align best with target 

	for (int j = 0; j < bounds.size(); ++j) {
		openspace::SpiceManager::ref().frameConversion(bounds[j], frame, galacticFrame, time);
		glm::dvec3 cornerPosition = openspace::SpiceManager::ref().orthogonalProjection(vecToTarget, bounds[j]);
		
		if (!_moving) {
			cornerPosition -= vecToTarget;
		}
		openspace::SpiceManager::ref().frameConversion(cornerPosition, galacticFrame, _target.frame, time);
				
		projection[j] = PowerScaledCoordinate::CreatePowerScaledCoordinate(cornerPosition[0], cornerPosition[1], cornerPosition[2]);
		projection[j][3] += 3;
	}

	if (!_moving) {
		SceneGraphNode* thisNode = OsEng.renderEngine()->scene()->sceneGraphNode(_name);
		SceneGraphNode* newParent = OsEng.renderEngine()->scene()->sceneGraphNode(_target.node);
		if (thisNode != nullptr && newParent != nullptr)
			thisNode->setParent(newParent);
	}
	
	const GLfloat vertex_data[] = { // square of two triangles drawn within fov in target coordinates
		//	  x      y     z     w     s     t
		projection[1][0], projection[1][1], projection[1][2], projection[1][3], 0, 1, // Lower left 1
		projection[3][0], projection[3][1], projection[3][2], projection[3][3], 1, 0, // Upper right 2
		projection[2][0], projection[2][1], projection[2][2], projection[2][3], 0, 0, // Upper left 3
		projection[1][0], projection[1][1], projection[1][2], projection[1][3], 0, 1, // Lower left 4 = 1
		projection[0][0], projection[0][1], projection[0][2], projection[0][3], 1, 1, // Lower right 5
		projection[3][0], projection[3][1], projection[3][2], projection[3][3], 1, 0, // Upper left 6 = 2
	};

	glBindVertexArray(_quad); // bind array
	glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));

	if (!_moving) {
		_texturePath = newPath;
		loadTexture();
	}
}

void RenderablePlaneProjection::setTarget(std::string body) {

	std::vector<SceneGraphNode*> nodes = OsEng.renderEngine()->scene()->allSceneGraphNodes();
	Renderable* possibleTarget;
	bool hasBody, found = false;
	std::string targetBody;

	for (auto node : nodes)
	{
		possibleTarget = node->renderable();
		if (possibleTarget != nullptr) {
			hasBody = possibleTarget->hasBody();
			if (hasBody && possibleTarget->getBody(targetBody) && (targetBody == body)) {
				_target.node = node->name(); // get name from propertyOwner
				found = true;
				break;
			}
		}
	}
	if (found) {
		_target.body = body;
		_target.frame = "IAU_" + body;
	}
}

} // namespace openspace
