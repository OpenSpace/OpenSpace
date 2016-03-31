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

// open space includes
#include <modules/planetbrowsing/rendering/Planet.h>

#include <openspace/rendering/renderengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>

#include <modules/base/rendering/planetgeometry.h>


#include <vector>
#include <glm/glm.hpp>

namespace {
	const std::string _loggerCat = "Planet";
	const std::string keyGeometry = "Geometry";

}

namespace openspace {

Planet::Planet(const ghoul::Dictionary& dictionary) :
	Renderable(dictionary),
	_geometry(nullptr),
	_testProgramObject(nullptr)
{
	std::string name;
	bool success = dictionary.getValue(SceneGraphNode::KeyName, name);
	LDEBUG(name);

	ghoul_assert(success,
		"Planet need the '" << SceneGraphNode::KeyName << "' be specified");


	ghoul::Dictionary geometryDictionary;
	success = dictionary.getValue(keyGeometry, geometryDictionary);
	if (success) {
		geometryDictionary.setValue(SceneGraphNode::KeyName, name);
		//geometryDictionary.setValue(constants::scenegraph::keyPathModule, path);
		_geometry = planetgeometry::PlanetGeometry::createFromDictionary(geometryDictionary);
	}

	addPropertySubOwner(_geometry);

	/*
	// Create a simple triangle for testing the geometry
	std::vector<unsigned int> triangleElements;
	std::vector<glm::vec4> trianglePositions;

	trianglePositions.push_back(glm::vec4(0, 0, -0.5, 1));
	trianglePositions.push_back(glm::vec4(1, 0, -0.5, 1));
	trianglePositions.push_back(glm::vec4(1, 1, -0.5, 1));

	triangleElements.push_back(0);
	triangleElements.push_back(1);
	triangleElements.push_back(2);

	_testGeometry = std::unique_ptr<Geometry>(new Geometry(
		triangleElements,
		Geometry::Positions::Yes,
		Geometry::Textures::No, 
		Geometry::Normals::No));

	_testGeometry->setPositionData(trianglePositions);
	_testGeometry->initialize();
	*/
	// Create a test shader program object

}

Planet::~Planet() {
	
}
	
bool Planet::initialize() {
	RenderEngine& renderEngine = OsEng.renderEngine();
	if (_testProgramObject == nullptr) {
		_testProgramObject = renderEngine.buildRenderProgram(
			"simpleTestProgram",
			"${MODULE_PLANETBROWSING}/shaders/simple_vs.glsl",
			"${MODULE_PLANETBROWSING}/shaders/simple_fs.glsl");
		if (!_testProgramObject) return false;
	}

	return true;
}

bool Planet::deinitialize() {
	RenderEngine& renderEngine = OsEng.renderEngine();
	if (_testProgramObject) {
		renderEngine.removeRenderProgram(_testProgramObject);
		_testProgramObject = nullptr;
	}
	
	if (_geometry) {
		_geometry->deinitialize();
		delete _geometry;
	}
	_geometry = nullptr;

	return true;
}

bool Planet::isReady() const {
	bool ready = true;
	ready &= (_testProgramObject != nullptr);
	ready &= (_geometry != nullptr);

	return ready;
}

void Planet::render(const RenderData& data) {
	// activate shader
	_testProgramObject->activate();


	//_testGeometry->render();
	_geometry->render();


	// disable shader
	_testProgramObject->deactivate();
}

void Planet::update(const UpdateData& data) {
		
}

}  // namespace openspace
