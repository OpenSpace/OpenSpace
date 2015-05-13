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

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/util/constants.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/spicemanager.h>

// ghoul
#include <ghoul/misc/dictionary.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/misc/assert.h>

namespace {
const std::string _loggerCat = "Renderable";
const std::string keyBody = "Body";
const std::string keyStart = "StartTime";
const std::string keyEnd = "EndTime";
}

namespace openspace {

Renderable* Renderable::createFromDictionary(const ghoul::Dictionary& dictionary)
{
	// The name is passed down from the SceneGraphNode
    std::string name;
    bool success = dictionary.getValue(constants::scenegraphnode::keyName, name);
	assert(success);

    std::string renderableType;
    success = dictionary.getValue(constants::renderable::keyType, renderableType);
	if (!success) {
        LERROR("Renderable '" << name << "' did not have key '"
                              << constants::renderable::keyType << "'");
        return nullptr;
	}

    ghoul::TemplateFactory<Renderable>* factory
          = FactoryManager::ref().factory<Renderable>();
    Renderable* result = factory->create(renderableType, dictionary);
    if (result == nullptr) {
        LERROR("Failed to create a Renderable object of type '" << renderableType << "'");
        return nullptr;
    }

    return result;
}

Renderable::Renderable(const ghoul::Dictionary& dictionary)
	: _enabled("enabled", "Is Enabled", true)
    , _startTime("")
    , _endTime("")
    , _targetBody("")
    , _hasBody(false)
    , _hasTimeInterval(false)
{
    setName("renderable");
#ifndef NDEBUG
	std::string name;
	ghoul_assert(dictionary.getValue(constants::scenegraphnode::keyName, name),
		"Scenegraphnode need to specify '" << constants::scenegraphnode::keyName 
		<< "' because renderables is going to use this for debugging!");
#endif
    // get path if available
	bool success = dictionary.getValue(constants::scenegraph::keyPathModule, _relativePath);
#ifndef NDEBUG
	ghoul_assert(success,
		"Scenegraphnode need to specify '" << constants::scenegraph::keyPathModule
		<< "' because renderables is going to use this for debugging!");
#endif
	if (success)
		_relativePath += ghoul::filesystem::FileSystem::PathSeparator;

	dictionary.getValue(keyStart, _startTime);
	dictionary.getValue(keyEnd, _endTime);

	if (_startTime != "" && _endTime != "")
		_hasTimeInterval = true;

	addProperty(_enabled);
}

Renderable::~Renderable() {
}

void Renderable::setBoundingSphere(const PowerScaledScalar& boundingSphere)
{
    boundingSphere_ = boundingSphere;
}

const PowerScaledScalar& Renderable::getBoundingSphere()
{
    return boundingSphere_;
}

void Renderable::update(const UpdateData&)
{
}

std::string Renderable::findPath(const std::string& path) {
    std::string tmp = absPath(path);
    if(FileSys.fileExists(tmp))
        return tmp;

    tmp = absPath(_relativePath + path);
    if(FileSys.fileExists(tmp))
        return tmp;

    LERROR("Could not find file '" << path << "'");

    return "";
}

void Renderable::setPscUniforms(
	ghoul::opengl::ProgramObject* program, 
	const Camera* camera,
	const PowerScaledCoordinate& position) 
{
	program->setUniform("campos", camera->position().vec4());
	program->setUniform("objpos", position.vec4());
	program->setUniform("camrot", camera->viewRotationMatrix());
	program->setUniform("scaling", camera->scaling());
}

bool Renderable::isVisible() const {
	return _enabled;
}

bool Renderable::hasTimeInterval() {
	return _hasTimeInterval;
}

bool Renderable::hasBody() {
	return _hasBody;
}

bool Renderable::getInterval(double& start, double& end) {
	if (_startTime != "" && _endTime != "") {
		bool successStart = openspace::SpiceManager::ref().getETfromDate(_startTime, start);
		bool successEnd = openspace::SpiceManager::ref().getETfromDate(_endTime, end);
		return successStart && successEnd;
	}
	else
		return false;
}

bool Renderable::getBody(std::string& body) {
	if (_hasBody) {
		body = _targetBody;
		return true;
	}
	else
		return false;
}

void Renderable::setBody(std::string& body) {
	_targetBody = body;
	_hasBody = true;
}

bool Renderable::isReady() const {
	return true;
}

bool Renderable::isEnabled() const {
	return _enabled;
}

}  // namespace openspace
