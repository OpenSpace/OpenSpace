/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLE_EXPLORATION_PATH___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLE_EXPLORATION_PATH___H__

#include <openspace/rendering/renderable.h>
#include <ghoul/opengl/programobject.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/models/renderablesite.h>


#include <map>

namespace openspace {
namespace globebrowsing {
	class RenderableSite;

class RenderableExplorationPath : public Renderable {
public:

	RenderableExplorationPath(const RenderableSite& owner, std::vector<glm::dvec2> coordinates);
	~RenderableExplorationPath();
	
	bool initialize() override;
	bool deinitialize() override;
	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;

private:
	void calculatePathModelCoordinates();

	std::unique_ptr<ghoul::opengl::ProgramObject> _pathShader;
	std::unique_ptr<ghoul::opengl::ProgramObject> _siteShader;
	properties::BoolProperty _isEnabled;

	bool _isReady;

	std::vector<glm::vec4> _stationPointsModelCoordinates;

	globebrowsing::RenderableGlobe* _globe;

	std::vector<glm::dvec2> _latLonCoordinates;

	float _fading;
	GLuint _vaioID;
	GLuint _vertexBufferID;
	double _cameraToPointDistance;
	bool _hasLoopedOnce;
	bool _isCloseEnough;

	const RenderableSite& _owner;

};
}
}

#endif //__OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLE_EXPLORATION_PATH___H__
