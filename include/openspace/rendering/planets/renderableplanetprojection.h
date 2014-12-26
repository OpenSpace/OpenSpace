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

#ifndef __RENDERABLEPLANETPROJECTION_H__
#define __RENDERABLEPLANETPROJECTION_H__
#include <ghoul/opengl/textureunit.h>

// open space includes
#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/framebufferobject.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <openspace/query/query.h>

namespace openspace {

namespace planetgeometryprojection {
class PlanetGeometryProjection;
}

class RenderablePlanetProjection : public Renderable {
public:
	RenderablePlanetProjection(const ghoul::Dictionary& dictionary);
	~RenderablePlanetProjection();

    bool initialize() override;
    bool deinitialize() override;
	bool isReady() const override;


	void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

protected:
    void loadTexture();

	GLuint genComputeProg();
	GLuint _computeShader;
	void updateTex();

private:
	void imageProject();

    properties::StringProperty _colorTexturePath;
	properties::StringProperty _projectionTexturePath;
	properties::TriggerProperty _imageTrigger;

    ghoul::opengl::ProgramObject* _programObject;
	ghoul::opengl::ProgramObject* _fboProgramObject;


    ghoul::opengl::Texture* _texture;
	ghoul::opengl::Texture* _textureProj;
	planetgeometryprojection::PlanetGeometryProjection* _geometry;

	glm::dmat3 _stateMatrix;
	glm::dmat3 _instrumentMatrix;
	glm::mat4  _projectorMatrix;
	glm::vec3  _boresight;
	glm::vec2  _camScaling;
	glm::mat4  _transform;

	double _time;
	openspace::SceneGraphNode* _targetNode;

	std::string _target;

	// FBO stuff
	GLuint _fboID;
	GLuint _quad;
	GLuint _vertexPositionBuffer;
};

}  // namespace openspace

#endif  // __RENDERABLEPLANETPROJECTION_H__