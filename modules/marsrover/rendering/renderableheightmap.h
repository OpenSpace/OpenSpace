/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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



// need to include roverterrain.cpp
//need to access the vertex positions of the fullsite_vs.glsl file. 

//send position of every vertex in fullsubsite_vs.glsl

//FUNCTIONALITY
// look at the x,y,z values of each vertex point
// check if there is several z values for the same point (maybe this is better done in fullsite_vs.glsl?)
	//decide which z value is the heighest.
// save the correct and sampled x,y,z values
//write to a txt file (or maybe png??)

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLEHEIGHTMAP___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLEHEIGHTMAP___H__


#include <openspace/rendering/renderable.h>
#include <ghoul/misc/dictionary.h>
#include <modules/roverterrainrenderer/filehandler/subsite.h>
#include <modules/roverterrainrenderer/renderable/subsitemodels.h>
#include <ghoul/opengl/programobject.h>
#include <modules/roverterrainrenderer/renderable/sitemanager.h>


//#include <modules/roverterrainrenderer/shaders/fullsubsite_vs.glsl>
#include <modules/roverterrainrenderer/filehandler/roverpathfilereader.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <openspace/properties/vector/vec2property.h>

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <ghoul/opengl/uniformcache.h>
#include <openspace/util/updatestructures.h>	//must be here in order to use globebrowsing and scenegraphnode

namespace openspace {

	struct RenderData;
	struct UpdateData;

	namespace documentation { struct Documentation; }

class RenderableHeightMap : public Renderable{
public:

	RenderableHeightMap(const ghoul::Dictionary& dictionary);

	void initializeGL() override;
	void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks&) override;
    void update(const UpdateData& data) override;

    

	static documentation::Documentation Documentation();

private:
	void renderOrthoCamera();
	void drawTexture(); 


	//glm::dvec3 _modelPathPosition; //If we want to improve our code with position for every submodel 
    properties::Vec2Property _frustumSize;
	ghoul::opengl::ProgramObject* _shader;
    //std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

 	UniformCache(cameraFrustumSize, directionToSurfaceViewSpace, 
 				 heightmapTexture, modelViewTransform, projectionTransform) _uniformCache;

 	openspace::SceneGraphNode* _parent;

 	globebrowsing::RenderableGlobe* _globe;

 	GLuint _vertexPositionBuffer;
 	GLuint _heightmap;
 	GLuint _zbuffer;

 	GLuint _vaHeightMap;
 	GLuint _vbHeightMap;


};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___EXTRAXTHEIGHTMAP___H__
