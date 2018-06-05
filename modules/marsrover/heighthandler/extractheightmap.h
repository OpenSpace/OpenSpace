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

#include <ghoul/misc/dictionary.h>
#include <modules/roverterrainrenderer/filehandler/subsite.h>
#include <modules/roverterrainrenderer/renderable/subsitemodels.h>
#include <openspace/scene/scenegraphnode.h>
#include <modules/roverterrainrenderer/renderable/sitemanager.h>

#include <modules/roverterrainrenderer/renderable/roverterrain.h>

//#include <modules/roverterrainrenderer/shaders/fullsubsite_vs.glsl>
#include <modules/roverterrainrenderer/filehandler/roverpathfilereader.h>


#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___EXTRAXTHEIGHTMAP___H__
#define __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___EXTRAXTHEIGHTMAP___H__


namespace openspace {

namespace documentation { struct Documentation; }

class ExtractHeightMap {
public:
	struct TextureInformation{
		std::vector<std::string> fileNames; //might not need
		std::vector<PointCloudInfo> cameraInfoVector;	//might not be able to reach this file, and might not need it
	};

	//ExtractHeightMap(const ghoul::Dictionary& dictionary);
	static int extractSubsiteInformation(const ghoul::Dictionary dictionary);	//maybe these should be static??
	

private:

	std::vector<std::shared_ptr<Subsite>> samplePositionValues(const ghoul::Dictionary dictionary);
	std::vector<std::shared_ptr<Subsite>> writeToFile(const ghoul::Dictionary dictionary);

	std::string _modelPath;
	std::vector<std::shared_ptr<Subsite>> _subsites;
	std::vector<std::shared_ptr<SubsiteModels>> vectorOfsubsiteModels;
	std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___EXTRAXTHEIGHTMAP___H__
