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

#include <modules/marsrover/heighthandler/extractheightmap.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>	//use ghoul to write to file?
#include <openspace/engine/openspaceengine.h>

#include <ghoul/fmt.h>

namespace {

	const char* _loggerCat = "ExtractHeightMap";

} // namespace
namespace ghoul::filesystem { class File; } //maybe not needed?
namespace openspace {

//************************Call these functions inside marsrovermodule****************************************//


//maybe have function as void and just write to file? yeeees?
    int extractSubsiteInformation(const ghoul::Dictionary dictionary) {
    	
        //RenderEngine& renderEngine = OsEng.renderEngine();
        //_programObject = renderEngine.buildRenderProgram("RenderableMarsrover",
        //    absPath("${MODULE_MARSROVER}/shaders/heightmap_vs.glsl"),
        //    absPath("${MODULE_MARSROVER}/shaders/heightmap_fs.glsl")
        //);

        //std::vector<std::shared_ptr<Subsite>> pos;
        //LERROR(fmt::format("position KRIIIIISTIN    '{}'", pos));
        int pos = 1;
        return pos;

    }

    std::vector<std::shared_ptr<Subsite>> samplePositionValues(std::vector<std::shared_ptr<Subsite>> vector) {

    	//look at z values and choose the largest one and eliminate the rest of them
    	std::vector<std::shared_ptr<Subsite>> hej;

    	return hej;
    }


    std::vector<std::shared_ptr<Subsite>> writeToFile(const ghoul::Dictionary dictionary) {

    	std::vector<std::shared_ptr<Subsite>> hej;

    	return hej;
    }



} // namespace openspace
