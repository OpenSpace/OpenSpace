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

#ifndef __RENDERABLEVOLUME_H__
#define __RENDERABLEVOLUME_H__

// open space includes
#include <openspace/rendering/renderable.h>

// ghoul includes
#include <ghoul/io/volume/rawvolumereader.h>

// Forward declare to minimize dependencies
namespace ghoul {
	namespace filesystem {
		class File;
	}
	namespace opengl {
		class Texture;
	}
}

namespace openspace {

class RenderableVolume: public Renderable {
public:
	// constructors & destructor
	RenderableVolume(const ghoul::Dictionary& dictionary);
	~RenderableVolume();
    
protected:
    ghoul::opengl::Texture* loadVolume(const std::string& filepath, const ghoul::Dictionary& hintsDictionary);
    glm::vec3 getVolumeOffset(const std::string& filepath, const ghoul::Dictionary& hintsDictionary);
    ghoul::RawVolumeReader::ReadHints readHints(const ghoul::Dictionary& dictionary);
    ghoul::opengl::Texture* loadTransferFunction(const std::string& filepath);

private:
};

} // namespace openspace

#endif
