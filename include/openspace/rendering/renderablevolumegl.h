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

#ifndef __RENDERABLEVOLUMEGL_H__
#define __RENDERABLEVOLUMEGL_H__

#include <openspace/rendering/renderablevolume.h>
#include <openspace/util/powerscaledcoordinate.h>

// Forward declare to minimize dependencies
namespace ghoul {
	namespace filesystem {
		class File;
	}
	namespace opengl {
		class ProgramObject;
		class Texture;
	}
}

namespace openspace {

class RenderableVolumeGL: public RenderableVolume {
public:
	RenderableVolumeGL(const ghoul::Dictionary& dictionary);
	~RenderableVolumeGL();
    
	bool initialize();
    bool deinitialize();

	virtual void render(const RenderData& data) override;
	virtual void update(const UpdateData& data) override;

private:
	ghoul::Dictionary _hintsDictionary;

    std::string _filename;

    std::string _transferFunctionName;
	std::string _volumeName;

    std::string _transferFunctionPath;
	std::string _samplerFilename;
    
    ghoul::filesystem::File* _transferFunctionFile;

	ghoul::opengl::Texture* _volume;
	ghoul::opengl::Texture* _transferFunction;

	GLuint _boxArray;
	ghoul::opengl::ProgramObject *_boxProgram;
	glm::vec3 _boxScaling;
	psc _pscOffset;
	float _w;
	GLint _MVPLocation, _modelTransformLocation, _typeLocation;
    
    bool _updateTransferfunction;
    int _id;
};

} // namespace openspace

#endif
