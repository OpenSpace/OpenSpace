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

#ifndef __RENDERABLECONSTELLATIONBOUNDS_H__
#define __RENDERABLECONSTELLATIONBOUNDS_H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/scalarproperty.h>

#include <ghoul/opengl/programobject.h>

#include <array>

namespace openspace {

class RenderableConstellationBounds : public Renderable {
public:
	RenderableConstellationBounds(const ghoul::Dictionary& dictionary);

	bool initialize() override;
	bool deinitialize() override;

	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;

private:
	struct ConstellationBound {
		std::string constellation;
		int startIndex;
		int nVertices;
	};

	bool loadFile();

	
	std::string _filename;

	ghoul::opengl::ProgramObject* _program;
	bool _programIsDirty;

	std::vector<ConstellationBound> _constellationBounds;
	
	typedef std::array<float, 3> Vertex;
	std::vector<Vertex> _vertexValues;

	properties::FloatProperty _distance;

	std::string _originReferenceFrame;
	glm::dmat3 _stateMatrix;

	GLuint _vao;
	GLuint _vbo;
};

} // namespace openspace

#endif // __RENDERABLECONSTELLATIONBOUNDS_H__

//#ifndef __RENDERABLESTARS_H__
//#define __RENDERABLESTARS_H__
//
//#include <openspace/rendering/renderable.h>
//#include <openspace/properties/stringproperty.h>
//#include <openspace/properties/optionproperty.h>
//
//#include <ghoul/opengl/programobject.h>
//#include <ghoul/opengl/texture.h>
//
//namespace openspace {
//
//class RenderableStars : public Renderable {
//public: 
//	RenderableStars(const ghoul::Dictionary& dictionary);
//
//	bool initialize() override;
//	bool deinitialize() override;
//
//	bool isReady() const override;
//
//	void render(const RenderData& data) override;
//	void update(const UpdateData& data) override;
//
//private:
//	enum ColorOption {
//		Color = 0,
//		Velocity = 1,
//		Speed = 2
//	};
//
//	void createDataSlice(ColorOption option);
//
//	bool loadData();
//	bool readSpeckFile();
//	bool loadCachedFile(const std::string& file);
//	bool saveCachedFile(const std::string& file) const;
//
//	properties::StringProperty _colorTexturePath;
//	ghoul::opengl::Texture* _texture;
//	bool _textureIsDirty;
//
//	properties::OptionProperty _colorOption;
//	bool _dataIsDirty;
//
//	properties::FloatProperty _spriteSize;
//
//	ghoul::opengl::ProgramObject* _program;
//	bool _programIsDirty;
//
//	std::string _speckFile;
//
//	std::vector<float> _slicedData;
//	std::vector<float> _fullData;
//	int _nValuesPerStar;
//
//	GLuint _vao;
//	GLuint _vbo;
//};
//
//} // namespace openspace
//
//#endif  // __RENDERABLESTARS_H__