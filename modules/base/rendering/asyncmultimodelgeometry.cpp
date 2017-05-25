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

#include <modules/base/rendering/asyncmultimodelgeometry.h>
#include <ghoul/logging/logmanager.h>
#include "ghoul/io/model/modelreadermultiformat.h"

namespace {
	const std::string _loggerCat = "AsynchMultiModelGeometry";
}

namespace openspace {

namespace modelgeometry {
AsyncMultiModelGeometry::AsyncMultiModelGeometry(const ghoul::Dictionary& dictionary)
	: ModelGeometry(dictionary)
{
	loadObjWithoutCaching(_file);
}

bool AsyncMultiModelGeometry::initialize(Renderable* parent) {
	glGenVertexArrays(1, &_vaoID);
	glBindVertexArray(_vaoID);
	glGenBuffers(1, &_ibo);
	glGenBuffers(1, &_vbo);

	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);

	glBindBuffer(GL_ARRAY_BUFFER, _vbo);
	glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, location)));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, tex)));
	glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex),
		reinterpret_cast<const GLvoid*>(offsetof(Vertex, normal)));

	size_t vboBufferSize{ _vertices.size() * sizeof(Vertex) };
	glBufferData(GL_ARRAY_BUFFER, vboBufferSize, nullptr, GL_STATIC_DRAW);

	_vertexBufferData = new Vertex[_vertices.size()];
	_vertexBufferData = (Vertex*)glMapBuffer(GL_ARRAY_BUFFER, GL_READ_WRITE);

	size_t iboBufferSize{ _indices.size() * sizeof(int) };
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, iboBufferSize, nullptr, GL_STATIC_DRAW);

	_indexBufferData = new int[_indices.size()];
	_indexBufferData = (int*)glMapBuffer(GL_ELEMENT_ARRAY_BUFFER, GL_READ_WRITE);

	glBindVertexArray(0);
	return true;
}

void AsyncMultiModelGeometry::deinitialize() {
	ModelGeometry::deinitialize();
}

void AsyncMultiModelGeometry::uploadData() {
	for (int i = 0; i < _vertices.size(); i++) {
		_vertexBufferData[i] = _vertices.at(i);
	}

	for (int k = 0; k < _indices.size(); k++) {
		_indexBufferData[k] = _indices.at(k);
	}
}

void AsyncMultiModelGeometry::unmapBuffers() {
	glBindBuffer(GL_ARRAY_BUFFER, _vbo);
	glUnmapBuffer(GL_ARRAY_BUFFER);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _ibo);
	glUnmapBuffer(GL_ELEMENT_ARRAY_BUFFER);
}

bool AsyncMultiModelGeometry::loadModel(const std::string& filename) {
	ghoul::io::ModelReaderMultiFormat modelReader;

	std::vector<ghoul::io::ModelReaderBase::Vertex> vertices;
	std::vector<int> indices;

	modelReader.loadModel(filename, vertices, indices);

	_vertices.reserve(vertices.size());
	for (const auto & v : vertices) {
		psc p = PowerScaledCoordinate::CreatePowerScaledCoordinate(
			v.location[0],
			v.location[1],
			v.location[2]
		);

		Vertex vv;
		memcpy(vv.location, v.location, sizeof(GLfloat) * 3);
		vv.location[3] = 1.0;
		//memcpy(vv.location, glm::value_ptr(p.vec4()), sizeof(GLfloat) * 4);
		memcpy(vv.tex, v.tex, sizeof(GLfloat) * 2);
		memcpy(vv.normal, v.normal, sizeof(GLfloat) * 3);
		_vertices.push_back(vv);
	}

	_indices.resize(indices.size());
	std::copy(indices.begin(), indices.end(), _indices.begin());

	return true;
}

} // namespace globebrowsing
} // namepsace openspace