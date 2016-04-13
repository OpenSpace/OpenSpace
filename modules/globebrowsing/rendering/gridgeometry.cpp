#include <modules/globebrowsing/rendering/gridgeometry.h>



namespace {
	const std::string _loggerCat = "GridGeometry";
}

namespace openspace {

GridGeometry::GridGeometry(
	unsigned int xRes,
	unsigned int yRes,
	Positions usePositions,
	TextureCoordinates useTextures,
	Normals useNormals)
	: Geometry(CreateElements(xRes, yRes), usePositions, useTextures, useNormals)
{
	if(_useVertexPositions){
		setVertexPositions(CreatePositions(xRes, yRes));
	}
	if (_useTextureCoordinates) {
		setVertexTextureCoordinates(CreateTextureCoordinates(xRes, yRes));
	}
	if (_useVertexNormals) {
		setVertexNormals(CreateNormals(xRes, yRes));
	}
	_xRes = xRes;
	_yRes = yRes;	
}

GridGeometry::~GridGeometry()
{

}

const unsigned int GridGeometry::xResolution() const {
	return _xRes;
}

const unsigned int GridGeometry::yResolution() const {
	return _yRes;
}

void GridGeometry::validate(unsigned int xRes, unsigned int yRes) {
	ghoul_assert(xRes > 0 && yRes > 0,
		"Resolution must be at least 1x1. (" << xRes << ", " << yRes << ")");
}

void GridGeometry::validateIndices(unsigned int x, unsigned int y) {
	validate(x, y);
	ghoul_assert(x < _xRes, "x index is outside range: x = " << x << "  xRes = " << _xRes);
	ghoul_assert(y < _yRes, "y index is outside range: y = " << y << "  yRes = " << _yRes);
}

inline size_t GridGeometry::numElements(unsigned int xRes, unsigned int yRes){
	return 3 * 2 * (xRes - 1)*(yRes - 1);
}

inline size_t GridGeometry::numVertices(unsigned int xRes, unsigned int yRes) {
	return xRes * yRes;
}

std::vector<GLuint> GridGeometry::CreateElements(unsigned int xRes, unsigned int yRes) {
	validate(xRes, yRes);

	std::vector<GLuint> elements;
	elements.reserve(numElements(xRes, yRes));
	for (unsigned int y = 0; y < yRes-1; y++) {
		for (unsigned int x = 0; x < xRes-1; x++) {

			// x    v01---v11   x ..
			//       |  /  |
			// x    v00---v10   x ..
			//
			// x	x     x     x ..
			// :    :     :     :

			GLuint v00 = (y + 0) * xRes + x + 0;
			GLuint v10 = (y + 0) * xRes + x + 1;
			GLuint v01 = (y + 1) * xRes + x + 0;
			GLuint v11 = (y + 1) * xRes + x + 1;

			// add upper triangle
			elements.push_back(v00);
			elements.push_back(v10);
			elements.push_back(v11);

			// add lower triangle
			elements.push_back(v00);
			elements.push_back(v11);
			elements.push_back(v01);
		}
	}

	return elements;
}

std::vector<glm::vec4> GridGeometry::CreatePositions(
	unsigned int xRes,
	unsigned int yRes,
	float xSize,
	float ySize,
	float xOffset,
	float yOffset) 
{
	validate(xRes, yRes);
	std::vector<glm::vec4> positions;
	positions.reserve(numVertices(xRes, yRes));

	// Copy from 2d texture coordinates and use as template to create positions
	std::vector<glm::vec2> templateTextureCoords = CreateTextureCoordinates(xRes, yRes);
	for (unsigned int i = 0; i < templateTextureCoords.size(); i++)
	{
		positions.push_back(glm::vec4(
			templateTextureCoords[i],
			0.0f,
			1.0f
			));
	}
	return positions;
}

std::vector<glm::vec2> GridGeometry::CreateTextureCoordinates(unsigned int xRes, unsigned int yRes){
	validate(xRes, yRes);
	std::vector<glm::vec2> textureCoordinates;
	textureCoordinates.reserve(numVertices(xRes, yRes));

	for (unsigned int y = 0; y < yRes; y++) {
		for (unsigned int x = 0; x < xRes; x++) {
			textureCoordinates.push_back(glm::vec2(
				static_cast<float>(x) / static_cast<float>(xRes - 1),
				static_cast<float>(y) / static_cast<float>(yRes - 1)
			));
		}
	}
	return textureCoordinates;
}

std::vector<glm::vec3> GridGeometry::CreateNormals(unsigned int xRes, unsigned int yRes) {
	validate(xRes, yRes);
	std::vector<glm::vec3> normals;
	normals.reserve(numVertices(xRes, yRes));

	for (unsigned int y = 0; y < yRes; y++) {
		for (unsigned int x = 0; x < xRes; x++) {
			normals.push_back(glm::vec3(0, 0, 1));
		}
	}

	return normals;
}

}// namespace openspace