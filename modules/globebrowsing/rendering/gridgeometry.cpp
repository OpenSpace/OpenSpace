#include <modules/globebrowsing/rendering/gridgeometry.h>



namespace {
	const std::string _loggerCat = "GridGeometry";
}

namespace openspace {

GridGeometry::GridGeometry(unsigned int xRes, unsigned int yRes, Positions usePositions, TextureCoordinates useTextures, Normals useNormals)
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
	//LDEBUG("CreateElements");
	
	validate(xRes, yRes);

	std::vector<GLuint> elements;
	elements.reserve(numElements(xRes, yRes));
	for (unsigned int y = 0; y < yRes-1; y++) {
		for (unsigned int x = 0; x < xRes-1; x++) {

			// x    v00---v10   x ..
			//       |  /  |
			// x    v01---v11   x ..
			//
			// x	x     x     x ..
			// :    :     :     :

			GLuint v00 = (y + 0)*xRes + x + 0;	GLuint v10 = (y + 0)*xRes + x + 1;
			GLuint v01 = (y + 1)*xRes + x + 0;	GLuint v11 = (y + 1)*xRes + x + 1;

			// add upper triangle
			elements.push_back(v00);
			elements.push_back(v10);
			elements.push_back(v01);
			
			// add lower triangle
			elements.push_back(v01);
			elements.push_back(v10);
			elements.push_back(v11);

			//LDEBUG(v00 << ", " << v10 << ", " << v01);
			//LDEBUG(v01 << ", " << v10 << ", " << v11);
		}
	}

	return elements;
}

std::vector<glm::vec4> GridGeometry::CreatePositions(unsigned int xRes, unsigned int yRes,
	float xSize, float ySize, float xOffset, float yOffset) 
{
	//LDEBUG("CreatePositions");

	validate(xRes, yRes);
	std::vector<glm::vec4> positions;
	positions.reserve(numVertices(xRes, yRes));

	glm::vec2 delta(
		xSize / static_cast<float>(xRes-1),
		ySize / static_cast<float>(yRes-1)
	);

	for (unsigned int y = 0; y < yRes; y++) {
		for (unsigned int x = 0; x < xRes; x++) {
			positions.push_back(glm::vec4(
				static_cast<float>(x) * delta.x + xOffset,
				static_cast<float>(y) * delta.y + yOffset,
				0.0f,
				1.0f
			));
			//const glm::vec4 v = positions.back();
			//LDEBUG(v.x << ", " << v.y);
		}
	}

	return positions;
}


std::vector<glm::vec2> GridGeometry::CreateTextureCoordinates(unsigned int xRes, unsigned int yRes){
	//LDEBUG("CreateTextureCoordinates");

	validate(xRes, yRes);
	std::vector<glm::vec2> textureCoordinates;
	textureCoordinates.reserve(numVertices(xRes, yRes));

	glm::vec2 delta(
		1.0f / static_cast<float>(xRes - 1),
		1.0f / static_cast<float>(yRes - 1)
	);

	for (unsigned int y = 0; y < yRes; y++) {
		for (unsigned int x = 0; x < xRes; x++) {
			textureCoordinates.push_back(glm::vec2(
				static_cast<float>(x) * delta.x,
				static_cast<float>(y) * delta.y
			));
			//const glm::vec2 v = textureCoordinates.back();
			//LDEBUG(v.s << ", " << v.t);
		}
	}

	return textureCoordinates;
}



std::vector<glm::vec3> GridGeometry::CreateNormals(unsigned int xRes, unsigned int yRes) {
	//LDEBUG("CreateNormals");

	validate(xRes, yRes);
	std::vector<glm::vec3> normals;
	normals.reserve(numVertices(xRes, yRes));

	for (unsigned int y = 0; y < yRes; y++) {
		for (unsigned int x = 0; x < xRes; x++) {
			normals.push_back(glm::vec3(0, 0, 1));

			//const glm::vec3 v = normals.back();
			//LDEBUG(v.x << ", " << v.y << ", " << v.z);
		}
	}

	return normals;
}


}// namespace openspace