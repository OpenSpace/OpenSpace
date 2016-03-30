#include <modules/planetbrowsing/rendering/geometry.h>
#include <ghoul/logging/logmanager.h>

namespace {
	const std::string _loggerCat = "Geometry";
}

namespace openspace
{

Geometry::Geometry(
	std::vector<unsigned int> elements,
	Positions usePositions,
	Textures useTextures,
	Normals useNormals) :
	_vaoID(0),
	_vertexBufferID(0),
	_elementBufferID(0),
	_usePositions(usePositions == Positions::Yes ? true : false),
	_useTextures(useTextures == Textures::Yes ? true : false),
	_useNormals(useNormals == Normals::Yes ? true : false)
{
	setElementData(elements);
}

Geometry::~Geometry() {
	glDeleteBuffers(1, &_vertexBufferID);
	glDeleteBuffers(1, &_elementBufferID);
	glDeleteVertexArrays(1, &_vaoID);
}

void Geometry::setPositionData(std::vector<glm::vec4> positions) {
	for (size_t i = 0; i < positions.size(); i++)
	{
		_vertexData[i].position[0] = static_cast<GLfloat>(positions[i].x);
		_vertexData[i].position[1] = static_cast<GLfloat>(positions[i].y);
		_vertexData[i].position[2] = static_cast<GLfloat>(positions[i].z);
		_vertexData[i].position[3] = static_cast<GLfloat>(positions[i].w);
	}
}

void Geometry::setTextureData(std::vector<glm::vec2> textures) {
	for (size_t i = 0; i < textures.size(); i++)
	{
		_vertexData[i].texture[0] = static_cast<GLfloat>(textures[i].s);
		_vertexData[i].texture[1] = static_cast<GLfloat>(textures[i].t);
	}
}

void Geometry::setNormalData(std::vector<glm::vec3> normals) {
	for (size_t i = 0; i < normals.size(); i++)
	{
		_vertexData[i].normal[0] = static_cast<GLfloat>(normals[i].x);
		_vertexData[i].normal[1] = static_cast<GLfloat>(normals[i].y);
		_vertexData[i].normal[2] = static_cast<GLfloat>(normals[i].z);
	}
}

void Geometry::setElementData(std::vector<unsigned int> elements) {
	for (size_t i = 0; i < elements.size(); i++)
	{
		_elementData[i] = static_cast<GLuint>(elements[i]);
	}
}

bool Geometry::initialize() {
	// Create VAO
	if (_vaoID == 0)
		glGenVertexArrays(1, &_vaoID);

	// Create VBOs
	if (_vertexBufferID == 0 && _vertexData.size() > 0) {
		glGenBuffers(1, &_vertexBufferID);
		if (_vertexBufferID == 0) {
			LERROR("Could not create vertex buffer");
			return false;
		}
	}
	if (_elementBufferID == 0 && _elementData.size() > 0) {
		glGenBuffers(1, &_elementBufferID);
		if (_elementBufferID == 0) {
			LERROR("Could not create vertex element buffer");
			return false;
		}
	}

	// First VAO setup
	glBindVertexArray(_vaoID);

	// Vertex buffer
	glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
	glBufferData(
		GL_ARRAY_BUFFER,
		_elementData.size() * sizeof(Vertex),
		&_elementData[0],
		GL_STATIC_DRAW);

	// Positions at location 0
	if (_usePositions) {
		glEnableVertexAttribArray(0);
		glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex),
			reinterpret_cast<const GLvoid*>(offsetof(Vertex, position)));
	}
	// Textures at location 1
	if (_useTextures) {
		glEnableVertexAttribArray(1);
		glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex),
			reinterpret_cast<const GLvoid*>(offsetof(Vertex, texture)));
	}
	// Normals at location 2
	if (_useNormals) {
		glEnableVertexAttribArray(2);
		glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex),
			reinterpret_cast<const GLvoid*>(offsetof(Vertex, normal)));
	}

	// Element buffer
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _elementBufferID);
	glBufferData(
		GL_ELEMENT_ARRAY_BUFFER,
		_elementData.size() * sizeof(GLint),
		&_elementData[0],
		GL_STATIC_DRAW);

	glBindVertexArray(0);
	return true;
}
void Geometry::render() const {
	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _elementBufferID);
	glDrawElements(GL_TRIANGLES, _elementData.size(), GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);
}

} // namespace openspace