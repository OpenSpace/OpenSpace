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

#ifndef __OPENSPACE_MODULE_BASE___MODELGEOMETRY___H__
#define __OPENSPACE_MODULE_BASE___MODELGEOMETRY___H__

#include <openspace/properties/propertyowner.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <memory>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace { class Renderable; }
namespace openspace::documentation { struct Documentation; }

namespace openspace::modelgeometry {

class ModelGeometry : public properties::PropertyOwner {
public:
    struct Vertex {
        GLfloat location[4];
        GLfloat tex[2];
        GLfloat normal[3];
    };

    static std::unique_ptr<ModelGeometry> createFromDictionary(
        const ghoul::Dictionary& dictionary
    );

    ModelGeometry(const ghoul::Dictionary& dictionary);
    virtual ~ModelGeometry() = default;

    virtual bool initialize(Renderable* parent);
    virtual void deinitialize();
    void render();

    virtual bool loadModel(const std::string& filename) = 0;
    void changeRenderMode(const GLenum mode);
    //bool getVertices(std::vector<Vertex>* vertexList);
    //bool getIndices(std::vector<int>* indexList);

    double boundingRadius() const;

    virtual void setUniforms(ghoul::opengl::ProgramObject& program);

    static documentation::Documentation Documentation();

protected:
    bool loadObj(const std::string& filename);
    bool loadCachedFile(const std::string& filename);
    bool saveCachedFile(const std::string& filename);

    GLuint _vaoID = 0;
    GLuint _vbo = 0;
    GLuint _ibo = 0 ;
    GLenum _mode = GL_TRIANGLES;

    double _boundingRadius = 0.0;

    std::vector<Vertex> _vertices;
    std::vector<int> _indices;
    std::string _file;
};

}  // namespace openspace::modelgeometry

#endif // __OPENSPACE_MODULE_BASE___MODELGEOMETRY___H__
