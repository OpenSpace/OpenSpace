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

#ifndef __OPENSPACE_MODULE_DSN___RENDERABLECONE___H__
#define __OPENSPACE_MODULE_DSN___RENDERABLECONE___H__

#include <openspace/rendering/renderable.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/properties/vector/vec3property.h>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

    namespace documentation { struct Documentation; }

    class Translation;

    /**
     * This is a class for rendering cones
     **/
    class RenderableCone : public Renderable {

    public:
        RenderableCone(const ghoul::Dictionary& dictionary);
        virtual ~RenderableCone() = default;

        void initializeGL() override;
        void deinitializeGL() override;

        virtual void update(const UpdateData& data) override;
        virtual void createShaderProgram();
        virtual void fillVertexArrays();
        virtual void updateVertexAttributes();
        virtual float calculateBaseRadius();

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void addVertexToVertexArray(std::vector<float> &_vertexArray, glm::dvec3 position, glm::vec4 color);
        bool isReady() const override;

        /// Number of variables in _uniformCache
        static const GLuint uniformCacheSize = 2;
        /// Returns the documentation entries
        static documentation::Documentation Documentation();

    protected:
        const char* _identifier = "RenderableCone";
        /// The backend storage for the vertex buffer object containing all points for the
        /// lines to be rendered; position (3 floats) color (4 floats), 
        std::vector<float> _vertexBaseArray;
        std::vector<float> _vertexLateralSurfaceArray;

        /// The RenderInformation contains information filled in by the concrete subclasses to
        /// be used by this class.
        struct RenderInformation {
            /// The vertex array object for this RenderInformation
            GLuint _vaoID = 0;
            /// The main vertex buffer object
            GLuint _vBufferID = 0;
        };
        /// Set of information about the lateral surface of the cone
        RenderInformation _lateralSurfaceInfo;
        /// Set of information about the base of the cone
        RenderInformation _baseInfo;
        /// The vertex attribute location for position
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocVer = 0;
        /// The vertex attribute location for color
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocCol = 1;
        /// Specifies the number of components per generic vertex attribute
        const GLuint _sizeFourVal = 4;
        const GLuint _sizeThreeVal = 3;
        const GLuint _sizeOneVal = 1;

        /// Local model matrix, used for rendering in camera space
        glm::dmat4 _localTransform = glm::dmat4(1.0);
        /// The first element in the vertex buffer to be rendered
        GLint _first = 0;
        /// The number of values to be rendered
        GLsizei _count = 0;
        /// Program object used to render the data stored in RenderInformation
        ghoul::opengl::ProgramObject* _programObject = nullptr;
        /// Cache for uniform variables, update _uniformCacheSize accordingly
        UniformCache(modelView, projection) _uniformCache;

        glm::dvec3 getCoordinatePosFromAnchorNode(glm::dvec3 worldPos);
        void updateUniforms(const RenderData& data);

        /// The VBO layout of the vertex position
        struct PositionVBOLayout {
            float x, y, z;
        };
        /// The VBO layout of the color
        struct ColorVBOLayout {
            float r, g, b, a;
        };

        glm::dvec3 _apexPosition;
        glm::dvec3 _baseCenterDirection;
        std::vector<glm::dvec3> _baseVertices;
        glm::dvec3 _baseCenterPosition;

        // lightdays in meters
        double _unit = 2.59E13;

        properties::FloatProperty _height;
        properties::FloatProperty _radius;
        properties::IntProperty _resolution;
        properties::Vec3Property _color;
        properties::BoolProperty _wireframe;

        bool _showbase = true;
        bool _apexIsNodeAttached = true;
        bool _baseCenterIsNodeAttached = true;
        bool _directionIsReversed = false;
        glm::vec3 _defaultColor = { 0.4,0.4,0.4};

        std::string _apexNodeId, _baseDirNodeId = "";

    };

} // namespace openspace

#endif //__OPENSPACE_MODULE_DSN___RENDERABLECONE___H__
