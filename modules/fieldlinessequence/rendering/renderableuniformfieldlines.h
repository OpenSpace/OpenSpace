/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEUNIFORMFIELDLINES___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEUNIFORMFIELDLINES___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>

namespace ghoul::opengl { class ProgramObject; }
namespace openspace::documentation { struct Documentation; }

namespace openspace {

    class Translation;

    /**
     * This is a class for a line that is drawn between two nodes in OpenSpace.
     */
    class RenderableUniformFieldlines : public Renderable {
    public:
        RenderableUniformFieldlines(const ghoul::Dictionary& dictionary);

        static documentation::Documentation Documentation();

        // Get the distance between the start and end node
        double distance() const;

        std::string start() const;
        std::string end() const;

    private:
        void initializeGL() override;
        void deinitializeGL() override;

        bool isReady() const override;
        void updateVertexData();
        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void validateNodes();

        void unbindGL();
        void bindGL();

        ghoul::opengl::ProgramObject* _program;
        /// The vertex attribute location for position
        /// must correlate to layout location in vertex shader
        const GLuint _locVertex = 0;
        GLuint _vaoId = 0;
        GLuint _vBufferId = 0;
        std::vector<float> _vertexArray;

        glm::dvec3 _startPos = glm::dvec3(0.0);
        glm::dvec3 _endPos = glm::dvec3(0.0);

        properties::StringProperty _start;
        properties::StringProperty _end;
        properties::Vec3Property _lineColor;
        properties::FloatProperty _lineWidth;

        // Simulated particles' color
        properties::Vec3Property _pFlowColor;
        // Toggle flow [ON/OFF]
        properties::BoolProperty _pFlowEnabled;
        // Group to hold the flow/particle properties
        properties::PropertyOwner _pFlowGroup;
        // Size of simulated flow particles
        properties::FloatProperty _pFlowParticleSize;
        // Size of simulated flow particles
        properties::FloatProperty _pFlowParticleSpacing;
        // Toggle flow direction [FORWARDS/BACKWARDS]
        properties::BoolProperty _pFlowReversed;
        // Speed of simulated flow
        properties::FloatProperty _pFlowSpeed;
    };

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEUNIFORMFIELDLINES___H__
