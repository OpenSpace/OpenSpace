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

#ifndef __OPENSPACE_MODULE_DSN___RENDERABLESTATIONFOV___H__
#define __OPENSPACE_MODULE_DSN___RENDERABLESTATIONFOV___H__

#include <modules/dsn/rendering/renderablecone.h>

namespace openspace {

    namespace documentation { struct Documentation; }
    /**
     * This is a class for rendering a station field of view. 
     * It is based off of RenderableCone but includes some extra
     * functionality such as defining the base radius with an angle
     * and having a distance fade from the apex point.
     **/
    class RenderableStationFov : public RenderableCone{

    public:
        RenderableStationFov(const ghoul::Dictionary& dictionary);
        ~RenderableStationFov() = default;
        static documentation::Documentation Documentation();

        void updateVertexAttributes() override;
        void fillVertexArrays() override;
        void createShaderProgram() override;
        void addVertexToVertexArray(std::vector<float> &vertexArray, glm::dvec3 position,
                                    glm::vec4 color, float distance);
        float calculateBaseRadius() override;

        properties::FloatProperty _angle;
        properties::BoolProperty _distanceFade;
        properties::StringProperty _colorPropertyUri;
        properties::Property* _colorPropertyPointer = nullptr;

        bool _hasUriColor = false;

    private:
        /// The vertex attribute location for position
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocDist = 2;

    };

} // namespace openspace
#endif //__OPENSPACE_MODULE_DSN___RENDERABLESTATIONFOV___H__
