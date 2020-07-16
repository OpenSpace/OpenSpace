/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2020                                                               *
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


#include <openspace/rendering/renderable.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>
#include <atomic>

#include <modules/base/rendering/renderabletrail.h>


namespace openspace {
    class RenderableLightTravel : public Renderable {
    public:
        RenderableLightTravel(const ghoul::Dictionary& dictionary);

        void initializeGL() override;
        void deinitializeGL() override;

        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void update(const UpdateData& data) override;


    private:
        std::string _identifier;
        std::vector<float> _vertexArray;
        glm::vec3 _normalizedVector;
        // OpenGL Vertex Array Object
        GLuint _vertexArrayObject = 0;
        // OpenGL Vertex Buffer Object containing the vertex positions
        GLuint _vertexPositionBuffer = 0;
        // OpenGL Vertex Buffer Object containing the Flux values used for coloring
        // the nodes
        GLuint _vertexColorBuffer = 0;
        // OpenGL Vertex Buffer Object containing the positions to filter the nodes
        GLuint _vertexFilteringBuffer = 0;
        // OpenGL Vertex Buffer Object containing the index of nodes
        GLuint _vertexindexBuffer = 0;

        enum class RenderMethod : int {
            LineStrip = 0,
            Lines = 1,
            Points = 2,
            Sprites = 3
        };

        std::vector<glm::vec3> positions;
        float _Timesincestart = -1.f;
        double _triggerTime;
        double _endTime;
        bool _needPositionUpdate = true;
        properties::FloatProperty _lightspeed;
        properties::FloatProperty _lineWidth;
        properties::OptionProperty _rendermode;
        properties::FloatProperty _pointSize;
        properties::IntProperty _timeStep;
        properties::FloatProperty _distanceFactor;
        // Uniform stream Color
        properties::Vec4Property _pDefaultColor;
        // Uniform stream Color
        properties::Vec4Property _pLightColor;
        std::unique_ptr<ghoul::Dictionary> _dictionary;
        std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;

        std::unique_ptr<ghoul::opengl::Texture> _spriteTexture;
        std::unique_ptr<ghoul::filesystem::File> _spriteTextureFile;

        GLuint _quad = 0;
       // void createPlane();

        double calculateEndTime(const double starttime, const glm::vec3 startpos, const glm::vec3 endpos);
    };
}
