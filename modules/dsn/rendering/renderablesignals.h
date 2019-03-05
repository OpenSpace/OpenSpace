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

#ifndef __OPENSPACE_MODULE_DSN___RENDERABLESIGNALS___H__
#define __OPENSPACE_MODULE_DSN___RENDERABLESIGNALS___H__

#include <openspace/rendering/renderable.h>
#include <modules/dsn/managers/signalmanager.h>
#include <modules/dsn/dsnmodule.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

    namespace documentation { struct Documentation; }

    class Translation;

    /**
     * This is the base class for signals that are sent out from an emitter
     * (such as a station or spacecraft dish).
     * Signals can be rendered with varying colors and line thicknesses.
     *
     * The endpositions for each signal line is provided by checking for
     * ScenegraphNode positions in OpenSpace. These positions have different origins
     * depending on if they are representing a station position or a spacecraft position.
     * The reason for this is because of precision problems occurring due to using very
     * large numbers representing the distances in space.
     **/
    class RenderableSignals : public Renderable {

    public:
        RenderableSignals(const ghoul::Dictionary& dictionary);
        ~RenderableSignals() = default;

        void extractData(std::unique_ptr<ghoul::Dictionary> &dictionary);
        void initializeGL() override;
        void deinitializeGL() override;
        void update(const UpdateData& data) override;
        /*
         * The render method will set up the shader information and then render the
         * information contained in the the \c _lineRenderInformation,
         * using the provided \p data
         * \param data The data that is necessary to render this Renderable
         */
        void render(const RenderData& data, RendererTasks& rendererTask) override;

        void updateVertexAttributes();
        bool isReady() const override;
        /* Adds the signaldata to _vertexArray*/
        void pushSignalDataToVertexArray(SignalManager::Signal signal);
        /* Returns position relative to the current anchor node */
        glm::dvec3 getCoordinatePosFromAnchorNode(glm::dvec3 worldPos);
        /*Returns a position for a spacecraft */
        glm::dvec3 getPrecisionPositionForNode(std::string id);
        /* Returns a position for a station placed on Earth with a set height*/
        glm::dvec3 getPrecisionPositionForStationNode(std::string id);
        /* Returns a color based on what site the station is located to */
        glm::vec4 getStationColor(std::string dishidentifier);
        /* Returns a distance between two scenegraphnodes */
        double getDistance(std::string nodeIdA, std::string nodeIdB);

        /// Number of variables in _uniformCache
        static const GLuint uniformCacheSize = 7;


    protected:
        const char* _identifier = "Signals";

        /// Returns the documentation entries
        static documentation::Documentation Documentation();

        /// The backend storage for the vertex buffer object containing all points for the
        /// lines to be rendered; position (3 floats) color (4 floats), 
        std::vector<float> _vertexArray;

        /// The index array that is potentially used in the draw call. If this is empty, no
        /// element draw call is used.
        std::vector<unsigned int> _indexArray;

        /// The Translation object that provides the position of the individual trail points
        std::unique_ptr<Translation> _translation;

        /// The RenderInformation contains information filled in by the concrete subclasses to
        /// be used by this class.
        struct RenderInformation {
            /// The first element in the vertex buffer to be rendered
            GLint first = 0;
            /// The number of values to be rendered
            GLsizei countLines = 0;
            /// Local model matrix, dependant on anchor node, used for rendering in camera space
            glm::dmat4 _localTransform = glm::dmat4(1.0);
            /// The vertex array object for this RenderInformation
            GLuint _vaoID = 0;
            /// The main vertex buffer object
            GLuint _vBufferID = 0;
        };

        /// Set of information about the main rendering parts
        RenderInformation _lineRenderInformation;

        /// Specifies the base color for the different sites
        std::vector<std::unique_ptr<properties::Vec4Property>> _siteColors;
        
        /// Maps a station identifier to a site location
        std::map<std::string, std::string> _stationToSite;

        /// Maps a station identifier to its size
        std::map<std::string, float> _stationToSize;

        /// Maps a site location to an index in the _siteColors property vector
        std::map<std::string, int> _siteToIndex;

        /// The vertex attribute location for position
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocVer = 0;
        /// The vertex attribute location for color
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocCol = 1;
        /// The vertex attribute location for distance from signal start position
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocDist = 2;
        /// The vertex attribute location time since signal started sending
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocTimeSinceStart = 3;
        /// The vertex attribute location for total transmission time
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocTransmissionTime = 4;
        /// The vertex attribute location for signal light travle time
        /// must correlate to layout location in vertex shader
        const GLuint _vaLocLightTravelTime = 5;

        /// Specifies the number of components per generic vertex attribute
        const GLuint _sizeFourVal = 4;
        const GLuint _sizeThreeVal = 3;
        const GLuint _sizeOneVal = 1;

    private:

        void updateUniforms(const RenderData& data);
        void addVertexToVertexArray(glm::dvec3 position, glm::vec4 color, double distance,
                                    double timeSinceStart, double transmissionTime, double lightTravelTime);

        /* The VBO layout of the vertex position */
        struct PositionVBOLayout {
            float x, y, z;
        };
        /* The VBO layout of the color */
        struct ColorVBOLayout {
            float r, g, b, a;
        };
        /* The summated VBO layout of all the one value float attributes */
        struct FloatsVBOLayout {
            float distance, timeSinceStart, transmissionTime, lightTravelTime;
        };

        /// Number of variables in FloatsVBOLayout
        static const int _floatsVBOSize = 4;
        /// Size buffer for signal vector
        int _signalSizeBuffer = 10;

        /// Line width for the line rendering part
        properties::FloatProperty _lineWidth;
        /// Opacity for the base line
        properties::FloatProperty _baseOpacity;
        /// Speed factor for segments within the transmission time
        properties::FloatProperty _flowSpeedFactor;
        /// Size factor for segments within the transmission time
        properties::FloatProperty _segmentSizeFactor;
        /// Size factor for the spacing between segments within the transmission time
        properties::FloatProperty _spacingSizeFactor;
        /// Edge fading factor for a segment within transmission time
        properties::FloatProperty _fadeFactor;

        /// Program object used to render the data stored in RenderInformation
        ghoul::opengl::ProgramObject* _programObject = nullptr;
        /// Cache for uniform variables, update _uniformCacheSize accordingly
        UniformCache(modelView, projection, baseOpacity, flowSpeedFactor, 
                    segmentSizeFactor, spacingSizeFactor, fadeFactor) _uniformCache;

        /*Checks if the current time is within a signal's start and endtime*/
        bool isSignalActive(double currentTime, SignalManager::Signal signal);

    };

} // namespace openspace

#endif // __OPENSPACE_MODULE_DSN___RENDERABLESIGNALS___H__
