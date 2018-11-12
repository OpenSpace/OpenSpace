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
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>


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

        bool isReady() const override;
        /* Returns an index for our filenames */
        int findFileIndexForCurrentTime(double time, std::vector<double> vec);
        /* Adds the signaldata to _vertexArray*/
        void pushSignalDataToVertexArray(SignalManager::Signal signal);
        /* Returns a position relative the current focus node */
        glm::dvec3 getCoordinatePosFromFocusNode(SceneGraphNode* node);
        /*Returns a position for a spacecraft*/
        glm::vec3 getSuitablePrecisionPositionForSceneGraphNode(std::string id);
        /* Returns a position for a station that has Earth as parent*/
        glm::vec3 getPositionForGeocentricSceneGraphNode(const char* id);
        /* Returns a color based on what site the station is located to */
        glm::vec3 getStationColor(std::string dishidentifier);

        /* The VBO layout of the vertex position */
        struct PositionVBOLayout {
            float x, y, z;
        };
        /* The VBO layout of the color */
        struct ColorVBOLayout {
            float r, g, b, a;
        };


        const char* _identifier = "Signals";
    protected:
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
            GLsizei count = 0;
            /// Local model matrix, for spacecraft, dependant on focusnode, used for rendering in camera space
            glm::dmat4 _localTransformSpacecraft = glm::dmat4(1.0);
            /// The vertex array object for this RenderInformation
            GLuint _vaoID = 0;
            /// The main vertex buffer object
            GLuint _vBufferID = 0;
        };

        /// Set of information about the main rendering parts
        RenderInformation _lineRenderInformation;

        /// Specifies the base color for the different sites
        std::vector<std::unique_ptr<properties::Vec3Property>> _siteColors;
        
        /// Maps a station identifier to a site location
        std::map<std::string, std::string> _stationToSite;

        /// Maps a site location to an index in the _siteColors property vector
        std::map<std::string, int> _siteToIndex;

        /// The attribute location for vertex position
        const GLuint _locVer = 0;
        /// The attribute location for vertex color
        const GLuint _locCol = 1;
        /// Specifies the number of components per generic vertex attribute
        const GLuint _sizeColorVal = 4;
        const GLuint _sizePosVal = 3;

        SceneGraphNode* _focusNode;

    private:

        /// Line width for the line rendering part
        properties::FloatProperty _lineWidth;
        /// Program object used to render the data stored in RenderInformation
        ghoul::opengl::ProgramObject* _programObject = nullptr;

        UniformCache(modelViewStation, modelViewSpacecraft, projection) _uniformCache;


        /*Checks if the current time is within a signal's start and endtime*/
        bool isSignalActive(double currentTime, std::string signalStartTime, std::string signalEndTime);


    };

} // namespace openspace

#endif // __OPENSPACE_MODULE_DSN___RENDERABLESIGNALS___H__
