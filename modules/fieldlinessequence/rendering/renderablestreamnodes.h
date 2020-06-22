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

namespace { enum class SourceFileType; }

namespace openspace {

    class RenderableStreamNodes : public Renderable {
    public:
        RenderableStreamNodes(const ghoul::Dictionary& dictionary);

        //these two are needed for startup and close i think. 
        void initializeGL() override;
        void deinitializeGL() override;
        const std::vector<GLsizei>& lineCount() const;
        const std::vector<GLint>& lineStart() const;

        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void update(const UpdateData& data) override;
        void updateActiveTriggerTimeIndex(double currentTime);

        //const std::vector<GLsizei>& lineCount() const;
        //const std::vector<GLint>& lineStart() const;

    private:
        std::vector<GLsizei> _lineCount;
        std::vector<GLint> _lineStart;
        // ------------------------------------- ENUMS -------------------------------------//
        // Used to determine if lines should be colored UNIFORMLY or by an extraQuantity
        enum class ColorMethod : int {
            Uniform = 0,
            ByFluxValue = 1
        };
        enum class ScalingMethod : int {
            Flux = 0,
            RFlux = 1,
            R2Flux = 2,
            log10RFlux = 3,
            lnRFlux = 4
        };
        enum class NodeskipMethod : int {
            Uniform = 0,
            Flux = 1,
            Radius = 2
        };

        UniformCache(streamColor, usingParticles, nodeSize, nodeSizeLargerFlux, thresholdFlux)
            _uniformCache;

        // ------------------------------------ STRINGS ------------------------------------//
        std::string _identifier;    /// Name of the Nod

    // ------------------------------------- FLAGS -------------------------------------//
    // False => states are stored in RAM (using 'in-RAM-states'), True => states are
    // loaded from disk during runtime (using 'runtime-states')
        bool _loadingStatesDynamically = true;

        // False => the previous frame's state should still be shown
        bool _needsUpdate = false;
        // Used for 'runtime-states'. True when finished loading a new state from disk on
        // another thread.
        std::atomic_bool _newStateIsReady = false;
        bool _isLoadingStateFromDisk = false;
        bool _mustLoadNewStateFromDisk = true;
        // --------------------------------- NUMERICALS ----------------------------------- //
        // In setup it is used to scale JSON coordinates. During runtime it is used to scale
        // domain limits.
        float _scalingFactor = 1.f;

        // Active index of _states. If(==-1)=>no state available for current time. Always the
        // same as _activeTriggerTimeIndex if(_loadingStatesDynamically==true), else
        // always = 0
        int _activeStateIndex = -1;
        
        // Active index of _startTimes
        int _activeTriggerTimeIndex = -1;
        // Estimated end of sequence.
        double _sequenceEndTime;
        // Number of states in the sequence
        size_t _nStates = 274;

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
        // ---------------------------------- Properties ---------------------------------- //
        // Group to hold the color properties
        properties::PropertyOwner _pColorGroup;
        // Uniform/transfer function/topology? //////////////////////?
        properties::OptionProperty _pColorMode;
        // Scaling options
        properties::OptionProperty _pScalingmethod;
        // Nodeskipping options
        properties::OptionProperty _pNodeskipMethod;
        // Uniform stream Color
        properties::Vec4Property _pStreamColor;
        // Index of the flux value to color lines by
        properties::OptionProperty _pColorFlux;
        // Color table/transfer function min
        //properties::StringProperty _pColorFluxMin;
        // Color table/transfer function max
        //properties::StringProperty _pColorFluxMax;
        // Color table/transfer function for "By Flux value" coloring
        properties::StringProperty _pColorTablePath;
        // Toggle flow [ON/OFF]
        properties::BoolProperty _pStreamsEnabled;
        // Group to hold the flow/particle properties
        properties::PropertyOwner _pStreamGroup;

        properties::PropertyOwner _pNodesamountGroup;
        // Size of simulated node particles
        properties::FloatProperty _pNodeSize;
        // Size of simulated node particles with larger flux value
        properties::FloatProperty _pNodeSizeLargerFlux;
        /// Line width for the line rendering part
        properties::FloatProperty _pLineWidth;
        ////////////////
        properties::Vec2Property _pColorTableRange;
        ////////////////
        properties::Vec2Property _pDomainZ;
        /// ///////////
        properties::FloatProperty _pThresholdFlux;
        // Filtering nodes within a range
        properties::FloatProperty _pFiltering;
        // Filtering nodes with a upper range
        properties::FloatProperty _pFilteringUpper;
        //Amount of nodes to show
        properties::IntProperty _pAmountofNodes;
        ////////////////
        properties::FloatProperty _pFluxColorAlpha;

        properties::FloatProperty _pFluxNodeskipThreshold;

        properties::FloatProperty _pRadiusNodeSkipThreshold;

        properties::IntProperty _pDefaultNodeSkip;

        // initialization
        std::vector<std::string> _sourceFiles;

        // ------------------------------------ VECTORS ----------------------------------- //
        // Paths to color tables. One for each 'extraQuantity'
        std::vector<std::string> _colorTablePaths;
        // Values represents min & max values represented in the color table
        std::vector<glm::vec2> _colorTableRanges;
        // Contains the _triggerTimes for all FieldlineStates in the sequence
        std::vector<double> _startTimes;
        // Contains vertexPositions
        std::vector<glm::vec3> _vertexPositions;
        // Contains vertex flux values for color
        std::vector<float> _vertexColor;
        // Contains vertexRedius
        std::vector<float> _vertexRadius;
        // Contains VertexIndex
        std::vector<int> _vertexIndex;

        std::vector<std::vector<glm::vec3>> _statesPos;
        std::vector<std::vector<float>> _statesColor;
        std::vector<std::vector<float>> _statesRadius;
        std::vector<std::vector<int>> _statesIndex;
        // ----------------------------------- POINTERS ------------------------------------//
        // The Lua-Modfile-Dictionary used during initialization
        std::unique_ptr<ghoul::Dictionary> _dictionary;

        std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;
        // Transfer function used to color lines when _pColorMethod is set to BY_QUANTITY
        std::unique_ptr<TransferFunction> _transferFunction;

        // --------------------- FUNCTIONS USED DURING INITIALIZATION --------------------- //    
        bool extractMandatoryInfoFromDictionary(SourceFileType& sourceFileType);
        //void extractOptionalInfoFromDictionary(std::string& outputFolderPath);
        void definePropertyCallbackFunctions();
        bool loadJsonStatesIntoRAM(const std::string& outputFolder);
        bool extractJsonInfoFromDictionary(fls::Model& model);
        void extractOptionalInfoFromDictionary(std::string& outputFolderPath);
        //std::vector<std::string> LoadJsonfile(const std::string& filename);
        std::vector<std::string> LoadJsonfile(std::string filepath);
        void setupProperties();
        void updatePositionBuffer();
        void updateVertexColorBuffer();
        void updateVertexFilteringBuffer();
        void updateVertexIndexBuffer();
        void extractTriggerTimesFromFileNames();
        void computeSequenceEndTime();
        void setModelDependentConstants();

        bool LoadfilesintoRam();

            // ------------------------- FUNCTIONS USED DURING RUNTIME ------------------------ //
  
    };




}
