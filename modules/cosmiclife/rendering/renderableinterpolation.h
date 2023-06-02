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

#ifndef __OPENSPACE_MODULE_COSMICLIFE___RENDERABLEINTERPOLATION___H__
#define __OPENSPACE_MODULE_COSMICLIFE___RENDERABLEINTERPOLATION___H__

#include <openspace/rendering/renderable.h>

#include <modules/space/speckloader.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <filesystem>
 // from billboard cloud
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec2property.h>
#include <functional>
#include <unordered_map>

namespace ghoul::filesystem { class File; }
namespace ghoul::fontrendering { class Font; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
}

namespace openspace {

    namespace documentation { struct Documentation; }

    class RenderableInterpolation : public Renderable {
    public:
        explicit RenderableInterpolation(const ghoul::Dictionary& dictionary);
        ~RenderableInterpolation() = default;

        void initialize() override;
        void initializeGL() override;
        void deinitializeGL() override;

        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void update(const UpdateData& data) override;

        static documentation::Documentation Documentation();

    private:
        struct Point {
            float x;
            float y;
            float z;
        };

        struct Vertex {
            float location[3];
        };

        struct DistancePoints {
            float distance;
            speck::Dataset::Entry p1;
            speck::Dataset::Entry p2;

            // Overload the less-than operator to compare ValueIndex objects
            bool operator<(const DistancePoints& other) const {
                // Compare values in reverse order for max heap
                return distance < other.distance;
            }
        };

        struct DistanceHolders {
            std::string dataset1;
            std::string dataset2;
            std::vector<Vertex> v1;
            std::vector<Vertex> v2;
        };

        std::vector<DistanceHolders> _distanceHolders;

        std::vector<float> createDataSlice(speck::Dataset& dataset, const RenderData& data);
        void renderPoints(const RenderData& data, const glm::dmat4& modelMatrix,
            const glm::dvec3& orthoRight, const glm::dvec3& orthoUp);
        speck::Dataset interpolationFunc(const speck::Dataset& d1, const speck::Dataset& d2, float iv);
        speck::Dataset::Entry interpol(const speck::Dataset::Entry& e1, const speck::Dataset::Entry& e2, float iv);
        void sort(const speck::Dataset& d1, const speck::Dataset& d2);

        void initializeLines();
        std::vector<float> computeDistances(const speck::Dataset::Entry& e1, const std::vector<speck::Dataset::Entry>& d1);
        std::vector<speck::Dataset::Entry> findPointsOfInterest(const speck::Dataset::Entry& e, const speck::Dataset& d);
        std::pair<std::vector<RenderableInterpolation::Vertex>, std::vector<RenderableInterpolation::Vertex>> ComputeOutliers(const speck::Dataset& d1, const speck::Dataset& d2);
        void renderLines(const RenderData& data);
        float fadeObjectDependingOnDistance(const RenderData& data, const speck::Dataset::Entry& e);
        void updateRenderData(const RenderData& data);
        void storeDistanceHolders(std::map<std::string, speck::Dataset> const & d);

        // bool variables
        bool _dataIsDirty = true;
        bool _textColorIsDirty = true;
        bool _hasSpriteTexture = false;
        bool _spriteTextureIsDirty = true;
        bool _hasColorMapFile = false;
        bool _isColorMapExact = false;
        bool _hasDatavarSize = false;
        bool _lineDataIsDirty1 = false;
        bool _lineDataIsDirty2 = false;

        GLuint _pTexture = 0;

        properties::FloatProperty _scaleFactor;
        properties::Vec3Property _pointColor;
        properties::Vec3Property _frameColor;
        properties::StringProperty _spriteTexturePath;
        properties::BoolProperty _useFade;
        properties::FloatProperty _maxThreshold;
        properties::BoolProperty _pixelSizeControl;
        properties::OptionProperty _colorOption;
        properties::Vec2Property _optionColorRangeData;
        properties::OptionProperty _datavarSizeOption;
        properties::Vec2Property _billboardMinMaxSize;
        properties::FloatProperty _correctionSizeEndDistance;
        properties::FloatProperty _correctionSizeFactor;
        properties::BoolProperty _useLinearFiltering;
        properties::TriggerProperty _setRangeFromData;
        properties::OptionProperty _renderOption;

        properties::FloatProperty _interpolationValue;
        properties::OptionProperty _dataSetOneOption;
        properties::OptionProperty _dataSetTwoOption;
        properties::StringProperty _directoryPath;
        properties::BoolProperty _computeDistances;
        properties::FloatProperty _percentageOfLines;


        std::map<std::string, std::string> _filePaths; 
        std::optional<std::string> _uniqueSpecies;

        std::vector<Point> _MDS_points;
        std::vector<Point> _Umap_points;

        std::vector<Vertex> _vertices1;
        std::vector<Vertex> _vertices2;

        ghoul::opengl::Texture* _spriteTexture = nullptr;
        ghoul::opengl::ProgramObject* _program = nullptr;
        ghoul::opengl::ProgramObject* _programL = nullptr;


        // variables that are sent to the shaders
        UniformCache(
            cameraViewProjectionMatrix, modelMatrix, cameraPos, cameraLookup, renderOption,
            minBillboardSize, maxBillboardSize, correctionSizeEndDistance,
            correctionSizeFactor, color, alphaValue, scaleFactor, up, right,
            screenSize, spriteTexture, hasColormap, enabledRectSizeControl, hasDvarScaling, frameColor, useGamma
        ) _uniformCache;

        // font variable from ghoul library
        std::shared_ptr<ghoul::fontrendering::Font> _font;

        // String variables
        std::string _colorMapFile;
        std::string _colorOptionString;
        std::string _datavarSizeOptionString;

        // distance default unit -- change?
        DistanceUnit _unit = DistanceUnit::Parsec;

        // speck files
        //speck::Dataset _dataset;
        std::map<std::string, speck::Dataset> _datasets;
        speck::Dataset _interpolationDataset;
        speck::Dataset _dataSetOne;
        speck::Dataset _dataSetTwo;

        speck::ColorMap _colorMap;

        // range data, do we need conversion map?
        std::vector<glm::vec2> _colorRangeData;
        std::unordered_map<int, std::string> _optionConversionMap;
        std::unordered_map<int, std::string> _optionConversionSizeMap;

        glm::dmat4 _transformationMatrix = glm::dmat4(1.0);

        GLuint _vao = 0;
        GLuint _vbo = 0;

        GLuint _vaoLines = 0;
        GLuint _vboLines = 0;
    };

} // namespace openspace

#endif // __OPENSPACE_MODULE_COSMICLIFE___RENDERABLEINTERPOLATION___H__
