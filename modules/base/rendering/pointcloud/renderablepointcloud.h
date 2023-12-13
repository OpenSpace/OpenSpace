/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEPOINTCLOUD___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEPOINTCLOUD___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/rendering/colormappingcomponent.h>
#include <openspace/rendering/labelscomponent.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <functional>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

/**
 * This class describes a point cloud renderable that can be used to draw billboraded
 * points based on a data file with 3D positions.  Alternatively the points can also
 * be colored and sized based on a separate column in the data file.
 */
class RenderablePointCloud : public Renderable {
public:
    explicit RenderablePointCloud(const ghoul::Dictionary& dictionary);
    ~RenderablePointCloud() override = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
    int nAttributesPerPoint() const;
    void updateBufferData();
    void updateSpriteTexture();

    /// Find the index of the currently chosen color parameter in the dataset
    int currentColorParameterIndex() const;
    /// Find the index of the currently chosen size parameter in the dataset
    int currentSizeParameterIndex() const;

    std::vector<float> createDataSlice();

    virtual void bindTextureForRendering() const;

    float computeDistanceFadeValue(const RenderData& data) const;

    void renderBillboards(const RenderData& data, const glm::dmat4& modelMatrix,
        const glm::dvec3& orthoRight, const glm::dvec3& orthoUp, float fadeInVariable);

    bool _dataIsDirty = true;
    bool _spriteTextureIsDirty = true;

    bool _hasSpriteTexture = false;
    bool _hasDataFile = false;
    bool _hasColorMapFile = false;
    bool _hasDatavarSize = false;
    bool _hasLabels = false;

    struct SizeSettings : properties::PropertyOwner {
        explicit SizeSettings(const ghoul::Dictionary& dictionary);

        struct SizeMapping : properties::PropertyOwner {
            SizeMapping();
            properties::BoolProperty enabled;
            properties::OptionProperty parameterOption;
        };
        SizeMapping sizeMapping;

        properties::FloatProperty scaleExponent;
        properties::FloatProperty scaleFactor;

        properties::BoolProperty pixelSizeControl;
        properties::FloatProperty maxPixelSize;
    };
    SizeSettings _sizeSettings;

    struct ColorSettings : properties::PropertyOwner {
        explicit ColorSettings(const ghoul::Dictionary& dictionary);
        properties::Vec3Property pointColor;
        std::unique_ptr<ColorMappingComponent> colorMapping;
    };
    ColorSettings _colorSettings;

    struct Fading : properties::PropertyOwner {
        explicit Fading(const ghoul::Dictionary& dictionary);
        properties::Vec2Property fadeInDistances;
        properties::BoolProperty enabled;
        properties::BoolProperty invert;
    };
    Fading _fading;

    properties::BoolProperty _useSpriteTexture;
    properties::StringProperty _spriteTexturePath;

    properties::BoolProperty _useAdditiveBlending;

    properties::BoolProperty _drawElements;
    properties::OptionProperty _renderOption;

    properties::UIntProperty _nDataPoints;

    ghoul::opengl::Texture* _spriteTexture = nullptr;
    ghoul::opengl::ProgramObject* _program = nullptr;

    UniformCache(
        cameraViewProjectionMatrix, modelMatrix, cameraPos, cameraLookup, renderOption,
        maxBillboardSize, color, opacity, scaleExponent, scaleFactor, up, right,
        fadeInValue, screenSize, hasSpriteTexture, spriteTexture, useColormap,
        colorMapTexture, cmapRangeMin, cmapRangeMax, nanColor, useNanColor,
        hideOutsideRange, enablePixelSizeControl, aboveRangeColor, useAboveRangeColor,
        belowRangeColor, useBelowRangeColor, hasDvarScaling
    ) _uniformCache;

    std::string _dataFile;

    DistanceUnit _unit = DistanceUnit::Parsec;

    dataloader::Dataset _dataset;
    dataloader::DataMapping _dataMapping;

    std::unique_ptr<LabelsComponent> _labels;

    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);

    GLuint _vao = 0;
    GLuint _vbo = 0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEPOINTCLOUD___H__
