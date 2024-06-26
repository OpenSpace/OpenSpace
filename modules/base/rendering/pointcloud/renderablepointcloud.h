/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/base/rendering/pointcloud/sizemappingcomponent.h>
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
#include <filesystem>
#include <functional>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

struct TextureFormat {
    glm::uvec2 resolution;
    bool useAlpha = false;

    friend bool operator==(const TextureFormat& l, const TextureFormat& r);
};
struct TextureFormatHash {
    size_t operator()(const TextureFormat& k) const;
};

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
    enum class TextureInputMode {
        Single = 0,
        Multi,
        Other // For subclasses that need to handle their own texture
    };

    virtual void initializeShadersAndGlExtras();
    virtual void deinitializeShaders();
    virtual void setExtraUniforms();
    virtual void preUpdate();

    glm::dvec3 transformedPosition(const dataloader::Dataset::Entry& e) const;
    glm::quat orientationQuaternion(const dataloader::Dataset::Entry& e) const;

    virtual int nAttributesPerPoint() const;

    /**
     * Helper function to buffer the vertex attribute with the given name and number
     * of values. Assumes that the value is a float value.
     *
     * Returns the updated offset after this attribute is added
     */
    int bufferVertexAttribute(const std::string& name, GLint nValues,
        int nAttributesPerPoint, int offset) const;

    virtual void updateBufferData();
    void updateSpriteTexture();

    /// Find the index of the currently chosen color parameter in the dataset
    int currentColorParameterIndex() const;
    /// Find the index of the currently chosen size parameter in the dataset
    int currentSizeParameterIndex() const;

    bool hasColorData() const;
    bool hasSizeData() const;
    bool hasMultiTextureData() const;
    bool useOrientationData() const;

    virtual void addPositionDataForPoint(unsigned int index, std::vector<float>& result,
        double& maxRadius) const;
    virtual void addColorAndSizeDataForPoint(unsigned int index,
        std::vector<float>& result) const;
    virtual void addOrientationDataForPoint(unsigned int index,
        std::vector<float>& result) const;

    std::vector<float> createDataSlice();

    /**
     * A function that subclasses could override to initialize their own textures to
     * use for rendering, when the `_textureMode` is set to Other
     */
    virtual void initializeCustomTexture();
    void initializeSingleTexture();
    void initializeMultiTextures();
    void clearTextureDataStructures();

    void loadTexture(const std::filesystem::path& path, int index);

    void initAndAllocateTextureArray(unsigned int textureId,
        glm::uvec2 resolution, size_t nLayers, bool useAlpha);

    void fillAndUploadTextureLayer(unsigned int arrayindex, unsigned int layer,
        size_t textureIndex, glm::uvec2 resolution, bool useAlpha, const void* pixelData);

    void generateArrayTextures();

    float computeDistanceFadeValue(const RenderData& data) const;

    void renderPoints(const RenderData& data, const glm::dmat4& modelMatrix,
        const glm::dvec3& orthoRight, const glm::dvec3& orthoUp, float fadeInVariable);

    gl::GLenum internalGlFormat(bool useAlpha) const;
    ghoul::opengl::Texture::Format glFormat(bool useAlpha) const;

    bool _dataIsDirty = true;
    bool _spriteTextureIsDirty = false;
    bool _cmapIsDirty = true;

    bool _hasSpriteTexture = false;
    bool _hasDataFile = false;
    bool _hasColorMapFile = false;
    bool _hasDatavarSize = false;
    bool _hasLabels = false;

    struct SizeSettings : properties::PropertyOwner {
        explicit SizeSettings(const ghoul::Dictionary& dictionary);

        std::unique_ptr<SizeMappingComponent> sizeMapping;

        properties::FloatProperty scaleExponent;
        properties::FloatProperty scaleFactor;

        properties::BoolProperty useMaxSizeControl;
        properties::FloatProperty maxAngularSize;
    };
    SizeSettings _sizeSettings;

    struct ColorSettings : properties::PropertyOwner {
        explicit ColorSettings(const ghoul::Dictionary& dictionary);
        properties::Vec3Property pointColor;
        std::unique_ptr<ColorMappingComponent> colorMapping;
        properties::BoolProperty enableOutline;
        properties::Vec3Property outlineColor;
        properties::FloatProperty outlineWidth;
        properties::OptionProperty outlineStyle;
        properties::BoolProperty applyCmapToOutline;
    };
    ColorSettings _colorSettings;

    struct Fading : properties::PropertyOwner {
        explicit Fading(const ghoul::Dictionary& dictionary);
        properties::Vec2Property fadeInDistances;
        properties::BoolProperty enabled;
        properties::BoolProperty invert;
    };
    Fading _fading;

    properties::BoolProperty _useAdditiveBlending;
    properties::BoolProperty _useRotation;

    properties::BoolProperty _drawElements;
    properties::OptionProperty _renderOption;

    properties::UIntProperty _nDataPoints;
    properties::BoolProperty _hasOrientationData;

    struct Texture : properties::PropertyOwner {
        Texture();
        properties::BoolProperty enabled;
        properties::BoolProperty allowCompression;
        properties::BoolProperty useAlphaChannel;
        properties::StringProperty spriteTexturePath;
        properties::StringProperty inputMode;
    };
    Texture _texture;
    TextureInputMode _textureMode = TextureInputMode::Single;
    std::filesystem::path _texturesDirectory;

    ghoul::opengl::ProgramObject* _program = nullptr;

    UniformCache(
        cameraViewMatrix, projectionMatrix, modelMatrix, cameraPosition, cameraLookUp,
        renderOption, maxAngularSize, color, opacity, scaleExponent, scaleFactor, up,
        right, fadeInValue, hasSpriteTexture, spriteTexture, useColorMap, colorMapTexture,
        cmapRangeMin, cmapRangeMax, nanColor, useNanColor, hideOutsideRange,
        enableMaxSizeControl, aboveRangeColor, useAboveRangeColor, belowRangeColor,
        useBelowRangeColor, hasDvarScaling, dvarScaleFactor, enableOutline, outlineColor,
        outlineWeight, outlineStyle, useCmapOutline, aspectRatioScale, useOrientationData
    ) _uniformCache;

    std::filesystem::path _dataFile;

    DistanceUnit _unit = DistanceUnit::Parsec;

    bool _useCaching = true;
    bool _shouldComputeScaleExponent = false;
    bool _createLabelsFromDataset = false;
    bool _skipFirstDataPoint = false;

    dataloader::Dataset _dataset;
    dataloader::DataMapping _dataMapping;

    std::unique_ptr<LabelsComponent> _labels;

    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);

    GLuint _vao = 0;
    GLuint _vbo = 0;

    // List of (unique) loaded textures. The other maps refer to the index in this vector
    std::vector<std::unique_ptr<ghoul::opengl::Texture>> _textures;
    std::unordered_map<std::string, size_t> _textureNameToIndex;

    // Texture index in dataset to index in vector of textures
    std::unordered_map<int, size_t> _indexInDataToTextureIndex;

    // Resolution/format to index in textures vector (used to generate one texture
    // array per unique format)
    std::unordered_map<TextureFormat, std::vector<size_t>, TextureFormatHash>
        _textureMapByFormat;

    // One per resolution above
    struct TextureArrayInfo {
        GLuint renderId;
        GLint startOffset = -1;
        int nPoints = -1;
        glm::vec2 aspectRatioScale = glm::vec2(1.f);
    };
    std::vector<TextureArrayInfo> _textureArrays;

    struct TextureId {
        unsigned int arrayId;
        unsigned int layer;
    };
    std::unordered_map<size_t, TextureId> _textureIndexToArrayMap;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEPOINTCLOUD___H__
