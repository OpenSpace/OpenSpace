/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/exoplanetsexperttool/rendering/renderabledensitypointcloud.h>

#include <modules/exoplanetsexperttool/util/densityvolume.h>
#include <openspace/data/colormaploader.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/logging/logmanager.h>
#include <openspace/opengl/openglstatecache.h>
#include <openspace/opengl/programobject.h>
#include <openspace/opengl/texture.h>
#include <openspace/opengl/textureunit.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/updatestructures.h>
#include <array>
#include <cstddef>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableDensityPointCloud";

    constexpr Property::PropertyInfo PointSizeInfo = {
        "PointSize",
        "Point size",
        "The size of each point, in pixels."
    };

    constexpr Property::PropertyInfo FixedColorInfo = {
        "FixedColor",
        "Fixed color",
        "The color of the points when no color map is used."
    };

    constexpr Property::PropertyInfo UseColorMapInfo = {
        "UseColorMap",
        "Use color map",
        "If true, and a color map file was provided, the points are colored by their "
        "density using the color map. Otherwise the fixed color is used."
    };

    constexpr Property::PropertyInfo ColorMapFileInfo = {
        "ColorMap",
        "Color map",
        "The file to use for the density-based color mapping."
    };

    constexpr Property::PropertyInfo DensityRangeInfo = {
        "DensityRange",
        "Density range",
        "The density values mapped to the start and end of the color map. Defaults to "
        "the minimum and maximum density of the computed volume."
    };

    constexpr Property::PropertyInfo UseLogScaleInfo = {
        "UseLogScale",
        "Use log scale",
        "If true, the density is mapped logarithmically onto the color map."
    };

    constexpr Property::PropertyInfo DensityThresholdInfo = {
        "DensityThreshold",
        "Density threshold",
        "Points with a density below this percentage of the maximum density are not "
        "rendered."
    };

    constexpr Property::PropertyInfo SelectedIndicesInfo = {
        "SelectedIndices",
        "Selected indices",
        "The indices of the selected points, in the order they appear in the dataset "
        "after rows with invalid positions have been removed.",
        Property::Visibility::Hidden
    };

    constexpr Property::PropertyInfo ClearSelectionInfo = {
        "ClearSelection",
        "Clear selection",
        "Clears the list of currently selected indices."
    };

    constexpr Property::PropertyInfo SelectionColorInfo = {
        "SelectionColor",
        "Selection color",
        "The color of the selected points."
    };

    constexpr Property::PropertyInfo UnselectedOpacityInfo = {
        "UnselectedOpacity",
        "Unselected opacity",
        "The opacity factor applied to points that are not selected, when there is a "
        "selection."
    };

    constexpr Property::PropertyInfo ShowBoundingBoxInfo = {
        "Ebabled",
        "Show bounding box",
        "If true, the bounding box of the density grid is rendered."
    };

    constexpr Property::PropertyInfo BoundingBoxColorInfo = {
        "Color",
        "Bounding box color",
        "The color of the bounding box of the density grid."
    };

    struct PointVertex {
        glm::vec3 position;
        float density;
    };

    struct [[codegen::Dictionary(RenderableDensityPointCloud)]] Parameters {
        // The CSV or SPECK file containing the particle positions. CSV files must have
        // a header row; SPECK files use the first three values as position coordinates.
        std::filesystem::path file;

        // The CSV column containing the x coordinate. Defaults to "x". Ignored for SPECK.
        std::optional<std::string> xColumn;

        // The CSV column containing the y coordinate. Defaults to "y". Ignored for SPECK.
        std::optional<std::string> yColumn;

        // The CSV column containing the z coordinate. Defaults to "z". Ignored for SPECK.
        std::optional<std::string> zColumn;

        enum class [[codegen::map(openspace::DistanceUnit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyear [[codegen::key("Gly")]]
        };
        // The unit of the positions in the file. Defaults to parsec.
        std::optional<Unit> unit;

        // The number of nodes of the density grid in each direction. Defaults to 128
        // in each direction.
        std::optional<glm::ivec3> gridDimensions [[codegen::greaterequal(glm::ivec3(2))]];

        // [[codegen::verbatim(PointSizeInfo.description)]]
        std::optional<float> pointSize [[codegen::greater(0.f)]];

        struct Color {
            // [[codegen::verbatim(FixedColorInfo.description)]]
            std::optional<glm::vec3> fixedColor [[codegen::color()]];

            // An optional color map file (.cmap or 1D image) used to color the points by
            // density.
            std::optional<std::filesystem::path> colorMap;

            // [[codegen::verbatim(DensityRangeInfo.description)]]
            std::optional<glm::vec2> densityRange;

            // [[codegen::verbatim(UseLogScaleInfo.description)]]
            std::optional<bool> useLogScale;

            // [[codegen::verbatim(SelectionColorInfo.description)]]
            std::optional<glm::vec3> selectionColor [[codegen::color()]];

            // [[codegen::verbatim(UnselectedOpacityInfo.description)]]
            std::optional<float> unselectedOpacity [[codegen::inrange(0.f, 1.f)]];
        };

        // Settings related to color of the points
        std::optional<Color> color;

        // [[codegen::verbatim(DensityThresholdInfo.description)]]
        std::optional<float> densityThreshold [[codegen::inrange(1.f, 100.f)]];

        struct BoundingBox {
            // [[codegen::verbatim(ShowBoundingBoxInfo.description)]]
            std::optional<bool> enabled;

            // [[codegen::verbatim(BoundingBoxColorInfo.description)]]
            std::optional<glm::vec4> color [[codegen::color()]];
        };

        // Settings for showing the bounding box of the volume.
        std::optional<BoundingBox> boundingBox;

    };
} // namespace
#include "renderabledensitypointcloud_codegen.cpp"

namespace openspace::exoplanets {

openspace::Documentation RenderableDensityPointCloud::Documentation() {
    return codegen::doc<Parameters>(
        "exoplanetsexperttool_renderable_densitypointcloud",
        Renderable::Documentation()
    );
}

RenderableDensityPointCloud::RenderableDensityPointCloud(const Dictionary& dictionary)
    : Renderable(dictionary)
    , _pointSize(PointSizeInfo, 3.f, 0.5f, 64.f)
    , _color{
        .owner = PropertyOwner({"Color", "Color"}),
        .fixedColor = Vec3Property(FixedColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f)),
        .useColorMap = BoolProperty(UseColorMapInfo, true),
        .colorMapFile = StringProperty(ColorMapFileInfo),
        .densityRange = Vec2Property(
            DensityRangeInfo,
            glm::vec2(0.f, 1.f),
            glm::vec2(0.f),
            glm::vec2(1.f)
        ),
        .useLogScale = BoolProperty(UseLogScaleInfo, false),
        .selectionColor = Vec3Property(
            SelectionColorInfo,
            glm::vec3(1.f, 0.6f, 0.1f),
            glm::vec3(0.f),
            glm::vec3(1.f)
        ),
        .unselectedOpacity = FloatProperty(UnselectedOpacityInfo, 0.15f, 0.f, 1.f)
    }
    , _densityThreshold(DensityThresholdInfo, 0.f, 0.f, 100.f)
    , _selectedIndices(SelectedIndicesInfo)
    , _clearSelection(ClearSelectionInfo)
    , _boundingBox {
        .owner = PropertyOwner({"BoudningBox", "Bounding Box"}),
        .enabled = BoolProperty(ShowBoundingBoxInfo, true),
        .color = Vec4Property(
            SelectionColorInfo,
            glm::vec4(1.f, 1.f, 1.f, 0.5f),
            glm::vec4(0.f),
            glm::vec4(1.f)
        )
    }
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _file = p.file;
    _xColumn = p.xColumn.value_or("x");
    _yColumn = p.yColumn.value_or("y");
    _zColumn = p.zColumn.value_or("z");
    _unitToMeter = toMeter(
        p.unit.has_value() ? codegen::map<DistanceUnit>(*p.unit) : DistanceUnit::Parsec
    );
    _gridDimensions = glm::uvec3(p.gridDimensions.value_or(glm::ivec3(128)));

    _pointSize = p.pointSize.value_or(_pointSize);
    addProperty(_pointSize);

    if (p.color.has_value()) {
        const Parameters::Color color = *p.color;

        if (color.colorMap.has_value())  {
            const bool fileExists = std::filesystem::exists(color.colorMap.value());
            if (!fileExists) {
                LERROR(std::format(
                    "Could not find cmap file: {}", color.colorMap.value()
                ));
            }
            _color.colorMapFile = (*color.colorMap).string();
            _color.useColorMap = true;
        }

        if (color.densityRange.has_value()) {
            _color.densityRange = *color.densityRange;
            _hasUserDensityRange = true;
        }

        _color.useLogScale = color.useLogScale.value_or(_color.useLogScale);

        _color.fixedColor = color.fixedColor.value_or(_color.fixedColor);

        _color.selectionColor = color.selectionColor.value_or(_color.selectionColor);
        _color.unselectedOpacity =
            color.unselectedOpacity.value_or(_color.unselectedOpacity);
    }

    _color.fixedColor.setViewOption(Property::ViewOptions::Color);
    _color.owner.addProperty(_color.fixedColor);

    _color.owner.addProperty(_color.useColorMap);
    _color.colorMapFile.onChange([this]() {
        const bool fileExists = std::filesystem::exists(_color.colorMapFile.value());
        if (!fileExists) {
            LERROR(std::format(
                "Could not find cmap file: {}", _color.colorMapFile.value()
            ));
            return;
        }
        _colorMapIsDirty = true;
    });
    _color.owner.addProperty(_color.colorMapFile);
    _color.owner.addProperty(_color.densityRange);
    _color.owner.addProperty(_color.useLogScale);

    _color.selectionColor.setViewOption(Property::ViewOptions::Color);
    _color.owner.addProperty(_color.selectionColor);

    _color.owner.addProperty(_color.unselectedOpacity);
    addPropertySubOwner(_color.owner);

    _densityThreshold = p.densityThreshold.value_or(_densityThreshold);
    addProperty(_densityThreshold);

    _selectedIndices.onChange([this]() { _selectionIsDirty = true; });
    addProperty(_selectedIndices);

    _clearSelection.onChange([this]() { _selectedIndices.value().clear();  });
    addProperty(_clearSelection);

    if (p.boundingBox.has_value()) {
        const Parameters::BoundingBox box = *p.boundingBox;
        _boundingBox.enabled = box.enabled.value_or(_boundingBox.enabled);
        _boundingBox.color = box.color.value_or(_boundingBox.color);
    }

    _boundingBox.owner.addProperty(_boundingBox.enabled);

    _boundingBox.color.setViewOption(Property::ViewOptions::Color);
    _boundingBox.owner.addProperty(_boundingBox.color);
    addPropertySubOwner(_boundingBox.owner);

    setRenderBin(RenderBin::PreDeferredTransparent);
}

RenderableDensityPointCloud::~RenderableDensityPointCloud() = default;

void RenderableDensityPointCloud::initialize() {
    _volume = std::make_unique<DensityVolume>(DensityVolume::fromFile(
        absPath(_file),
        _xColumn,
        _yColumn,
        _zColumn,
        _gridDimensions
    ));

    if (!_volume) {
        LERROR("Could not create volume from dataset");
        return;
    }

    const float maxDensity = _volume->maxDensity();
    _color.densityRange.setMaxValue(glm::vec2(maxDensity));
    if (!_hasUserDensityRange) {
        _color.densityRange = glm::vec2(_volume->minDensity(), maxDensity);
    }

    const glm::vec3 farthest = glm::max(
        glm::abs(_volume->boundsMin()),
        glm::abs(_volume->boundsMax())
    );
    setBoundingSphere(glm::length(glm::dvec3(farthest)) * _unitToMeter);
}

void RenderableDensityPointCloud::initializeCmap() {
    _colorMapTexture = dataloader::colormap::loadColorMapTexture(
        absPath(_color.colorMapFile),
        { .wrapping = opengl::Texture::WrappingMode::ClampToEdge }
    );
    if (!_colorMapTexture) {
        LWARNING(std::format(
            "Could not load color map '{}', using fixed color",
            _color.colorMapFile.value()
        ));
    }
}

void RenderableDensityPointCloud::initializeGL() {
    _pointsProgram = global::renderEngine->buildRenderProgram(
        "DensityPointCloud",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/densitypoints_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/densitypoints_fs.glsl")
    );
    opengl::updateUniformLocations(*_pointsProgram, _pointsUniformCache);

    _boxProgram = global::renderEngine->buildRenderProgram(
        "DensityPointCloudBox",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/densitybox_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/densitybox_fs.glsl")
    );
    opengl::updateUniformLocations(*_boxProgram, _boxUniformCache);

    if (!_color.colorMapFile.value().empty()) {
        initializeCmap();
    }

    glGenVertexArrays(1, &_pointsVao);
    glGenBuffers(1, &_pointsVbo);
    glGenBuffers(1, &_selectionVbo);
    glBindVertexArray(_pointsVao);

    glBindBuffer(GL_ARRAY_BUFFER, _pointsVbo);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0, 3, GL_FLOAT, GL_FALSE, sizeof(PointVertex),
        reinterpret_cast<void*>(offsetof(PointVertex, position))
    );
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1, 1, GL_FLOAT, GL_FALSE, sizeof(PointVertex),
        reinterpret_cast<void*>(offsetof(PointVertex, density))
    );

    glBindBuffer(GL_ARRAY_BUFFER, _selectionVbo);
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, sizeof(float), nullptr);

    glGenVertexArrays(1, &_boxVao);
    glGenBuffers(1, &_boxVbo);
    glBindVertexArray(_boxVao);
    glBindBuffer(GL_ARRAY_BUFFER, _boxVbo);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(glm::vec3), nullptr);

    glBindVertexArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    uploadPointData();
    uploadBoundingBox();
    _selectionIsDirty = true;
}

void RenderableDensityPointCloud::deinitializeGL() {
    glDeleteVertexArrays(1, &_pointsVao);
    glDeleteBuffers(1, &_pointsVbo);
    glDeleteBuffers(1, &_selectionVbo);
    glDeleteVertexArrays(1, &_boxVao);
    glDeleteBuffers(1, &_boxVbo);
    _pointsVao = 0;
    _pointsVbo = 0;
    _selectionVbo = 0;
    _boxVao = 0;
    _boxVbo = 0;

    _colorMapTexture = nullptr;

    if (_pointsProgram) {
        global::renderEngine->removeRenderProgram(_pointsProgram.get());
        _pointsProgram = nullptr;
    }
    if (_boxProgram) {
        global::renderEngine->removeRenderProgram(_boxProgram.get());
        _boxProgram = nullptr;
    }
}

const DensityVolume* RenderableDensityPointCloud::densityVolume() const {
    return _volume.get();
}

std::optional<glm::dmat4> RenderableDensityPointCloud::modelToClipTransform() const {
    return _modelToClip;
}

double RenderableDensityPointCloud::unitToMeter() const {
    return _unitToMeter;
}

void RenderableDensityPointCloud::uploadPointData() {
    if (!_volume) {
        return;
    }
    const std::vector<glm::vec3>& positions = _volume->particlePositions();
    const std::vector<float>& densities = _volume->particleDensities();

    std::vector<PointVertex> vertices;
    vertices.reserve(positions.size());
    for (size_t i = 0; i < positions.size(); i++) {
        vertices.push_back({ positions[i], densities[i] });
    }

    glBindBuffer(GL_ARRAY_BUFFER, _pointsVbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        vertices.size() * sizeof(PointVertex),
        vertices.data(),
        GL_STATIC_DRAW
    );
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void RenderableDensityPointCloud::uploadSelection() {
    const size_t nPoints = _volume->particlePositions().size();
    std::vector<float> selected(nPoints, 0.f);
    for (int index : _selectedIndices.value()) {
        if (index >= 0 && static_cast<size_t>(index) < nPoints) {
            selected[index] = 1.f;
        }
    }

    glBindBuffer(GL_ARRAY_BUFFER, _selectionVbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        selected.size() * sizeof(float),
        selected.data(),
        GL_DYNAMIC_DRAW
    );
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    _selectionIsDirty = false;
}

void RenderableDensityPointCloud::uploadBoundingBox() {
    if (!_volume) {
        return;
    }
    const glm::vec3 lo = _volume->boundsMin();
    const glm::vec3 hi = _volume->boundsMax();
    auto corner = [&lo, &hi](int i) {
        return glm::vec3(
            (i & 1) ? hi.x : lo.x,
            (i & 2) ? hi.y : lo.y,
            (i & 4) ? hi.z : lo.z
        );
    };

    // Each edge connects two corners whose indices differ in exactly one bit
    std::array<glm::vec3, 24> lines;
    size_t v = 0;
    for (int i = 0; i < 8; i++) {
        for (int bit = 1; bit < 8; bit <<= 1) {
            if (!(i & bit)) {
                lines[v++] = corner(i);
                lines[v++] = corner(i | bit);
            }
        }
    }

    glBindBuffer(GL_ARRAY_BUFFER, _boxVbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(lines), lines.data(), GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void RenderableDensityPointCloud::update(const UpdateData&) {
    if (_colorMapIsDirty) {
        initializeCmap();
    }

    if (_pointsProgram && _pointsProgram->isDirty()) {
        _pointsProgram->rebuildFromFile();
        opengl::updateUniformLocations(*_pointsProgram, _pointsUniformCache);
    }
    if (_boxProgram && _boxProgram->isDirty()) {
        _boxProgram->rebuildFromFile();
        opengl::updateUniformLocations(*_boxProgram, _boxUniformCache);
    }
    if (_selectionIsDirty && _volume && _selectionVbo != 0) {
        uploadSelection();
    }
}

void RenderableDensityPointCloud::render(const RenderData& data, RendererTasks&) {
    if (!_volume) {
        return;
    }

    const glm::dmat4 modelView = calcModelViewTransform(data);
    const glm::mat4 projection = data.camera.projectionMatrix();
    _modelToClip = glm::dmat4(projection) * modelView *
        glm::scale(glm::dmat4(1.0), glm::dvec3(_unitToMeter));

    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_FALSE);
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

    _pointsProgram->activate();
    _pointsProgram->setUniform(_pointsUniformCache.modelViewTransform, modelView);
    _pointsProgram->setUniform(_pointsUniformCache.projectionTransform, projection);
    _pointsProgram->setUniform(_pointsUniformCache.unitToMeter, _unitToMeter);
    _pointsProgram->setUniform(_pointsUniformCache.pointSize, _pointSize);
    _pointsProgram->setUniform(_pointsUniformCache.fixedColor, _color.fixedColor);
    _pointsProgram->setUniform(_pointsUniformCache.densityRange, _color.densityRange);
    _pointsProgram->setUniform(_pointsUniformCache.useLogScale, _color.useLogScale);
    _pointsProgram->setUniform(
        _pointsUniformCache.densityThreshold,
        _densityThreshold / 100.f * _volume->maxDensity()
    );
    _pointsProgram->setUniform(
        _pointsUniformCache.hasSelection,
        !_selectedIndices.value().empty()
    );
    _pointsProgram->setUniform(_pointsUniformCache.selectionColor, _color.selectionColor);
    _pointsProgram->setUniform(
        _pointsUniformCache.unselectedOpacity,
        _color.unselectedOpacity
    );
    _pointsProgram->setUniform(_pointsUniformCache.opacity, opacity());

    const bool useColorMap = _color.useColorMap && _colorMapTexture;
    _pointsProgram->setUniform(_pointsUniformCache.useColorMap, useColorMap);
    opengl::TextureUnit colorMapUnit;
    if (useColorMap) {
        colorMapUnit.bind(*_colorMapTexture);
        _pointsProgram->setUniform(_pointsUniformCache.colorMapTexture, colorMapUnit);
    }

    glEnable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(_pointsVao);
    glDrawArrays(
        GL_POINTS,
        0,
        static_cast<GLsizei>(_volume->particlePositions().size())
    );
    glDisable(GL_PROGRAM_POINT_SIZE);
    _pointsProgram->deactivate();

    if (_boundingBox.enabled) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        _boxProgram->activate();
        _boxProgram->setUniform(_boxUniformCache.modelViewTransform, modelView);
        _boxProgram->setUniform(_boxUniformCache.projectionTransform, projection);
        _boxProgram->setUniform(_boxUniformCache.unitToMeter, _unitToMeter);
        glm::vec4 color = _boundingBox.color;
        color.a *= opacity();
        _boxProgram->setUniform(_boxUniformCache.color, color);
        glLineWidth(1.f);
        glBindVertexArray(_boxVao);
        glDrawArrays(GL_LINES, 0, 24);
        _boxProgram->deactivate();
    }

    glBindVertexArray(0);
    opengl::OpenGLStateCache& state = global::renderEngine->openglStateCache();
    state.resetBlendState();
    state.resetDepthState();
    state.resetLineState();
}

} // namespace openspace::exoplanets
