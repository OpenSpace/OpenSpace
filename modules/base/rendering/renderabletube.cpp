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

#include <modules/base/rendering/renderabletube.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/lightsource.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/texture.h>
#include <glm/gtx/projection.hpp>
#include <optional>

using json = nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "RenderableTube";
    constexpr int8_t CurrentMajorVersion = 0;
    constexpr int8_t CurrentMinorVersion = 1;

    constexpr int NearestInterpolation = 0;
    constexpr int LinearInterpolation = 1;

    std::map<std::string, int> InterpolationMapping = {
        { "Nearest Neighbor", NearestInterpolation },
        { "Linear", LinearInterpolation },
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunctionPath",
        "Transfer Function Path",
        "Specifies the transfer function file path",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFaceCullingInfo = {
        "EnableFaceCulling",
        "Enable Face Culling",
        "Enable OpenGL automatic face culling optimization",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadingEnabledInfo = {
        "PerformShading",
        "Perform Shading",
        "This value determines whether shading should be applied to the tube",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo AmbientIntensityInfo = {
        "AmbientIntensity",
        "Ambient Intensity",
        "A multiplier for ambient lighting for the shading of the tube",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DiffuseIntensityInfo = {
        "DiffuseIntensity",
        "Diffuse Intensity",
        "A multiplier for diffuse lighting for the shading of the tube",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SpecularIntensityInfo = {
        "SpecularIntensity",
        "Specular Intensity",
        "A multiplier for specular lighting for the shading of the tube",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourcesInfo = {
        "LightSources",
        "Light Sources",
        "A list of light sources that this tube should accept light from",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TubeColorInfo = {
        "FixedColor",
        "Fixed Color",
        "This value is used to define the color of the tube when no color map is"
        "used",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo AddEdgesInfo = {
        "AddEdges",
        "Add Edges",
        "This value determines whether a bottom and top should b eadded to the tube",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextureDirectoryInfo = {
        "TextureDirectory",
        "Texture Directory",
        "The directory where the cut-plane textures are located",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawWireframeInfo = {
        "DrawWireframe",
        "Wireframe",
        "If true, draw the wire frame of the tube",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo WireLineWidthInfo = {
        "WireLineWidth",
        "Wire Line Width",
        "The line width to use when the tube is rendered as a wireframe",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseSmoothNormalsInfo = {
        "UseSmoothNormals",
        "Use Smooth Normals",
        "If ture, the tube is shaded using smooth normals. If false, every triangle "
        "get its own normal, which can lead to harder shadows",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowAllTubeInfo = {
        "ShowAllTube",
        "Show all the tube",
        "If ture, only the part of the tube that corresponds to the current time is "
        "shown. If false, the whole tube is shown.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo JumpToPrevPolygonInfo = {
        "JumpToPrevPolygon",
        "Jump To Previous Polygon",
        "Jumps to the exact time of the previous polygon relative the current time",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo JumpToNextPolygonInfo = {
        "JumpToNextPolygon",
        "Jump To Next Polygon",
        "Jumps to the exact time of the next polygon relative the current time",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo InterpolationMethodInfo = {
        "InterpolationMethod",
        "Interpolation Method",
        "Which interpolaiton method to use for the cutplane texture",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableTube)]] Parameters {
        // The input file with data for the tube
        std::string file;

        // [[codegen::verbatim(EnableFaceCullingInfo.description)]]
        std::optional<bool> enableFaceCulling;

        // [[codegen::verbatim(ShadingEnabledInfo.description)]]
        std::optional<bool> performShading;

        // [[codegen::verbatim(AmbientIntensityInfo.description)]]
        std::optional<float> ambientIntensity [[codegen::inrange(0.f, 1.f)]];

        // [[codegen::verbatim(DiffuseIntensityInfo.description)]]
        std::optional<float> diffuseIntensity [[codegen::inrange(0.f, 1.f)]];

        // [[codegen::verbatim(SpecularIntensityInfo.description)]]
        std::optional<float> specularIntensity [[codegen::inrange(0.f, 1.f)]];

        // [[codegen::verbatim(LightSourcesInfo.description)]]
        std::optional<std::vector<ghoul::Dictionary>> lightSources
            [[codegen::reference("core_light_source")]];

        struct ColorSettings {
            // [[codegen::verbatim(TubeColorInfo.description)]]
            std::optional<glm::vec3> fixedColor [[codegen::color()]];

            // Settings related to the choice of color map, parameters, etc.
            std::optional<ghoul::Dictionary> colorMapping
                [[codegen::reference("colormappingcomponent")]];
        };
        // Settings related to the coloring of the points, such as a fixed color,
        // color map, etc.
        std::optional<ColorSettings> coloring;

        // [[codegen::verbatim(AddEdgesInfo.description)]]
        std::optional<bool> addEdges;

        struct ColorSettingsCutplane {
            // [[codegen::verbatim(TubeColorInfo.description)]]
            std::optional<glm::vec3> fixedColor [[codegen::color()]];

            // Settings related to the choice of color map, parameters, etc.
            std::optional<ghoul::Dictionary> colorMapping
                [[codegen::reference("colormappingcomponent")]];
        };
        // Settings related to the coloring of the points, such as a fixed color,
        // color map, etc.
        std::optional<ColorSettingsCutplane> coloringCutplane;

        // [[codegen::verbatim(TextureDirectoryInfo.description)]]
        std::optional<std::string> textureDirectory;

        // [[codegen::verbatim(InterpolationMethodInfo.description)]]
        std::optional<std::string> interpolationMethod;

        // [[codegen::verbatim(DrawWireframeInfo.description)]]
        std::optional<bool> drawWireframe;

        // [[codegen::verbatim(WireLineWidthInfo.description)]]
        std::optional<float> wireLineWidth;

        // [[codegen::verbatim(UseSmoothNormalsInfo.description)]]
        std::optional<bool> useSmoothNormals;

        // [[codegen::verbatim(ShowAllTubeInfo.description)]]
        std::optional<bool> showAllTube;
    };
#include "renderabletube_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTube::Documentation() {
    return codegen::doc<Parameters>("base_renderable_tube");
}

RenderableTube::Shading::Shading()
    : properties::PropertyOwner({ "Shading" })
    , enabled(ShadingEnabledInfo, true)
    , ambientIntensity(AmbientIntensityInfo, 0.2f, 0.f, 1.f)
    , diffuseIntensity(DiffuseIntensityInfo, 1.f, 0.f, 1.f)
    , specularIntensity(SpecularIntensityInfo, 1.f, 0.f, 1.f)
{
    addProperty(enabled);
    addProperty(ambientIntensity);
    addProperty(diffuseIntensity);
    addProperty(specularIntensity);
}

RenderableTube::ColorSettings::ColorSettings(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Coloring", "Coloring", "" })
    , tubeColor(TubeColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.coloring.has_value()) {
        const Parameters::ColorSettings settings = *p.coloring;
        tubeColor = settings.fixedColor.value_or(tubeColor);

        if (settings.colorMapping.has_value()) {
            colorMapping = std::make_unique<ColorMappingComponent>(
                *settings.colorMapping
            );
            addPropertySubOwner(colorMapping.get());
        }
    }
    tubeColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(tubeColor);
}

RenderableTube::ColorSettingsCutplane::ColorSettingsCutplane(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "ColoringCutplane", "Coloring Cutplane", "" })
    , fixedColor(TubeColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.coloringCutplane.has_value()) {
        const Parameters::ColorSettingsCutplane settings = *p.coloringCutplane;
        fixedColor = settings.fixedColor.value_or(fixedColor);

        if (settings.colorMapping.has_value()) {
            colorMapping = std::make_unique<ColorMappingComponent>(
                *settings.colorMapping
            );
            addPropertySubOwner(colorMapping.get());
        }
    }
    fixedColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(fixedColor);
}

RenderableTube::RenderableTube(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _enableFaceCulling(EnableFaceCullingInfo, true)
    , _lightSourcePropertyOwner({ "LightSources", "Light Sources" })
    , _colorSettings(dictionary)
    , _addEdges(AddEdgesInfo, true)
    , _interpolationMethod(
        InterpolationMethodInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _drawWireframe(DrawWireframeInfo, false)
    , _wireLineWidth(WireLineWidthInfo, 1.f, 1.f, 10.f)
    , _useSmoothNormals(UseSmoothNormalsInfo, true)
    , _showAllTube(ShowAllTubeInfo, false)
    , _jumpToPrevPolygon(JumpToPrevPolygonInfo)
    , _jumpToNextPolygon(JumpToNextPolygonInfo)
    , _colorSettingsCutplane(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _dataFile = p.file;

    addProperty(Fadeable::_opacity);

    if (p.coloring.has_value() && (*p.coloring).colorMapping.has_value()) {
        _hasColorMapFile = true;

        _colorSettings.colorMapping->dataColumn.onChange(
            [this]() { _tubeIsDirty = true; }
        );

        _colorSettings.colorMapping->setRangeFromData.onChange([this]() {
            _colorSettings.colorMapping->valueRange = _colorDataset.findValueRange(
                currentColorParameterIndex()
            );
        });

        _colorSettings.colorMapping->colorMapFile.onChange([this]() {
            _tubeIsDirty = true;
            _hasColorMapFile = std::filesystem::exists(
                _colorSettings.colorMapping->colorMapFile.value()
            );
        });
    }
    addPropertySubOwner(_colorSettings);

    _shading.enabled = p.performShading.value_or(_shading.enabled);
    _shading.ambientIntensity = p.ambientIntensity.value_or(_shading.ambientIntensity);
    _shading.diffuseIntensity = p.diffuseIntensity.value_or(_shading.diffuseIntensity);
    _shading.specularIntensity = p.specularIntensity.value_or(_shading.specularIntensity);
    addPropertySubOwner(_shading);

    if (p.lightSources.has_value()) {
        std::vector<ghoul::Dictionary> lightsources = *p.lightSources;

        for (const ghoul::Dictionary& lsDictionary : lightsources) {
            std::unique_ptr<LightSource> lightSource =
                LightSource::createFromDictionary(lsDictionary);
            _lightSourcePropertyOwner.addPropertySubOwner(lightSource.get());
            _lightSources.push_back(std::move(lightSource));
        }
    }

    _drawWireframe = p.drawWireframe.value_or(_drawWireframe);
    addProperty(_drawWireframe);

    _wireLineWidth = p.wireLineWidth.value_or(_wireLineWidth);
    addProperty(_wireLineWidth);

    _useSmoothNormals.onChange([this]() { _tubeIsDirty = true; });
    _useSmoothNormals = p.useSmoothNormals.value_or(_useSmoothNormals);
    addProperty(_useSmoothNormals);

    _addEdges.onChange([this]() { _tubeIsDirty = true; });
    _addEdges = p.addEdges.value_or(_addEdges);
    addProperty(_addEdges);

    if (p.textureDirectory.has_value()) {
        std::filesystem::path folder = absPath(*p.textureDirectory);
        _texturesDirectory = absPath(folder).string();
    }

    _interpolationMethod.addOption(NearestInterpolation, "Nearest Neighbor");
    _interpolationMethod.addOption(LinearInterpolation, "Linear");
    addProperty(_interpolationMethod);
    if (p.interpolationMethod.has_value()) {
        const std::string interpolationMethod = *p.interpolationMethod;
        _interpolationMethod = InterpolationMapping[interpolationMethod];
    }

    _showAllTube = p.showAllTube.value_or(_showAllTube);
    addProperty(_showAllTube);

    _enableFaceCulling = p.enableFaceCulling.value_or(_enableFaceCulling);
    addProperty(_enableFaceCulling);

    _jumpToPrevPolygon.onChange([this]() { jumpToPrevPolygon(); });
    addProperty(_jumpToPrevPolygon);

    _jumpToNextPolygon.onChange([this]() { jumpToNextPolygon(); });
    addProperty(_jumpToNextPolygon);

    if (p.coloringCutplane.has_value() && (*p.coloringCutplane).colorMapping.has_value()) {
        if (!_hasColorMapFile) {
            LWARNING("Color map provided for sides of the tube but not the cutplane of the tube");
        }
        else {
            _colorSettingsCutplane.colorMapping->dataColumn.onChange(
                [this]() { _tubeIsDirty = true; }
            );

            _colorSettingsCutplane.colorMapping->valueRange = glm::vec2(0.0, 1.0);

            _colorSettingsCutplane.colorMapping->colorMapFile.onChange([this]() {
                _tubeIsDirty = true;
                _hasColorMapFile = std::filesystem::exists(
                    _colorSettingsCutplane.colorMapping->colorMapFile.value()
                );
            });
        }
    }
    addPropertySubOwner(_colorSettingsCutplane);
}

bool RenderableTube::isReady() const {
    return _shader != nullptr;
}

void RenderableTube::initialize() {
    readDataFile();
    createTube();

    if (_hasColorMapFile) {
        _colorSettings.colorMapping->initialize(_colorDataset);
        _colorSettingsCutplane.colorMapping->initialize(_colorDatasetCutplane);
    }

    for (const std::unique_ptr<LightSource>& ls : _lightSources) {
        ls->initialize();
    }
}

void RenderableTube::initializeGL() {
    _shader = global::renderEngine->buildRenderProgram(
        "TubeProgram",
        absPath("${MODULE_BASE}/shaders/tube_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/tube_fs.glsl")
    );

    _shaderCutplane = global::renderEngine->buildRenderProgram(
        "TubeProgram",
        absPath("${MODULE_BASE}/shaders/tube_cutplane_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/tube_cutplane_fs.glsl")
    );

    if (_hasColorMapFile) {
        _colorSettings.colorMapping->initializeTexture();
        _colorSettingsCutplane.colorMapping->initializeTexture();
    }

    if (_hasInterpolationTextures) {
        initializeTextures();
    }

    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vboId);
    glGenBuffers(1, &_iboId);

    glBindVertexArray(_vaoId);

    updateBufferData();

    glEnableVertexAttribArray(0);
    glVertexAttribLPointer(0, 3, GL_DOUBLE, sizeof(PolygonVertex), nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, normal))
    );

    glEnableVertexAttribArray(2);
    glVertexAttribPointer(
        2,
        1,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, value))
    );

    glEnableVertexAttribArray(3);
    glVertexAttribPointer(
        3,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, tex))
    );

    glEnableVertexAttribArray(4);
    glVertexAttribPointer(
        4,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, tex_next))
    );

    // Ending
    glGenVertexArrays(1, &_vaoIdEnding);
    glGenBuffers(1, &_vboIdEnding);
    glGenBuffers(1, &_iboIdEnding);

    glBindVertexArray(_vaoIdEnding);

    updateEndingBufferData();

    glEnableVertexAttribArray(0);
    glVertexAttribLPointer(0, 3, GL_DOUBLE, sizeof(PolygonVertex), nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, normal))
    );

    glEnableVertexAttribArray(2);
    glVertexAttribPointer(
        2,
        1,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, value))
    );

    glEnableVertexAttribArray(3);
    glVertexAttribPointer(
        3,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, tex))
    );

    glEnableVertexAttribArray(4);
    glVertexAttribPointer(
        4,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, tex_next))
    );

    glBindVertexArray(0);
}

void RenderableTube::deinitializeGL() {
    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;

    global::renderEngine->removeRenderProgram(_shaderCutplane.get());
    _shaderCutplane = nullptr;

    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vboId);
    _vboId = 0;

    glDeleteBuffers(1, &_iboId);
    _iboId = 0;

    // Ending
    glDeleteVertexArrays(1, &_vaoIdEnding);
    _vaoIdEnding = 0;

    glDeleteBuffers(1, &_vboIdEnding);
    _vboIdEnding = 0;

    glDeleteBuffers(1, &_iboIdEnding);
    _iboIdEnding = 0;

    _textures.clear();
    glDeleteTextures(1, &_texArrayId);
}

int RenderableTube::currentColorParameterIndex() const {
    const properties::OptionProperty& property =
        _colorSettings.colorMapping->dataColumn;

    if (!_hasColorMapFile || property.options().empty()) {
        return 0;
    }

    return _colorDataset.index(property.option().description);
}

int RenderableTube::currentColorCutplaneParameterIndex() const {
    const properties::OptionProperty& property =
        _colorSettingsCutplane.colorMapping->dataColumn;

    if (!_hasColorMapFile || property.options().empty()) {
        return 0;
    }

    return _colorDatasetCutplane.index(property.option().description);
}

void RenderableTube::readDataFile() {
    std::filesystem::path file = absPath(_dataFile);
    if (!std::filesystem::is_regular_file(file)) {
        LWARNING(fmt::format("The data file '{}' could not be found", file));
        return;
    }

    // Open file
    std::ifstream fileStream(file);
    if (!fileStream.good()) {
        LERROR(fmt::format("Failed to open data file '{}'", file));
        return;
    }

    // Read the entire file into a string
    constexpr size_t readSize = std::size_t(4096);
    fileStream.exceptions(std::ios_base::badbit);

    std::string data;
    std::string buf = std::string(readSize, '\0');
    while (fileStream.read(buf.data(), readSize)) {
        data.append(buf, 0, fileStream.gcount());
    }
    data.append(buf, 0, fileStream.gcount());
    fileStream.close();

    // Convert to a json object
    json jsonData = json::parse(data);

    // Check version
    bool foundVersion = false;
    if (auto version = jsonData.find("version"); version != jsonData.end()) {
        auto major = version->find("major");
        auto minor = version->find("minor");

        if (major != version->end() && minor != version->end()) {
            foundVersion = true;
            if (*major != CurrentMajorVersion || *minor != CurrentMinorVersion) {
                LWARNING(fmt::format(
                    "Unknown data version '{}.{}' found. The currently supported version "
                    "is {}.{}", major->dump(), minor->dump(), CurrentMajorVersion,
                    CurrentMinorVersion
                ));
            }
        }
    }

    if (!foundVersion) {
        LWARNING("Could not find version information, version might not be supported");
    }

    // Meta informaiton about the textures (optional)
    auto textureMeta = jsonData.find("texture-channels");
    if (textureMeta != jsonData.end()) {
        int colorDataIndex = 0;

        for (auto channelInfo = textureMeta->begin();
             channelInfo < textureMeta->end();
             ++channelInfo)
        {
            std::string channelName = channelInfo->dump();
            channelName.erase(
                std::remove(channelName.begin(), channelName.end(), '\"'),
                channelName.end()
            );

            _colorDatasetCutplane.variables.push_back({
                .index = colorDataIndex++, .name = channelName
            });
        }

        // Fill with some data even if it is not usefull
        dataloader::Dataset::Entry entry;
        entry.data.push_back(0.0);
        entry.data.push_back(1.0);
        _colorDatasetCutplane.entries.push_back(entry);

        if (colorDataIndex > 4) {
            LERROR("Texture can only handle maximum 4 channels");
        }
    }

    // Find polygons
    auto polygons = jsonData.find("polygons");
    if (polygons == jsonData.end() || polygons->size() < 1) {
        LERROR("Could not find any polygon in the data");
        return;
    }

    // Loop throught json object to fill the datastructure for the polygons
    bool isFirstPlygonAndPoint = true;
    for (auto it = polygons->begin(); it < polygons->end(); ++it) {
        TimePolygon timePolygon;

        // Timestamp
        auto time = it->find("time");
        if (time == it->end()) {
            LERROR("Could not find time for polygon in data");
            return;
        }
        std::string timeString = time->dump();
        timeString.erase(
            std::remove(timeString.begin(), timeString.end(), '\"'),
            timeString.end()
        );
        timePolygon.timestamp = Time::convertTime(timeString);

        // Center
        auto centerPt = it->find("center");
        if (centerPt == it->end()) {
            LERROR("Could not find center for polygon in data");
            return;
        }
        double x, y, z;
        centerPt->at("x").get_to(x);
        centerPt->at("y").get_to(y);
        centerPt->at("z").get_to(z);
        timePolygon.center = glm::dvec3(x, y, z);

        // Texture (optional)
        auto texPt = it->find("texture");
        if (texPt != it->end()) {
            if (_texturesDirectory.empty()) {
                LWARNING("Cannot load textures form empty texture directory");
            }
            else {
                std::string filename = texPt->dump();
                filename.erase(
                    std::remove(filename.begin(), filename.end(), '\"'),
                    filename.end()
                );
                std::filesystem::path fullPath = _texturesDirectory / filename;

                // Check that file exits
                if (!std::filesystem::is_regular_file(fullPath)) {
                    LERROR(fmt::format("Cannot find texture file {}", fullPath));
                }

                timePolygon.texturePath = fullPath;
                _hasInterpolationTextures = true;
            }
        }

        // Points
        auto points = it->find("points");
        if (points == it->end() || points->size() < 1) {
            LERROR("Could not find points for polygon in data");
            return;
        }
        for (auto pt = points->begin(); pt < points->end(); ++pt) {
            TimePolygonPoint timePolygonPoint;

            // Coordinates
            auto px = pt->find("x");
            auto py = pt->find("y");
            auto pz = pt->find("z");

            if (px == pt->end() || py == pt->end() || pz == pt->end()) {
                LERROR("Could not find coordinate component for polygon in data");
                return;
            }

            double x, y, z;
            pt->at("x").get_to(x);
            pt->at("y").get_to(y);
            pt->at("z").get_to(z);
            timePolygonPoint.coordinate = glm::dvec3(x, y, z);

            // Data values (optional)
            auto colorData = pt->find("data");
            if (colorData != pt->end() && _hasColorMapFile) {
                int colorDataIndex = 0;
                dataloader::Dataset::Entry entry;
                for (auto dt : colorData->items()) {
                    if (isFirstPlygonAndPoint) {
                        _colorDataset.variables.push_back({
                           .index = colorDataIndex++, .name = dt.key()
                        });
                    }
                    entry.data.push_back(dt.value());
                }

                _colorDataset.entries.push_back(entry);
                if (isFirstPlygonAndPoint) {
                    isFirstPlygonAndPoint = false;
                }
            }

            // Texture coordinates (optional)
            auto pu = pt->find("u");
            auto pv = pt->find("v");
            if (pu != pt->end() && pv != pt->end()) {
                float u, v;
                pt->at("u").get_to(u);
                pt->at("v").get_to(v);
                timePolygonPoint.tex = glm::vec2(u, v);
            }
            else if (_hasInterpolationTextures) {
                // Texture exist but no texture coordinates
                LERROR("Could not find texture coordinates for polygon with texture");
                _hasInterpolationTextures = false;
                return;
            }

            timePolygon.points.push_back(timePolygonPoint);
        }
        _data.push_back(timePolygon);
    }
}

void RenderableTube::initializeTextures() {
    _textures.reserve(_data.size());

    for (size_t i = 0; i < _data.size(); ++i) {
        std::unique_ptr<ghoul::opengl::Texture> t =
            ghoul::io::TextureReader::ref().loadTexture(_data[i].texturePath.string(), 2);
        if (t) {
            LINFO(fmt::format("Loaded texture {}", _data[i].texturePath));
            // Do not upload the loaded texture to the GPU, we just want it to
            // hold the data
        }
        else {
            throw ghoul::RuntimeError(fmt::format(
                "Could not find image file {}", _data[i].texturePath
            ));
        }

        // Check the resolution of first image and assume all texturea are same size
        if (i == 0) {
            _textureResolution = glm::uvec2(t->width(), t->height());
        }

        _textures.push_back(std::move(t));
    }

    // Generate textuer array
    glGenTextures(1, &_texArrayId);
    glBindTexture(GL_TEXTURE_2D_ARRAY, _texArrayId);

    // Create storage for the texture (OpenGl 4.2 and above)
    glTexStorage3D(
        GL_TEXTURE_2D_ARRAY,
        1, // No mipmaps
        GL_RGBA32F,
        _textureResolution.x,
        _textureResolution.y,
        static_cast<gl::GLsizei>(_textures.size())
    );

    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    if (_textures.size() > GL_MAX_ARRAY_TEXTURE_LAYERS) {
        LERROR("Too many textures for one texture array");
        // We will need to split the textures over several texture arrays if there are
        // too many
    }

    // Fill that storage with the data from each textures
    // TODO loop over textures instead
    for (size_t i = 0; i < _textures.size(); ++i) {
        const ghoul::opengl::Texture* texture = _textures[i].get();
        glTexSubImage3D(
            GL_TEXTURE_2D_ARRAY,
            0, // Mipmap number
            0, // xoffset
            0, // yoffset
            gl::GLint(i), // zoffset
            gl::GLsizei(_textureResolution.x), // width
            gl::GLsizei(_textureResolution.y), // height
            1, // depth
            gl::GLenum(ghoul::opengl::Texture::Format::RGBA),
            GL_UNSIGNED_BYTE, // type
            texture->pixelData()
        );
    }

    // Reset
    glBindTexture(GL_TEXTURE_2D_ARRAY, 0);
}

void RenderableTube::createTube() {
    // Tube needs at least two polygons
    const size_t nPolygons = _data.size();
    if (nPolygons < 2) {
        LERROR("Tube is empty");
        _nPolygons = 0;
        return;
    }
    else {
        _nPolygons = nPolygons;
    }

    // Polygon needs at least 3 sides
    // NOTE: assumes all polygons have the same number of points
    const size_t nPoints = _data.front().points.size();
    if (nPoints < 3) {
        LERROR("Polygons need at least 3 edges");
        _nPoints = 0;
        return;
    }
    else {
        _nPoints = nPoints;
    }

    // Reset
    _verticies.clear();
    _indicies.clear();

    // Calciulate the normals for the top and bottom
    glm::dvec3 bottomCenter = _data.front().center;
    glm::dvec3 topCenter = _data.back().center;
    glm::dvec3 bottomNormal = bottomCenter - _data[1].center;
    glm::dvec3 topNormal = topCenter - _data[_data.size() - 2].center;

    // Add the bottom verticies and indicies
    if (_addEdges) {
        unsigned int bottomCenterIndex = 0;
        addEdge(0, &_data.front(), bottomCenterIndex);
    }

    // Add the sides of the tube
    unsigned int firstSideIndex = _addEdges ? _nPoints + 1 : 0;
    if (_useSmoothNormals) {
        createSmoothTube(firstSideIndex);
    }
    else {
        createLowPolyTube(firstSideIndex);
    }

    // Add the top verticies and indicies
    if (_addEdges) {
        unsigned int topCenterIndex = _verticies.size();
        addEdge(_nPolygons - 1, & _data.back(), topCenterIndex);
    }
}

void RenderableTube::createSmoothTube(unsigned int firstSideIndex) {
    // Add verticies and indicies for the sides of the tube
    for (unsigned int polyIndex = 0; polyIndex < _nPolygons; ++polyIndex) {
        // Check if this is the the last polygon that will run in the loop
        bool isLastPoly = polyIndex == _nPolygons - 1;
        unsigned int vIndex = firstSideIndex + polyIndex * _nPoints;
        addSmoothSection(polyIndex, &_data[polyIndex], isLastPoly, vIndex);
    }
}

void RenderableTube::createLowPolyTube(unsigned int firstSideIndex) {
    // Add verticies and indices for the sides of the tube
    unsigned int vIndex = firstSideIndex;
    for (unsigned int polyIndex = 0; polyIndex < _nPolygons - 1; ++polyIndex) {
        TimePolygon* currentTimePolygon = &_data[polyIndex];
        TimePolygon* nextTimePolygon = &_data[polyIndex + 1];
        addLowPolySection(polyIndex, currentTimePolygon, nextTimePolygon, vIndex);
    }
}

void RenderableTube::addEdge(int polygonIndex, const TimePolygon const* polygon,
                             int centerIndex, bool isCutplane, double tInterpolation)
{
    // Set where to store the verticies and indicies
    std::vector<PolygonVertex>* verticies = isCutplane ? &_verticiesEnding : &_verticies;
    std::vector<unsigned int>* indicies = isCutplane ? &_indiciesCutplane : &_indicies;

    // Get the selected color parameter
    int colorParamIndex = currentColorParameterIndex();
    int pointColorIndex = polygonIndex * _nPoints;

    // Calculate the transfer function value for the center point of the given polygon
    float centerValue = 0.f;
    if (_hasColorMapFile) {
        for (size_t pointIndex = 0; pointIndex < polygon->points.size(); ++pointIndex) {
            float tempCenterValue =
                _colorDataset.entries[pointColorIndex + pointIndex].data[colorParamIndex];

            if (tInterpolation > 0.0) {
                int prevPointColorIndex = (polygonIndex - 1) * _nPoints + pointIndex;
                float prevPolyValue = _colorDataset.entries[prevPointColorIndex + pointIndex].data[colorParamIndex];
                tempCenterValue = tInterpolation * tempCenterValue + (1.0 - tInterpolation) * prevPolyValue;
            }

            centerValue += tempCenterValue;
        }
        centerValue /= _nPoints;
    }

    // Calculate texture coordinate for the center point of the given polygon
    glm::vec2 centerTex = glm::vec2(0.f);
    glm::vec2 centerTexNext = glm::vec2(0.f);
    for (const TimePolygonPoint& timePolygonPoint : polygon->points) {
        if (isCutplane) {
            centerTex += timePolygonPoint.tex;
            centerTexNext += timePolygonPoint.tex_next;
        }
        else {
            centerTex += timePolygonPoint.tex;
        }
    }

    if (isCutplane) {
        centerTexNext /= _nPoints;
    }
    centerTex /= _nPoints;

    // Add the center point of the edge
    PolygonVertex centerPoint;
    centerPoint.position[0] = polygon->center.x;
    centerPoint.position[1] = polygon->center.y;
    centerPoint.position[2] = polygon->center.z;

    // Calculate the normal, we know there are at least 3 point in the polygon
    glm::dvec3 v0 = polygon->center;
    glm::dvec3 v1 = polygon->points[0].coordinate;
    glm::dvec3 v2 = polygon->points[1].coordinate;

    glm::dvec3 a = glm::normalize(v1 - v0);
    glm::dvec3 b = glm::normalize(v2 - v0);

    // For the first edge make the normal point towards the bottom of the tube
    // Otherwise the normal should point to the top of hte tube
    glm::dvec3 normal = polygonIndex == 0 ? glm::cross(a, b) : glm::cross(b, a);

    centerPoint.normal[0] = normal.x;
    centerPoint.normal[1] = normal.y;
    centerPoint.normal[2] = normal.z;

    if (_hasColorMapFile) {
        centerPoint.value = centerValue;
    }

    centerPoint.tex[0] = centerTex.x;
    centerPoint.tex[1] = centerTex.y;

    if (isCutplane) {
        centerPoint.tex_next[0] = centerTexNext.x;
        centerPoint.tex_next[1] = centerTexNext.y;
    }

    verticies->push_back(centerPoint);

    // Add the side verticies for the edge
    for (size_t pointIndex = 0; pointIndex < polygon->points.size(); ++pointIndex) {
        PolygonVertex sidePoint;
        sidePoint.position[0] = polygon->points[pointIndex].coordinate.x;
        sidePoint.position[1] = polygon->points[pointIndex].coordinate.y;
        sidePoint.position[2] = polygon->points[pointIndex].coordinate.z;

        sidePoint.normal[0] = normal.x;
        sidePoint.normal[1] = normal.y;
        sidePoint.normal[2] = normal.z;

        if (_hasColorMapFile) {
            sidePoint.value =
                _colorDataset.entries[pointColorIndex + pointIndex].data[colorParamIndex];

            if (tInterpolation > 0.0) {
                int prevPointColorIndex = (polygonIndex - 1) * _nPoints + pointIndex;
                float prevPolyValue = _colorDataset.entries[prevPointColorIndex].data[colorParamIndex];
                sidePoint.value = tInterpolation * sidePoint.value + (1.0 - tInterpolation) * prevPolyValue;
            }
        }

        if (isCutplane) {
            sidePoint.tex[0] = polygon->points[pointIndex].tex.x;
            sidePoint.tex[1] = polygon->points[pointIndex].tex.y;

            sidePoint.tex_next[0] = polygon->points[pointIndex].tex_next.x;
            sidePoint.tex_next[1] = polygon->points[pointIndex].tex_next.y;
        }
        else {
            sidePoint.tex[0] = polygon->points[pointIndex].tex.x;
            sidePoint.tex[1] = polygon->points[pointIndex].tex.y;
        }

        verticies->push_back(sidePoint);
    }

    // Add Indices for edge
    for (unsigned int pointIndex = 0; pointIndex < _nPoints; ++pointIndex) {
        bool isLast = pointIndex == _nPoints - 1;

        unsigned int v0 = centerIndex;
        unsigned int v1 = centerIndex + pointIndex + 1;
        unsigned int v2 = isLast ? v0 + 1 : v1 + 1;

        indicies->push_back(v0);

        // For the first edge make the normal point towards the bottom of the tube
        // Otherwise the normal should point to the top of hte tube
        if (polygonIndex == 0) {
            indicies->push_back(v1);
            indicies->push_back(v2);
        }
        else {
            indicies->push_back(v2);
            indicies->push_back(v1);
        }
    }
}

void RenderableTube::addSmoothSection(int polygonIndex, const TimePolygon const* polygon,
                                      bool isLastPoly, unsigned int vIndex,
                                      bool isEnding, double tInterpolation)
{
    // Set where to store the verticies and indicies
    std::vector<PolygonVertex>* verticies = isEnding ? &_verticiesEnding : &_verticies;
    std::vector<unsigned int>* indicies = isEnding ? &_indiciesEnding : &_indicies;

    // Get the selected color parameter
    int colorParamIndex = currentColorParameterIndex();

    // Add the verticies and indicies for the polygon
    for (unsigned int pointIndex = 0; pointIndex < _nPoints; ++pointIndex) {
        bool isLast = pointIndex == _nPoints - 1;

        PolygonVertex sidePoint;
        sidePoint.position[0] = polygon->points[pointIndex].coordinate.x;
        sidePoint.position[1] = polygon->points[pointIndex].coordinate.y;
        sidePoint.position[2] = polygon->points[pointIndex].coordinate.z;

        // Calculate normal
        glm::dvec3 normal =
            polygon->points[pointIndex].coordinate - polygon->center;
        sidePoint.normal[0] = normal.x;
        sidePoint.normal[1] = normal.y;
        sidePoint.normal[2] = normal.z;

        if (_hasColorMapFile) {
            int pointColorIndex = polygonIndex * _nPoints + pointIndex;
            float value = _colorDataset.entries[pointColorIndex].data[colorParamIndex];

            if (tInterpolation > 0.0) {
                int prevPointColorIndex = (polygonIndex - 1)* _nPoints + pointIndex;
                float prevPolyValue = _colorDataset.entries[prevPointColorIndex].data[colorParamIndex];
                value = tInterpolation * value + (1.0 - tInterpolation) * prevPolyValue;
            }

            sidePoint.value = value;
        }

        sidePoint.tex[0] = polygon->points[pointIndex].tex.x;
        sidePoint.tex[1] = polygon->points[pointIndex].tex.y;

        verticies->push_back(sidePoint);

        // Add indicies
        if (isLastPoly) {
            // The indicies for the last polygon have already been added before
            continue;
        }

        // Add the indicies and connect this polygon to the next
        // v0 is the current point in this polygon
        unsigned int v0 = vIndex + pointIndex;
        // v1 is the coresponding current point in the next polygon
        unsigned int v1 = v0 + _nPoints;
        // v2 is the coresponding next point in the next polygon
        unsigned int v2 = isLast ? v1 + 1 - _nPoints : v1 + 1;
        // v3 is the next point in this polygon
        unsigned int v3 = isLast ? v0 + 1 - _nPoints : v0 + 1;

        // 2 triangles per sector
        indicies->push_back(v0);
        indicies->push_back(v1);
        indicies->push_back(v2);

        indicies->push_back(v0);
        indicies->push_back(v2);
        indicies->push_back(v3);
    }
}

void RenderableTube::addLowPolySection(int polygonIndex, const TimePolygon const* polygon,
                                       const TimePolygon const* nextPolygon,
                                       unsigned int& vIndex, double tInterpolation)
{
    // Set where to store the verticies and indicies
    std::vector<PolygonVertex>* verticies =
        tInterpolation > 0.0 ? &_verticiesEnding : &_verticies;
    std::vector<unsigned int>* indicies =
        tInterpolation > 0.0 ? &_indiciesEnding : &_indicies;

    // Get the selected color parameter
    int colorParamIndex = currentColorParameterIndex();

    // Add verticies for this section
    const unsigned int nPointsPerSide = 4;
    for (unsigned int pointIndex = 0; pointIndex < _nPoints; ++pointIndex) {
        bool isLast = pointIndex == _nPoints - 1;

        // Identify all the points that are included in this section
        // v0 is the current point in polygon
        TimePolygonPoint v0 = polygon->points[pointIndex];
        // v1 is the coresponding current point in the nextPolygon
        TimePolygonPoint v1 = nextPolygon->points[pointIndex];
        // v2 is the coresponding next point in the nextPolygon
        TimePolygonPoint v2 = isLast ?
            nextPolygon->points[pointIndex + 1 - _nPoints] :
            nextPolygon->points[pointIndex + 1];
        // v3 is the next point in the polygon
        TimePolygonPoint v3 = isLast ?
            polygon->points[pointIndex + 1 - _nPoints] :
            polygon->points[pointIndex + 1];

        // Create the verticies for all points in this section
        PolygonVertex sidePointV0, sidePointV1, sidePointV2, sidePointV3;

        // Position
        sidePointV0.position[0] = v0.coordinate.x;
        sidePointV0.position[1] = v0.coordinate.y;
        sidePointV0.position[2] = v0.coordinate.z;

        sidePointV1.position[0] = v1.coordinate.x;
        sidePointV1.position[1] = v1.coordinate.y;
        sidePointV1.position[2] = v1.coordinate.z;

        sidePointV2.position[0] = v2.coordinate.x;
        sidePointV2.position[1] = v2.coordinate.y;
        sidePointV2.position[2] = v2.coordinate.z;

        sidePointV3.position[0] = v3.coordinate.x;
        sidePointV3.position[1] = v3.coordinate.y;
        sidePointV3.position[2] = v3.coordinate.z;

        // Normal
        glm::dvec3 toNextPoly = glm::normalize(v1.coordinate - v0.coordinate);
        glm::dvec3 toNextPoint = glm::normalize(v3.coordinate - v0.coordinate);
        glm::dvec3 normal = glm::cross(toNextPoly, toNextPoint);

        sidePointV0.normal[0] = normal.x;
        sidePointV0.normal[1] = normal.y;
        sidePointV0.normal[2] = normal.z;

        sidePointV1.normal[0] = normal.x;
        sidePointV1.normal[1] = normal.y;
        sidePointV1.normal[2] = normal.z;

        sidePointV2.normal[0] = normal.x;
        sidePointV2.normal[1] = normal.y;
        sidePointV2.normal[2] = normal.z;

        sidePointV3.normal[0] = normal.x;
        sidePointV3.normal[1] = normal.y;
        sidePointV3.normal[2] = normal.z;

        // Value
        if (_hasColorMapFile) {
            int pointColorIndex = polygonIndex * _nPoints + pointIndex;

            // v0 is the current point in polygon
            int pointColorIndexV0 = pointColorIndex;
            float v0Value =
                _colorDataset.entries[pointColorIndexV0].data[colorParamIndex];

            // v1 is the coresponding current point in the nextPolygon
            int pointColorIndexV1 = pointColorIndex + _nPoints;
            float v1Value =
                _colorDataset.entries[pointColorIndexV1].data[colorParamIndex];

            // v2 is the coresponding next point in the nextPolygon
            int pointColorIndexV2 =
                isLast ? pointColorIndex + 1 : pointColorIndex + _nPoints + 1;
            float v2Value =
                _colorDataset.entries[pointColorIndexV2].data[colorParamIndex];

            // v3 is the next point in the polygon
            int pointColorIndexV3 =
                isLast ? pointColorIndex + 1 - _nPoints : pointColorIndex + 1;
            float v3Value =
                _colorDataset.entries[pointColorIndexV3].data[colorParamIndex];

            sidePointV0.value = v0Value;
            sidePointV1.value = tInterpolation > 0.0 ?
                tInterpolation * v1Value + (1.0 - tInterpolation) * v0Value :
                v1Value;
            sidePointV2.value = tInterpolation > 0.0 ?
                tInterpolation * v2Value + (1.0 - tInterpolation) * v3Value :
                v2Value;
            sidePointV3.value = v3Value;
        }

        // Texture coordinate
        sidePointV0.tex[0] = v0.tex.x;
        sidePointV0.tex[1] = v0.tex.y;

        sidePointV1.tex[0] = v1.tex.x;
        sidePointV1.tex[1] = v1.tex.y;

        sidePointV2.tex[0] = v2.tex.x;
        sidePointV2.tex[1] = v2.tex.y;

        sidePointV3.tex[0] = v3.tex.x;
        sidePointV3.tex[1] = v3.tex.y;

        // Add all points to the list
        verticies->push_back(sidePointV0);
        verticies->push_back(sidePointV1);
        verticies->push_back(sidePointV2);
        verticies->push_back(sidePointV3);

        // Add indicies for this point
        unsigned int indexV0 = vIndex;
        unsigned int indexV1 = indexV0 + 1;
        unsigned int indexV2 = indexV1 + 1;
        unsigned int indexV3 = indexV2 + 1;
        vIndex += nPointsPerSide;

        // 2 triangles per side
        indicies->push_back(indexV0);
        indicies->push_back(indexV1);
        indicies->push_back(indexV2);

        indicies->push_back(indexV0);
        indicies->push_back(indexV2);
        indicies->push_back(indexV3);
    }
}

RenderableTube::FindTimeStruct RenderableTube::findTime(double time) const {
    FindTimeStruct result;

    // Find the polygon before and after the current time
    double nextPolygonTime = std::numeric_limits<double>::max();

    for (size_t i = 0; i < _data.size(); ++i) {
        // Found a time smaller than now
        if (_data[i].timestamp < time) {
            result.lastPolygonBeforeTime = i;
            result.foundPrev = true;
        }
        // Found a time larger than now
        else if (_data[i].timestamp > time && _data[i].timestamp < nextPolygonTime) {
            nextPolygonTime = _data[i].timestamp;
            result.firstPolygonAfterTime = i;
        }
        // Found a time exactly equal to now
        else if (std::abs(_data[i].timestamp - time) <
                 std::numeric_limits<double>::epsilon())
        {
            result.lastPolygonBeforeTime = i;
            result.foundPrev = true;
            result.onSlice = true;
        }
    }

    return result;
}

void RenderableTube::jumpToPrevPolygon() const {
    double now = global::timeManager->time().j2000Seconds();

    // Find the polygons that are closest to the current time
    FindTimeStruct result = findTime(now);
    double prevTime = _data[result.lastPolygonBeforeTime].timestamp;

    // If we are exactly on a polygon, take the previous one instead of the current one
    if (std::abs(now - prevTime) < std::numeric_limits<double>::epsilon()) {
        result = findTime(now - 1);
        prevTime = _data[result.lastPolygonBeforeTime].timestamp;
    }

    // Before beginning
    if (!result.foundPrev) {
        LWARNING("Current time is before the start time for the tube");
        return;
    }

    global::timeManager->setTimeNextFrame(Time(prevTime));
}

void RenderableTube::jumpToNextPolygon() const {
    double now = global::timeManager->time().j2000Seconds();

    // Find the polygons that are closest to the current time
    FindTimeStruct result = findTime(now);
    double nextTime = _data[result.firstPolygonAfterTime].timestamp;

    // If we are exactly on a polygon, take the next one instead of the current one
    if (std::abs(now - nextTime) < std::numeric_limits<double>::epsilon()) {
        result = findTime(now + 1);
        nextTime = _data[result.firstPolygonAfterTime].timestamp;
    }

    // After end
    if (result.firstPolygonAfterTime == std::numeric_limits<size_t>::max()) {
        LWARNING("Current time is after the end time for the tube");
        return;
    }

    global::timeManager->setTimeNextFrame(Time(nextTime));
}

void RenderableTube::interpolateEnd(double now) {
    // Find the polygons that are closest to the current time
    FindTimeStruct result = findTime(now);

    _interpolationNeeded = true;
    if (result.onSlice) {
        _interpolationNeeded = false;
    }
    _lastPolygonBeforeNow = result.lastPolygonBeforeTime;
    _firstPolygonAfterNow = result.firstPolygonAfterTime;

    // Count the number of indicies in the tube up to and including the
    // _lastPolygonBeforeNow polygon
    int nIndiciesUntilNow = 0;
    // Before beginning
    if (!result.foundPrev) {
        // Do not show anything
        nIndiciesUntilNow = 0;
        _interpolationNeeded = false;
    }
    // At or after end
    else if (_lastPolygonBeforeNow == _nPolygons - 1) {
        // Show all of the tube
        nIndiciesUntilNow = _indicies.size();
        _interpolationNeeded = false;
    }
    // Middle
    else {
        // First add the bottom
        if (_addEdges) {
            nIndiciesUntilNow += static_cast<int>(_nPoints * 3);
        }

        // Show all sections until and including the _lastPolygonBeforeNow polygon
        const unsigned int nIndiciesPerSection = 6;
        nIndiciesUntilNow +=
            static_cast<int>(_lastPolygonBeforeNow * _nPoints * nIndiciesPerSection);
    }

    if (nIndiciesUntilNow > _indicies.size()) {
        LERROR("Cannot render more verticies than what is in the tube");
        _nIndiciesToRender = 0;
    }
    else {
        _nIndiciesToRender = nIndiciesUntilNow;
    }

    // Interpolate the last step
    if (_interpolationNeeded) {
        creteEnding(now);
        updateEndingBufferData();
    }
    // Add cutplane even if exactly on a slice
    else if (result.onSlice && _addEdges) {
        // Reset
        _verticiesEnding.clear();
        _indiciesEnding.clear();
        _indiciesCutplane.clear();

        // Add cutplane exactly at polygon _lastPolygonBeforeNow
        TimePolygon currentTimePolygon = _data[_lastPolygonBeforeNow];

        // Add texture coordinates for adjacent plane
        // Since we know that this plane is exactly on the first data slice there will
        // not be any interpolation, so we can just copy the same coordinate again
        for (unsigned int pointIndex = 0; pointIndex < _nPoints; ++pointIndex) {
            currentTimePolygon.points[pointIndex].tex_next =
                _data[_lastPolygonBeforeNow].points[pointIndex].tex;
        }

        addEdge(_lastPolygonBeforeNow, &currentTimePolygon, 0, true);
        updateEndingBufferData();
    }

    glBindVertexArray(0);
}

void RenderableTube::creteEnding(double now) {
    // Reset
    _verticiesEnding.clear();
    _indiciesEnding.clear();
    _indiciesCutplane.clear();

    // Interpolate to find current data
    double prevTime = _data[_lastPolygonBeforeNow].timestamp;
    double nextTime = _data[_firstPolygonAfterNow].timestamp;
    double t = (now - prevTime) / (nextTime - prevTime);
    _tValue = t;

    // Create a temporary TimePolygon at time t between prev and next using interpolation
    const TimePolygon const* prevTimePolygon = &_data[_lastPolygonBeforeNow];
    const TimePolygon const* nextTimePolygon = &_data[_firstPolygonAfterNow];
    TimePolygon currentTimePolygon;
    currentTimePolygon.timestamp = now;
    currentTimePolygon.center =
        t * nextTimePolygon->center + (1.0 - t) * prevTimePolygon->center;

    // Add interpolated points
    currentTimePolygon.points.reserve(_nPoints);
    for (unsigned int pointIndex = 0; pointIndex < _nPoints; ++pointIndex) {
        TimePolygonPoint currentTimePolygonPoint;
        currentTimePolygonPoint.coordinate =
            t * nextTimePolygon->points[pointIndex].coordinate +
            (1.0 - t) * prevTimePolygon->points[pointIndex].coordinate;

        // Texture coordinate
        currentTimePolygonPoint.tex = _data[_lastPolygonBeforeNow].points[pointIndex].tex;
        currentTimePolygonPoint.tex_next = _data[_firstPolygonAfterNow].points[pointIndex].tex;

        currentTimePolygon.points.push_back(currentTimePolygonPoint);
    }

    if (_useSmoothNormals) {
        createSmoothEnding(prevTimePolygon, &currentTimePolygon);
    }
    else {
        createLowPolyEnding(prevTimePolygon, &currentTimePolygon);
    }

    // Add cutplane
    if (_addEdges) {
        unsigned int centerIndex = _verticiesEnding.size();
        addEdge(_firstPolygonAfterNow, &currentTimePolygon, centerIndex, true, t);
    }
}

void RenderableTube::createSmoothEnding(const TimePolygon const* prevTimePolygon,
                                        const TimePolygon const* currentTimePolygon)
{
    // Add the trianles of the ending
    unsigned int vIndex = 0;
    addSmoothSection(
        _lastPolygonBeforeNow,
        prevTimePolygon,
        false, // Not the last polygon in this section
        vIndex,
        true // This is part of the ending
    );

    vIndex += _nPoints;
    addSmoothSection(
        _firstPolygonAfterNow,
        currentTimePolygon,
        true, // The last polygon in this section
        vIndex,
        true, // This is part of the ending
        _tValue
    );
}

void RenderableTube::createLowPolyEnding(const TimePolygon const* prevTimePolygon,
                                         const TimePolygon const* currentTimePolygon)
{
    // Add the trianles of the ending
    int pointColorIndex = _lastPolygonBeforeNow * _nPoints;
    unsigned int vIndex = 0;
    addLowPolySection(
        _lastPolygonBeforeNow,
        prevTimePolygon,
        currentTimePolygon,
        vIndex,
        _tValue
    );
}

void RenderableTube::setCommonUniforms(ghoul::opengl::ProgramObject* shader, const RenderData& data) {
    shader->setUniform("opacity", opacity());

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 modelViewTransform = calcModelViewTransform(data);
    glm::dmat4 normalTransform = glm::transpose(glm::inverse(modelViewTransform));

    shader->setUniform("modelViewTransform", modelViewTransform);
    shader->setUniform(
        "projectionTransform",
        glm::dmat4(data.camera.projectionMatrix())
    );
    shader->setUniform("normalTransform", glm::mat3(normalTransform));

    // Shading and light settings
    int nLightSources = 0;
    _lightIntensitiesBuffer.resize(_lightSources.size());
    _lightDirectionsViewSpaceBuffer.resize(_lightSources.size());
    for (const std::unique_ptr<LightSource>& lightSource : _lightSources) {
        if (!lightSource->isEnabled()) {
            continue;
        }
        _lightIntensitiesBuffer[nLightSources] = lightSource->intensity();
        _lightDirectionsViewSpaceBuffer[nLightSources] =
            lightSource->directionViewSpace(data);

        ++nLightSources;
    }

    shader->setUniform("performShading", _shading.enabled);
    if (_shading.enabled) {
        shader->setUniform("nLightSources", nLightSources);
        shader->setUniform("lightIntensities", _lightIntensitiesBuffer);
        shader->setUniform(
            "lightDirectionsViewSpace",
            _lightDirectionsViewSpaceBuffer
        );

        shader->setUniform("ambientIntensity", _shading.ambientIntensity);
        shader->setUniform("diffuseIntensity", _shading.diffuseIntensity);
        shader->setUniform("specularIntensity", _shading.specularIntensity);
    }
}

void RenderableTube::render(const RenderData& data, RendererTasks&) {
    if (_nIndiciesToRender == 0) {
        return;
    }

    _shader->activate();

    // Uniforms
    setCommonUniforms(_shader.get(), data);

    // Colormap settings
    ghoul::opengl::TextureUnit colorMapTextureUnit;
    _shader->setUniform("colorMapTexture", colorMapTextureUnit);
    bool useColorMap = _hasColorMapFile && _colorSettings.colorMapping->enabled &&
        _colorSettings.colorMapping->texture();
    if (useColorMap) {
        colorMapTextureUnit.activate();
        _colorSettings.colorMapping->texture()->bind();
    }
    _shader->setUniform("useColorMap", useColorMap);

    _shader->setUniform("color", _colorSettings.tubeColor);

    if (useColorMap) {
        const glm::vec2 range = _colorSettings.colorMapping->valueRange;
        _shader->setUniform("cmapRangeMin", range.x);
        _shader->setUniform("cmapRangeMax", range.y);
        _shader->setUniform(
            "hideOutsideRange",
            _colorSettings.colorMapping->hideOutsideRange
        );

        _shader->setUniform(
            "nanColor",
            _colorSettings.colorMapping->nanColor
        );
        _shader->setUniform(
            "useNanColor",
            _colorSettings.colorMapping->useNanColor
        );

        _shader->setUniform(
            "aboveRangeColor",
            _colorSettings.colorMapping->aboveRangeColor
        );
        _shader->setUniform(
            "useAboveRangeColor",
            _colorSettings.colorMapping->useAboveRangeColor
        );

        _shader->setUniform(
            "belowRangeColor",
            _colorSettings.colorMapping->belowRangeColor
        );
        _shader->setUniform(
            "useBelowRangeColor",
            _colorSettings.colorMapping->useBelowRangeColor
        );
    }


    // Settings
    if (!_enableFaceCulling) {
        glDisable(GL_CULL_FACE);
    }

    if (_drawWireframe) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

#ifndef __APPLE__
        glLineWidth(_wireLineWidth);
#else
        glLineWidth(1.f);
#endif
    }

    // Render
    glBindVertexArray(_vaoId);
    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(_nIndiciesToRender),
        GL_UNSIGNED_INT,
        nullptr
    );

    // Render the last section until now with interpolation
    if (_interpolationNeeded && !_showAllTube) {
        glBindVertexArray(_vaoIdEnding);
        glDrawElements(
            GL_TRIANGLES,
            static_cast<GLsizei>(_indiciesEnding.size()),
            GL_UNSIGNED_INT,
            nullptr
        );
    }

    // Render the cutplane
    if (_addEdges && !_showAllTube &&
        (_interpolationNeeded || _nIndiciesToRender < _indicies.size()))
    {
        // Use the texture based shader instead for the cutplane if textures exist
        if (_hasInterpolationTextures) {
            // Switch shader
            _shaderCutplane->activate();

            // Uniforms
            setCommonUniforms(_shaderCutplane.get(), data);

            // Colormap settings
            ghoul::opengl::TextureUnit colorMapTextureUnit;
            _shaderCutplane->setUniform("colorMapTexture", colorMapTextureUnit);
            bool useColorMap = _hasColorMapFile && _colorSettingsCutplane.colorMapping->enabled &&
                _colorSettingsCutplane.colorMapping->texture();
            if (useColorMap) {
                colorMapTextureUnit.activate();
                _colorSettingsCutplane.colorMapping->texture()->bind();
            }
            _shaderCutplane->setUniform("useColorMap", useColorMap);

            _shaderCutplane->setUniform("color", _colorSettingsCutplane.fixedColor);

            if (useColorMap) {
                const glm::vec2 range = _colorSettingsCutplane.colorMapping->valueRange;
                _shaderCutplane->setUniform("cmapRangeMin", range.x);
                _shaderCutplane->setUniform("cmapRangeMax", range.y);
                _shaderCutplane->setUniform(
                    "hideOutsideRange",
                    _colorSettingsCutplane.colorMapping->hideOutsideRange
                );

                _shaderCutplane->setUniform(
                    "nanColor",
                    _colorSettingsCutplane.colorMapping->nanColor
                );
                _shaderCutplane->setUniform(
                    "useNanColor",
                    _colorSettingsCutplane.colorMapping->useNanColor
                );

                _shaderCutplane->setUniform(
                    "aboveRangeColor",
                    _colorSettingsCutplane.colorMapping->aboveRangeColor
                );
                _shaderCutplane->setUniform(
                    "useAboveRangeColor",
                    _colorSettingsCutplane.colorMapping->useAboveRangeColor
                );

                _shaderCutplane->setUniform(
                    "belowRangeColor",
                    _colorSettingsCutplane.colorMapping->belowRangeColor
                );
                _shaderCutplane->setUniform(
                    "useBelowRangeColor",
                    _colorSettingsCutplane.colorMapping->useBelowRangeColor
                );
            }

            _shaderCutplane->setUniform(
                "selectedChannel",
                currentColorCutplaneParameterIndex()
            );

            _shaderCutplane->setUniform(
                "hasInterpolationTexture",
                _hasInterpolationTextures
            );
            _shaderCutplane->setUniform(
                "useNearesNeighbor",
                _interpolationMethod == NearestInterpolation
            );
            _shaderCutplane->setUniform("interpolationTime", _tValue);

            // Cutplane textures
            ghoul::opengl::TextureUnit texturesUnit;
            _shaderCutplane->setUniform("textures", texturesUnit);
            texturesUnit.activate();
            glBindTexture(GL_TEXTURE_2D_ARRAY, _texArrayId);

            // Find the polygons corresponding to before and after now
            double now = data.time.j2000Seconds();
            FindTimeStruct result = findTime(now);
            double prevTime = _data[result.lastPolygonBeforeTime].timestamp;
            double nextTime = _data[result.firstPolygonAfterTime].timestamp;

            // Check if time is before or after valid time for tube
            if (!result.foundPrev) {
                LWARNING("Current time is before the start time for the tube");
                result.lastPolygonBeforeTime = 0;
            }
            if (result.firstPolygonAfterTime == std::numeric_limits<size_t>::max()) {
                LWARNING("Current time is after the end time for the tube");
                result.firstPolygonAfterTime = _data.size() - 1;
            }

            _shaderCutplane->setUniform(
                "prev_texture_index",
                static_cast<int>(result.lastPolygonBeforeTime)
            );
            _shaderCutplane->setUniform(
                "next_texture_index",
                static_cast<int>(result.firstPolygonAfterTime)
            );
        }

        // Bind the cutplane ibo instead
        glBindVertexArray(_vaoIdEnding);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboIdEnding);
        glBufferData(
            GL_ELEMENT_ARRAY_BUFFER,
            _indiciesCutplane.size() * sizeof(unsigned int),
            _indiciesCutplane.data(),
            GL_STREAM_DRAW
        );

        // Render the cutplane
        glDrawElements(
            GL_TRIANGLES,
            static_cast<GLsizei>(_indiciesCutplane.size()),
            GL_UNSIGNED_INT,
            nullptr
        );

        _shaderCutplane->deactivate();
    }

    // Reset
    if (!_enableFaceCulling) {
        glEnable(GL_CULL_FACE);
    }

    if (_drawWireframe) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
        global::renderEngine->openglStateCache().resetLineState();
    }

    glBindVertexArray(0);
    glBindTexture(GL_TEXTURE_2D_ARRAY, 0);
    global::renderEngine->openglStateCache().resetLineState();
    _shader->deactivate();
}

void RenderableTube::updateBufferData() {
    glBindVertexArray(_vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, _vaoId);
    glBufferData(
        GL_ARRAY_BUFFER,
        _verticies.size() * sizeof(PolygonVertex),
        _verticies.data(),
        GL_STREAM_DRAW
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboId);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        _indicies.size() * sizeof(unsigned int),
        _indicies.data(),
        GL_STREAM_DRAW
    );
}

void RenderableTube::updateEndingBufferData() {
    glBindVertexArray(_vaoIdEnding);
    glBindBuffer(GL_ARRAY_BUFFER, _vboIdEnding);
    glBufferData(
        GL_ARRAY_BUFFER,
        _verticiesEnding.size() * sizeof(PolygonVertex),
        _verticiesEnding.data(),
        GL_STREAM_DRAW
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboIdEnding);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        _indiciesEnding.size() * sizeof(unsigned int),
        _indiciesEnding.data(),
        GL_STREAM_DRAW
    );
}

void RenderableTube::update(const UpdateData& data) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }

    if (_shaderCutplane->isDirty()) {
        _shaderCutplane->rebuildFromFile();
    }

    if (_hasColorMapFile) {
        _colorSettings.colorMapping->update(_colorDataset);
        _colorSettingsCutplane.colorMapping->update(_colorDatasetCutplane);
    }

    if (_tubeIsDirty) {
        createTube();
        updateBufferData();
        //setBoundingSphere(???);
        _tubeIsDirty = false;
    }

    if (_showAllTube) {
        _nIndiciesToRender = _indicies.size();
        return;
    }

    interpolateEnd(data.time.j2000Seconds());
}

} // namespace openspace
