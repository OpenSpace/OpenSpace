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

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <modules/debugging/rendering/debugrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/chunk/chunknode.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/rendering/layer/layer.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/debugging/rendering/debugrenderer.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texture.h>

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/geometry/ellipsoid.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/meshes/grid.h>
#include <modules/globebrowsing/meshes/trianglesoup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/gpu/gpulayergroup.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/opengl/programobject.h>
#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/geometry/geodetic3.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <openspace/util/updatestructures.h>
#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/rendering/layer/layer.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>


namespace {
    constexpr const char* keyFrame = "Frame";
    constexpr const char* keyRadii = "Radii";
    constexpr const char* keySegmentsPerPatch = "SegmentsPerPatch";
    constexpr const char* keyLayers = "Layers";
    constexpr const char* keyShadowGroup = "ShadowGroup";
    constexpr const char* keyShadowSource = "Source";
    constexpr const char* keyShadowCaster = "Caster";
    constexpr const double KM_TO_M = 1000.0;
    const openspace::globebrowsing::AABB3 CullingFrustum(glm::vec3(-1, -1, 0), glm::vec3(1, 1, 1e35));

    constexpr const int DefaultSkirtedGridSegments = 64;
    constexpr static const int UnknownDesiredLevel = -1;

    const openspace::globebrowsing::GeodeticPatch Coverage =
        openspace::globebrowsing::GeodeticPatch(0, 0, 90, 180);

    const openspace::globebrowsing::TileIndex LeftHemisphereIndex =
        openspace::globebrowsing::TileIndex(0, 0, 1);

    const openspace::globebrowsing::TileIndex RightHemisphereIndex =
        openspace::globebrowsing::TileIndex(1, 0, 1);

    constexpr openspace::properties::Property::PropertyInfo SaveOrThrowInfo = {
        "SaveOrThrowCamera",
        "Save or throw camera",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ShowChunkEdgeInfo = {
        "ShowChunkEdges",
        "Show chunk edges",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ShowChunkBoundsInfo = {
        "ShowChunkBounds",
        "Show chunk bounds",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ShowChunkAABBInfo = {
        "ShowChunkAABB",
        "Show chunk AABB",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo HeightResolutionInfo = {
        "ShowHeightResolution",
        "Show height resolution",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo HeightIntensityInfo = {
        "ShowHeightIntensities",
        "Show height intensities",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo FrustumCullingInfo = {
        "PerformFrustumCulling",
        "Perform frustum culling",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo HorizonCullingInfo = {
        "PerformHorizonCulling",
        "Perform horizon culling",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo LevelProjectedAreaInfo = {
        "LevelByProjectedAreaElseDistance",
        "Level by projected area (else distance)",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ResetTileProviderInfo = {
        "ResetTileProviders",
        "Reset tile providers",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo LimitLevelInfo = {
        "LimitLevelByAvailableData",
        "Limit level by available data",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ModelSpaceRenderingInfo = {
        "ModelSpaceRenderingCutoffLevel",
        "Model Space Rendering Cutoff Level",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo PerformShadingInfo = {
        "PerformShading",
        "Perform shading",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo AccurateNormalsInfo = {
        "UseAccurateNormals",
        "Use Accurate Normals",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo EclipseInfo = {
        "Eclipse",
        "Eclipse",
        "Enables/Disable Eclipse shadows"
    };

    constexpr openspace::properties::Property::PropertyInfo EclipseHardShadowsInfo = {
        "EclipseHardShadows",
        "Eclipse Hard Shadows",
        "Enables the rendering of eclipse shadows using hard shadows"
    };

    constexpr openspace::properties::Property::PropertyInfo LodScaleFactorInfo = {
        "LodScaleFactor",
        "Level of Detail Scale Factor",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo CameraMinHeightInfo = {
        "CameraMinHeight",
        "Camera Minimum Height",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo OrenNayarRoughnessInfo = {
        "OrenNayarRoughness",
        "orenNayarRoughness",
        "" // @TODO Missing documentation
    };
} // namespace

using namespace openspace::properties;

namespace openspace::globebrowsing {

RenderableGlobe::RenderableGlobe(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _debugProperties({
        BoolProperty(SaveOrThrowInfo, false),
        BoolProperty(ShowChunkEdgeInfo, false),
        BoolProperty(ShowChunkBoundsInfo, false),
        BoolProperty(ShowChunkAABBInfo, false),
        BoolProperty(HeightResolutionInfo, false),
        BoolProperty(HeightIntensityInfo, false),
        BoolProperty(FrustumCullingInfo, true),
        BoolProperty(HorizonCullingInfo, true),
        BoolProperty(LevelProjectedAreaInfo, false),
        BoolProperty(ResetTileProviderInfo, false),
        BoolProperty(LimitLevelInfo, true),
        IntProperty(ModelSpaceRenderingInfo, 10, 1, 22)
    })
    , _generalProperties({
        BoolProperty(PerformShadingInfo, true),
        BoolProperty(AccurateNormalsInfo, false),
        BoolProperty(EclipseInfo, false),
        BoolProperty(EclipseHardShadowsInfo, false),
        FloatProperty(LodScaleFactorInfo, 10.f, 1.f, 50.f),
        FloatProperty(CameraMinHeightInfo, 100.f, 0.f, 1000.f),
        FloatProperty(OrenNayarRoughnessInfo, 0.f, 0.f, 1.f)
    })
    , _debugPropertyOwner({ "Debug" })
    , _leftRoot(std::make_unique<ChunkNode>(Chunk(*this, LeftHemisphereIndex)))
    , _rightRoot(std::make_unique<ChunkNode>(Chunk(*this, RightHemisphereIndex)))
    , _grid(DefaultSkirtedGridSegments,
        DefaultSkirtedGridSegments,
        TriangleSoup::Positions::No,
        TriangleSoup::TextureCoordinates::Yes,
        TriangleSoup::Normals::No)
{
    setIdentifier("RenderableGlobe");

    // Read the radii in to its own dictionary
    if (dictionary.hasKeyAndValue<glm::dvec3>(keyRadii)) {
        _ellipsoid = Ellipsoid(dictionary.value<glm::vec3>(keyRadii));
        setBoundingSphere(static_cast<float>(_ellipsoid.maximumRadius()));
    }
    else if (dictionary.hasKeyAndValue<double>(keyRadii)) {
        const double radius = dictionary.value<double>(keyRadii);
        _ellipsoid = Ellipsoid({ radius, radius, radius });
        setBoundingSphere(static_cast<float>(_ellipsoid.maximumRadius()));
    }

    if (dictionary.hasValue<bool>("PerformShading")) {
        _generalProperties.performShading = dictionary.value<bool>("PerformShading");
    }

    // Init layer manager
    ghoul::Dictionary layersDictionary;
    if (!dictionary.getValue(keyLayers, layersDictionary)) {
        throw ghoul::RuntimeError(std::string(keyLayers) + " must be specified");
    }

    _layerManager.initialize(layersDictionary);

    addProperty(_generalProperties.performShading);
    addProperty(_generalProperties.useAccurateNormals);
    addProperty(_generalProperties.eclipseShadowsEnabled);
    addProperty(_generalProperties.eclipseHardShadows);
    addProperty(_generalProperties.lodScaleFactor);
    addProperty(_generalProperties.cameraMinHeight);
    addProperty(_generalProperties.orenNayarRoughness);

    _debugPropertyOwner.addProperty(_debugProperties.saveOrThrowCamera);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkEdges);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkBounds);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkAABB);
    _debugPropertyOwner.addProperty(_debugProperties.showHeightResolution);
    _debugPropertyOwner.addProperty(_debugProperties.showHeightIntensities);
    _debugPropertyOwner.addProperty(_debugProperties.performFrustumCulling);
    _debugPropertyOwner.addProperty(_debugProperties.performHorizonCulling);
    _debugPropertyOwner.addProperty(
        _debugProperties.levelByProjectedAreaElseDistance
    );
    _debugPropertyOwner.addProperty(_debugProperties.resetTileProviders);
    _debugPropertyOwner.addProperty(_debugProperties.limitLevelByAvailableData);
    _debugPropertyOwner.addProperty(_debugProperties.modelSpaceRenderingCutoffLevel);

    auto notifyShaderRecompilation = [&](){
        _shadersNeedRecompilation = true;
    };
    _generalProperties.useAccurateNormals.onChange(notifyShaderRecompilation);
    _generalProperties.eclipseShadowsEnabled.onChange(notifyShaderRecompilation);
    _generalProperties.eclipseHardShadows.onChange(notifyShaderRecompilation);
    _generalProperties.performShading.onChange(notifyShaderRecompilation);
    _debugProperties.showChunkEdges.onChange(notifyShaderRecompilation);
    _debugProperties.showHeightResolution.onChange(notifyShaderRecompilation);
    _debugProperties.showHeightIntensities.onChange(notifyShaderRecompilation);

    _layerManager.onChange(notifyShaderRecompilation);

    addPropertySubOwner(_debugPropertyOwner);
    addPropertySubOwner(_layerManager);

    //================================================================
    //======== Reads Shadow (Eclipses) Entries in mod file ===========
    //================================================================
    ghoul::Dictionary shadowDictionary;
    bool success = dictionary.getValue(keyShadowGroup, shadowDictionary);
    bool disableShadows = false;
    if (success) {
        std::vector<std::pair<std::string, double>> sourceArray;
        unsigned int sourceCounter = 1;
        while (success) {
            std::string sourceName;
            success = shadowDictionary.getValue(keyShadowSource +
                std::to_string(sourceCounter) + ".Name", sourceName);
            if (success) {
                double sourceRadius;
                success = shadowDictionary.getValue(keyShadowSource +
                    std::to_string(sourceCounter) + ".Radius", sourceRadius);
                if (success) {
                    sourceArray.emplace_back(sourceName, sourceRadius);
                }
                else {
                    //LWARNING("No Radius value expecified for Shadow Source Name "
                    //    << sourceName << " from " << name
                    //    << " planet.\nDisabling shadows for this planet.");
                    disableShadows = true;
                    break;
                }
            }
            sourceCounter++;
        }

        if (!disableShadows && !sourceArray.empty()) {
            success = true;
            std::vector<std::pair<std::string, double>> casterArray;
            unsigned int casterCounter = 1;
            while (success) {
                std::string casterName;
                success = shadowDictionary.getValue(keyShadowCaster +
                    std::to_string(casterCounter) + ".Name", casterName);
                if (success) {
                    double casterRadius;
                    success = shadowDictionary.getValue(keyShadowCaster +
                        std::to_string(casterCounter) + ".Radius", casterRadius);
                    if (success) {
                        casterArray.emplace_back(casterName, casterRadius);
                    }
                    else {
                        //LWARNING("No Radius value expecified for Shadow Caster Name "
                        //    << casterName << " from " << name
                        //    << " planet.\nDisabling shadows for this planet.");
                        disableShadows = true;
                        break;
                    }
                }

                casterCounter++;
            }

            std::vector<Ellipsoid::ShadowConfiguration> shadowConfArray;
            if (!disableShadows && (!sourceArray.empty() && !casterArray.empty())) {
                for (const auto & source : sourceArray) {
                    for (const auto & caster : casterArray) {
                        Ellipsoid::ShadowConfiguration sc;
                        sc.source = source;
                        sc.caster = caster;
                        shadowConfArray.push_back(sc);
                    }
                }
                _ellipsoid.setShadowConfigurationArray(shadowConfArray);
            }
        }
    }
}

void RenderableGlobe::initializeGL() {
    _layerManager.update();

    // Recompile the shaders directly so that it is not done the first time the render
    // function is called.
    recompileShaders();
}

void RenderableGlobe::deinitialize() {
    _layerManager.deinitialize();
}

void RenderableGlobe::deinitializeGL() {
    if (_localProgramObject) {
        global::renderEngine.removeRenderProgram(_localProgramObject.get());
        _localProgramObject  = nullptr;
    }

    if (_globalProgramObject) {
        global::renderEngine.removeRenderProgram(_globalProgramObject.get());
        _globalProgramObject = nullptr;
    }
}

bool RenderableGlobe::isReady() const {
    return true;
}

void RenderableGlobe::render(const RenderData& data, RendererTasks& rendererTask) {
    if (_enabled) {
        if (_debugProperties.saveOrThrowCamera) {
            if (!_savedCamera) {
                // save camera
                _savedCamera = std::make_unique<Camera>(data.camera);
            }
            else { // throw camera
                _savedCamera = nullptr;
            }
         
            _debugProperties.saveOrThrowCamera = false;
        }

        const double distanceToCamera = distance(
            data.camera.positionVec3(),
            data.modelTransform.translation
        );

        // This distance will be enough to render the globe as one pixel if the field of
        // view is 'fov' radians and the screen resolution is 'res' pixels.
        //constexpr double fov = 2 * glm::pi<double>() / 6; // 60 degrees
        //constexpr double tfov = tan(fov / 2.0); // doesn't work unfortunately
        const double tfov = 0.5773502691896257;
        constexpr int res = 2880;
        const double distance = res * boundingSphere() / tfov;

        if (distanceToCamera < distance) {
            renderChunks(data, rendererTask);
        }
    }
    if (_savedCamera != nullptr) {
        DebugRenderer::ref().renderCameraFrustum(data, *_savedCamera);
    }
}

void RenderableGlobe::update(const UpdateData& data) {
    if (_shadersNeedRecompilation) {
        recompileShaders();
    }


    setBoundingSphere(static_cast<float>(
        _ellipsoid.maximumRadius() * data.modelTransform.scale
    ));

    glm::dmat4 translation =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation);
    glm::dmat4 rotation = glm::dmat4(data.modelTransform.rotation);
    glm::dmat4 scaling =
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale,
            data.modelTransform.scale, data.modelTransform.scale));

    _cachedModelTransform = translation * rotation * scaling;
    _cachedInverseModelTransform = glm::inverse(_cachedModelTransform);

    if (_debugProperties.resetTileProviders) {
        _layerManager.reset();
        _debugProperties.resetTileProviders = false;
    }
    _layerManager.update();
}

void RenderableGlobe::renderChunks(const RenderData& data, RendererTasks&) {
    _leftRoot->updateChunkTree(data);
    _rightRoot->updateChunkTree(data);

    // Calculate the MVP matrix
    const glm::dmat4 viewTransform = glm::dmat4(data.camera.combinedViewMatrix());
    const glm::dmat4 vp = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
        viewTransform;
    const glm::dmat4 mvp = vp * _cachedModelTransform;

    int count = 0;

    auto renderJob = [this, &data, &mvp, &count](const ChunkNode& chunkNode) {
        const Chunk& chunk = chunkNode.chunk();
        if (chunkNode.isLeaf() && chunk.isVisible()) {
            if (chunk.tileIndex().level < _debugProperties.modelSpaceRenderingCutoffLevel) {
                renderChunkGlobally(chunk, data);
            }
            else {
                renderChunkLocally(chunk, data);
            }

            ++count;

            debugRenderChunk(chunk, mvp);
        }
    };

    _leftRoot->breadthFirst(renderJob);
    _rightRoot->breadthFirst(renderJob);

    LINFOC(identifier(), std::to_string(count));
}

void RenderableGlobe::renderChunkGlobally(const Chunk& chunk, const RenderData& data) {
    const TileIndex& tileIndex = chunk.tileIndex();
    ghoul::opengl::ProgramObject& program = *_globalProgramObject;

    if (_globalProgramObjectUpdatedSinceLastCall) {
        _globalGpuLayerManager.bind(&program, _layerManager);
        _globalProgramObjectUpdatedSinceLastCall = false;
    }

    // Activate the shader program
    program.activate();

    _globalGpuLayerManager.setValue(&program, _layerManager, tileIndex);

    // The length of the skirts is proportional to its size
    program.setUniform(
        "skirtLength",
        static_cast<float>(
            glm::min(
                chunk.surfacePatch().halfSize().lat * 1000000,
                _ellipsoid.minimumRadius()
            )
            )
    );

    program.setUniform("xSegments", _grid.xSegments());

    if (_debugProperties.showHeightResolution) {
        program.setUniform(
            "vertexResolution",
            glm::vec2(_grid.xSegments(), _grid.ySegments())
        );
    }


    if (_layerManager.hasAnyBlendingLayersEnabled()) {
        // Calculations are done in the reference frame of the globe. Hence, the
        // camera position needs to be transformed with the inverse model matrix
        const glm::dvec3 cameraPosition = glm::dvec3(
            _cachedInverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1.0)
        );
        const float distanceScaleFactor = static_cast<float>(
            _generalProperties.lodScaleFactor * _ellipsoid.minimumRadius()
        );
        program.setUniform("cameraPosition", glm::vec3(cameraPosition));
        program.setUniform("distanceScaleFactor", distanceScaleFactor);
        program.setUniform("chunkLevel", chunk.tileIndex().level);
    }

    // Calculate other uniform variables needed for rendering
    const Geodetic2 swCorner = chunk.surfacePatch().corner(Quad::SOUTH_WEST);
    const Geodetic2& patchSize = chunk.surfacePatch().size();

    const glm::dmat4& viewTransform = data.camera.combinedViewMatrix();
    const glm::mat4 modelViewTransform = glm::mat4(viewTransform * _cachedModelTransform);
    const glm::mat4 modelViewProjectionTransform =
        data.camera.sgctInternal.projectionMatrix() * modelViewTransform;

    // Upload the uniform variables
    program.setUniform(
        "modelViewProjectionTransform",
        modelViewProjectionTransform
    );
    program.setUniform("minLatLon", glm::vec2(swCorner.toLonLatVec2()));
    program.setUniform("lonLatScalingFactor", glm::vec2(patchSize.toLonLatVec2()));
    // Ellipsoid Radius (Model Space)
    program.setUniform("radiiSquared", glm::vec3(_ellipsoid.radiiSquared()));

    const bool hasNightLayers = !_layerManager.layerGroup(
        layergroupid::GroupID::NightLayers
    ).activeLayers().empty();

    const bool hasWaterLayer = !_layerManager.layerGroup(
        layergroupid::GroupID::WaterMasks
    ).activeLayers().empty();

    if (hasNightLayers || hasWaterLayer || _generalProperties.performShading) {
        program.setUniform("modelViewTransform", modelViewTransform);
    }

    const bool hasHeightLayer = !_layerManager.layerGroup(
        layergroupid::HeightLayers
    ).activeLayers().empty();
    if (_generalProperties.useAccurateNormals && hasHeightLayer) {
        // Apply an extra scaling to the height if the object is scaled
        program.setUniform(
            "heightScale",
            static_cast<float>(data.modelTransform.scale * data.camera.scaling())
        );
    }

    setCommonUniforms(program, chunk, data);

    if (_generalProperties.eclipseShadowsEnabled && _ellipsoid.hasEclipseShadows()) {
        calculateEclipseShadows(chunk, &program, data);
    }

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    _grid.geometry().drawUsingActiveProgram();
    _globalGpuLayerManager.deactivate();
    program.deactivate();
}

void RenderableGlobe::renderChunkLocally(const Chunk& chunk, const RenderData& data) {
    const TileIndex& tileIndex = chunk.tileIndex();
    ghoul::opengl::ProgramObject& program = *_localProgramObject;

    if (_localProgramObjectUpdatedSinceLastCall) {
        _localGpuLayerManager.bind(&program, _layerManager);
        _localProgramObjectUpdatedSinceLastCall = false;
    }

    // Activate the shader program
    program.activate();

    _localGpuLayerManager.setValue(&program, _layerManager, tileIndex);

    // The length of the skirts is proportional to its size
    program.setUniform(
        "skirtLength",
        static_cast<float>(
            glm::min(
                chunk.surfacePatch().halfSize().lat * 1000000,
                _ellipsoid.minimumRadius()
            )
        )
    );

    program.setUniform("xSegments", _grid.xSegments());

    if (_debugProperties.showHeightResolution) {
        program.setUniform(
            "vertexResolution",
            glm::vec2(_grid.xSegments(), _grid.ySegments())
        );
    }

    if (_layerManager.hasAnyBlendingLayersEnabled()) {
        float distanceScaleFactor = static_cast<float>(
            _generalProperties.lodScaleFactor * _ellipsoid.minimumRadius()
        );

        program.setUniform("distanceScaleFactor", distanceScaleFactor);
        program.setUniform("chunkLevel", chunk.tileIndex().level);
    }

    // Calculate other uniform variables needed for rendering
    // Send the matrix inverse to the fragment for the global and local shader (JCC)
    const glm::dmat4 viewTransform = data.camera.combinedViewMatrix();
    const glm::dmat4 modelViewTransform = viewTransform * _cachedModelTransform;


    std::array<glm::dvec3, 4> cornersCameraSpace;
    std::array<glm::dvec3, 4> cornersModelSpace;
    for (int i = 0; i < 4; ++i) {
        constexpr const std::array<const char*, 4> CornerNames = {
            "p01", "p11", "p00", "p10"
        };

        const Quad q = static_cast<Quad>(i);
        const Geodetic2 corner = chunk.surfacePatch().corner(q);
        const glm::dvec3 cornerModelSpace = _ellipsoid.cartesianSurfacePosition(corner);
        cornersModelSpace[i] = cornerModelSpace;
        const glm::dvec3 cornerCameraSpace = glm::dvec3(
            modelViewTransform * glm::dvec4(cornerModelSpace, 1)
        );
        cornersCameraSpace[i] = cornerCameraSpace;
        program.setUniform(CornerNames[i], glm::vec3(cornerCameraSpace));

    }

    // TODO: Patch normal can be calculated for all corners and then linearly
    // interpolated on the GPU to avoid cracks for high altitudes.
    const glm::vec3 patchNormalCameraSpace = normalize(
        cross(
            cornersCameraSpace[Quad::SOUTH_EAST] - cornersCameraSpace[Quad::SOUTH_WEST],
            cornersCameraSpace[Quad::NORTH_EAST] - cornersCameraSpace[Quad::SOUTH_WEST]
        )
    );

    // In order to improve performance, lets use the normal in object space (model space)
    // for deferred rendering.
    const glm::vec3 patchNormalModelSpace = normalize(
        cross(
            cornersModelSpace[Quad::SOUTH_EAST] - cornersModelSpace[Quad::SOUTH_WEST],
            cornersModelSpace[Quad::NORTH_EAST] - cornersModelSpace[Quad::SOUTH_WEST]
        )
    );

    program.setUniform("patchNormalModelSpace", patchNormalModelSpace);
    program.setUniform("patchNormalCameraSpace", patchNormalCameraSpace);
    program.setUniform(
        "projectionTransform",
        data.camera.sgctInternal.projectionMatrix()
    );

    if (!_layerManager.layerGroup(layergroupid::HeightLayers).activeLayers().empty()) {
        // Apply an extra scaling to the height if the object is scaled
        program.setUniform(
            "heightScale",
            static_cast<float>(data.modelTransform.scale * data.camera.scaling())
        );
    }

    setCommonUniforms(program, chunk, data);

    if (_generalProperties.eclipseShadowsEnabled && _ellipsoid.hasEclipseShadows()) {
        calculateEclipseShadows(chunk, &program, data);
    }

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    _grid.geometry().drawUsingActiveProgram();

    _localGpuLayerManager.deactivate();
    program.deactivate();
}
//
//ghoul::opengl::ProgramObject* RenderableGlobe::getActivatedProgramWithTileData(
//                                              ghoul::opengl::ProgramObject& programObject,
//                                                               bool& updatedSinceLastCall,
//                                                         GPULayerManager& gpuLayerManager,
//                                                                       const Chunk& chunk)
//{
//
// }







void RenderableGlobe::recompileShaders() {
    struct LayerShaderPreprocessingData {
        struct LayerGroupPreprocessingData {
            int lastLayerIdx;
            bool layerBlendingEnabled;
            std::vector<layergroupid::TypeID> layerType;
            std::vector<layergroupid::BlendModeID> blendMode;
            std::vector<layergroupid::AdjustmentTypeID> layerAdjustmentType;
        };

        std::array<LayerGroupPreprocessingData, layergroupid::NUM_LAYER_GROUPS>
            layeredTextureInfo;
        std::vector<std::pair<std::string, std::string>> keyValuePairs;
    };

    //LayerShaderManager::LayerShaderPreprocessingData preprocessingData =
//    LayerShaderManager::LayerShaderPreprocessingData::get(*this);
//_globalLayerShaderManager.recompileShaderProgram(preprocessingData);
//_localLayerShaderManager.recompileShaderProgram(preprocessingData);
/*
        , _globalLayerShaderManager(
            "GlobalChunkedLodPatch",
            "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_fs.glsl"
        )
            , _localLayerShaderManager(
                "LocalChunkedLodPatch",
                "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_vs.glsl",
                "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_fs.glsl"
            )*/


    //
    // Create LayerShaderPreprocessingData
    //

    LayerShaderPreprocessingData preprocessingData;

    for (size_t i = 0; i < layergroupid::NUM_LAYER_GROUPS; i++) {
        LayerShaderPreprocessingData::LayerGroupPreprocessingData layeredTextureInfo;

        const LayerGroup& layerGroup = _layerManager.layerGroup(i);
        const std::vector<std::shared_ptr<Layer>>& layers = layerGroup.activeLayers();

        // This check was implicit before;  not sure if it will fire or will be handled
        // elsewhere
        //ghoul_assert(
        //    !layerGroup.activeLayers().empty(),
        //    "If activeLayers is empty the following line will lead to an overflow"
        //);
        layeredTextureInfo.lastLayerIdx = static_cast<int>(
            layerGroup.activeLayers().size() - 1
            );
        layeredTextureInfo.layerBlendingEnabled = layerGroup.layerBlendingEnabled();

        for (const std::shared_ptr<Layer>& layer : layers) {
            layeredTextureInfo.layerType.push_back(layer->type());
            layeredTextureInfo.blendMode.push_back(layer->blendMode());
            layeredTextureInfo.layerAdjustmentType.push_back(
                layer->layerAdjustment().type()
            );
        }

        preprocessingData.layeredTextureInfo[i] = layeredTextureInfo;
    }

    std::vector<std::pair<std::string, std::string>>& pairs =
        preprocessingData.keyValuePairs;

    pairs.emplace_back("useAccurateNormals",
        std::to_string(_generalProperties.useAccurateNormals)
    );
    pairs.emplace_back("useAtmosphere", std::to_string(false));
    pairs.emplace_back("performShading", std::to_string(_generalProperties.performShading));
    pairs.emplace_back(
        "useEclipseShadows",
        std::to_string(_generalProperties.eclipseShadowsEnabled)
    );
    pairs.emplace_back(
        "useEclipseHardShadows",
        std::to_string(_generalProperties.eclipseHardShadows)
    );
    pairs.emplace_back("showChunkEdges", std::to_string(_debugProperties.showChunkEdges));
    pairs.emplace_back("showHeightResolution",
        std::to_string(_debugProperties.showHeightResolution)
    );
    pairs.emplace_back("showHeightIntensities",
        std::to_string(_debugProperties.showHeightIntensities)
    );
    pairs.emplace_back("defaultHeight", std::to_string(Chunk::DefaultHeight));


    //
    // Create dictionary from layerpreprocessing data
    //

    ghoul::Dictionary shaderDictionary;

    // Different layer types can be height layers or color layers for example.
    // These are used differently within the shaders.
    preprocessingData.layeredTextureInfo;

    for (size_t i = 0; i < preprocessingData.layeredTextureInfo.size(); i++) {
        // lastLayerIndex must be at least 0 for the shader to compile,
        // the layer type is inactivated by setting use to false
        const std::string& groupName = layergroupid::LAYER_GROUP_IDENTIFIERS[i];
        shaderDictionary.setValue(
            "lastLayerIndex" + groupName,
            glm::max(preprocessingData.layeredTextureInfo[i].lastLayerIdx, 0)
        );
        shaderDictionary.setValue(
            "use" + groupName,
            preprocessingData.layeredTextureInfo[i].lastLayerIdx >= 0
        );
        shaderDictionary.setValue(
            "blend" + groupName,
            preprocessingData.layeredTextureInfo[i].layerBlendingEnabled
        );

        // This is to avoid errors from shader preprocessor
        shaderDictionary.setValue(groupName + "0" + "LayerType", 0);

        for (int j = 0; j < preprocessingData.layeredTextureInfo[i].lastLayerIdx + 1; ++j) {
            shaderDictionary.setValue(
                groupName + std::to_string(j) + "LayerType",
                static_cast<int>(preprocessingData.layeredTextureInfo[i].layerType[j])
            );
        }

        // This is to avoid errors from shader preprocessor
        shaderDictionary.setValue(groupName + "0" + "BlendMode", 0);

        for (int j = 0; j < preprocessingData.layeredTextureInfo[i].lastLayerIdx + 1; ++j) {
            shaderDictionary.setValue(
                groupName + std::to_string(j) + "BlendMode",
                static_cast<int>(preprocessingData.layeredTextureInfo[i].blendMode[j])
            );
        }

        // This is to avoid errors from shader preprocessor
        std::string keyLayerAdjustmentType = groupName + "0" + "LayerAdjustmentType";
        shaderDictionary.setValue(keyLayerAdjustmentType, 0);

        for (int j = 0; j < preprocessingData.layeredTextureInfo[i].lastLayerIdx + 1; ++j) {
            shaderDictionary.setValue(
                groupName + std::to_string(j) + "LayerAdjustmentType",
                static_cast<int>(preprocessingData.layeredTextureInfo[i].layerAdjustmentType[j])
            );
        }
    }

    ghoul::Dictionary layerGroupNames;
    for (int i = 0; i < layergroupid::NUM_LAYER_GROUPS; ++i) {
        layerGroupNames.setValue(
            std::to_string(i),
            layergroupid::LAYER_GROUP_IDENTIFIERS[i]
        );
    }
    shaderDictionary.setValue("layerGroups", layerGroupNames);

    // Other settings such as "useAtmosphere"
    for (const std::pair<std::string, std::string>& p : preprocessingData.keyValuePairs)
    {
        shaderDictionary.setValue(p.first, p.second);
    }

    //
    // Create local shader
    //
    global::renderEngine.removeRenderProgram(_localProgramObject.get());
    _localProgramObject = global::renderEngine.buildRenderProgram(
        "LocalChunkedLodPatch",
        "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_vs.glsl",
        "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_fs.glsl",
        shaderDictionary
    );
    ghoul_assert(_localProgramObject != nullptr, "Failed to initialize programObject!");
    _localProgramObjectUpdatedSinceLastCall = true;


    //
    // Create global shader
    //
    global::renderEngine.removeRenderProgram(_globalProgramObject.get());
    _globalProgramObject = global::renderEngine.buildRenderProgram(
        "GlobalChunkedLodPatch",
        "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_vs.glsl",
        "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_fs.glsl",
        shaderDictionary
    );
    ghoul_assert(_globalProgramObject != nullptr, "Failed to initialize programObject!");
    _globalProgramObjectUpdatedSinceLastCall = true;

    _shadersNeedRecompilation = false;
}

SurfacePositionHandle RenderableGlobe::calculateSurfacePositionHandle(
                                                 const glm::dvec3& targetModelSpace) const
{
    glm::dvec3 centerToEllipsoidSurface =
        _ellipsoid.geodeticSurfaceProjection(targetModelSpace);
    glm::dvec3 ellipsoidSurfaceToTarget = targetModelSpace - centerToEllipsoidSurface;
    // ellipsoidSurfaceOutDirection will point towards the target, we want the outward
    // direction. Therefore it must be flipped in case the target is under the reference
    // ellipsoid so that it always points outwards
    glm::dvec3 ellipsoidSurfaceOutDirection = glm::normalize(ellipsoidSurfaceToTarget);
    if (glm::dot(ellipsoidSurfaceOutDirection, centerToEllipsoidSurface) < 0) {
        ellipsoidSurfaceOutDirection *= -1.0;
    }

    double heightToSurface = getHeight(targetModelSpace);
    heightToSurface = glm::isnan(heightToSurface) ? 0.0 : heightToSurface;
    centerToEllipsoidSurface = glm::isnan(glm::length(centerToEllipsoidSurface)) ?
        (glm::dvec3(0.0, 1.0, 0.0) * static_cast<double>(boundingSphere())) :
        centerToEllipsoidSurface;
    ellipsoidSurfaceOutDirection = glm::isnan(glm::length(ellipsoidSurfaceOutDirection)) ?
        glm::dvec3(0.0, 1.0, 0.0) : ellipsoidSurfaceOutDirection;

    return {
        centerToEllipsoidSurface,
        ellipsoidSurfaceOutDirection,
        heightToSurface
    };
}



bool RenderableGlobe::testIfCullable(const Chunk& chunk, 
                                     const RenderData& renderData) const
{
    const bool phc = _debugProperties.performHorizonCulling;
    const bool pfc = _debugProperties.performFrustumCulling;

    return (phc && isCullableByHorizon(chunk, renderData)) ||
           (pfc && isCullableByFrustum(chunk, renderData));
}

const ChunkNode& RenderableGlobe::findChunkNode(const Geodetic2& location) const {
    ghoul_assert(
        Coverage.contains(location),
        "Point must be in lat [-90, 90] and lon [-180, 180]"
    );

    return location.lon < Coverage.center().lon ?
        _leftRoot->find(location) :
        _rightRoot->find(location);
}

int RenderableGlobe::desiredLevel(const Chunk& chunk, const RenderData& renderData) const
{
    const int desiredLevel = _debugProperties.levelByProjectedAreaElseDistance ?
        desiredLevelByProjectedArea(chunk, renderData) :
        desiredLevelByDistance(chunk, renderData);
    const int levelByAvailableData = desiredLevelByAvailableTileData(
        chunk,
        renderData
    );

    const bool llbad = _debugProperties.limitLevelByAvailableData;
    if (levelByAvailableData != UnknownDesiredLevel && llbad) {
        const int l = glm::min(desiredLevel, levelByAvailableData);
        return glm::clamp(l, MinSplitDepth, MaxSplitDepth);
    }
    else {
        return glm::clamp(desiredLevel, MinSplitDepth, MaxSplitDepth);
    }
}

float RenderableGlobe::getHeight(const glm::dvec3& position) const {
    float height = 0;

    // Get the uv coordinates to sample from
    const Geodetic2 geodeticPosition = _ellipsoid.cartesianToGeodetic2(position);
    const int chunkLevel = findChunkNode(geodeticPosition).chunk().tileIndex().level;

    const TileIndex tileIndex = TileIndex(geodeticPosition, chunkLevel);
    const GeodeticPatch patch = GeodeticPatch(tileIndex);

    const Geodetic2 geoDiffPatch = patch.corner(Quad::NORTH_EAST) -
        patch.corner(Quad::SOUTH_WEST);

    const Geodetic2 geoDiffPoint = geodeticPosition - patch.corner(Quad::SOUTH_WEST);
    const glm::vec2 patchUV = glm::vec2(
        geoDiffPoint.lon / geoDiffPatch.lon,
        geoDiffPoint.lat / geoDiffPatch.lat
    );

    // Get the tile providers for the height maps
    const std::vector<std::shared_ptr<Layer>>& heightMapLayers =
        _layerManager.layerGroup(layergroupid::GroupID::HeightLayers).activeLayers();

    for (const std::shared_ptr<Layer>& layer : heightMapLayers) {
        tileprovider::TileProvider* tileProvider = layer->tileProvider();
        if (!tileProvider) {
            continue;
        }
        // Transform the uv coordinates to the current tile texture
        const ChunkTile chunkTile = tileProvider->chunkTile(tileIndex);
        const Tile& tile = chunkTile.tile;
        const TileUvTransform& uvTransform = chunkTile.uvTransform;
        const TileDepthTransform& depthTransform = tileProvider->depthTransform();
        if (tile.status() != Tile::Status::OK) {
            return 0;
        }

        ghoul::opengl::Texture* tileTexture = tile.texture();
        if (!tileTexture) {
            return 0;
        }

        glm::vec2 transformedUv = layer->TileUvToTextureSamplePosition(
            uvTransform,
            patchUV,
            glm::uvec2(tileTexture->dimensions())
        );

        // Sample and do linear interpolation
        // (could possibly be moved as a function in ghoul texture)
        // Suggestion: a function in ghoul::opengl::Texture that takes uv coordinates
        // in range [0,1] and uses the set interpolation method and clamping.

        const glm::uvec3 dimensions = tileTexture->dimensions();

        const glm::vec2 samplePos = transformedUv * glm::vec2(dimensions);
        glm::uvec2 samplePos00 = samplePos;
        samplePos00 = glm::clamp(
            samplePos00,
            glm::uvec2(0, 0),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::vec2 samplePosFract = samplePos - glm::vec2(samplePos00);

        const glm::uvec2 samplePos10 = glm::min(
            samplePos00 + glm::uvec2(1, 0),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::uvec2 samplePos01 = glm::min(
            samplePos00 + glm::uvec2(0, 1),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::uvec2 samplePos11 = glm::min(
            samplePos00 + glm::uvec2(1, 1),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );

        const float sample00 = tileTexture->texelAsFloat(samplePos00).x;
        const float sample10 = tileTexture->texelAsFloat(samplePos10).x;
        const float sample01 = tileTexture->texelAsFloat(samplePos01).x;
        const float sample11 = tileTexture->texelAsFloat(samplePos11).x;

        // In case the texture has NaN or no data values don't use this height map.
        const bool anySampleIsNaN =
            std::isnan(sample00) ||
            std::isnan(sample01) ||
            std::isnan(sample10) ||
            std::isnan(sample11);

        const bool anySampleIsNoData =
            sample00 == tileProvider->noDataValueAsFloat() ||
            sample01 == tileProvider->noDataValueAsFloat() ||
            sample10 == tileProvider->noDataValueAsFloat() ||
            sample11 == tileProvider->noDataValueAsFloat();

        if (anySampleIsNaN || anySampleIsNoData) {
            continue;
        }

        const float sample0 = sample00 * (1.f - samplePosFract.x) +
            sample10 * samplePosFract.x;
        const float sample1 = sample01 * (1.f - samplePosFract.x) +
            sample11 * samplePosFract.x;

        const float sample = sample0 * (1.f - samplePosFract.y) +
            sample1 * samplePosFract.y;

        // Same as is used in the shader. This is not a perfect solution but
        // if the sample is actually a no-data-value (min_float) the interpolated
        // value might not be. Therefore we have a cut-off. Assuming no data value
        // is smaller than -100000
        if (sample > -100000) {
            // Perform depth transform to get the value in meters
            height = depthTransform.depthOffset + depthTransform.depthScale * sample;
            // Make sure that the height value follows the layer settings.
            // For example if the multiplier is set to a value bigger than one,
            // the sampled height should be modified as well.
            height = layer->renderSettings().performLayerSettings(height);
        }
    }
    // Return the result
    return height;
}

void RenderableGlobe::debugRenderChunk(const Chunk& chunk, const glm::dmat4& mvp) const {
    if (_debugProperties.showChunkBounds || _debugProperties.showChunkAABB) {
        const std::vector<glm::dvec4>& modelSpaceCorners =
            chunk.boundingPolyhedronCorners();

        std::vector<glm::vec4> clippingSpaceCorners(8);
        AABB3 screenSpaceBounds;

        for (size_t i = 0; i < 8; ++i) {
            const glm::vec4& clippingSpaceCorner = mvp * modelSpaceCorners[i];
            clippingSpaceCorners[i] = clippingSpaceCorner;

            glm::vec3 screenSpaceCorner =
                glm::vec3((1.f / clippingSpaceCorner.w) * clippingSpaceCorner);
            screenSpaceBounds.expand(std::move(screenSpaceCorner));
        }

        const unsigned int colorBits = 1 + chunk.tileIndex().level % 6;
        const glm::vec4 color = glm::vec4(
            colorBits & 1,
            colorBits & 2,
            colorBits & 4,
            0.3f
        );

        if (_debugProperties.showChunkBounds) {
            DebugRenderer::ref().renderNiceBox(clippingSpaceCorners, color);
        }

        if (_debugProperties.showChunkAABB) {
            const std::vector<glm::vec4>& screenSpacePoints =
                DebugRenderer::ref().verticesFor(screenSpaceBounds);
            DebugRenderer::ref().renderNiceBox(screenSpacePoints, color);
        }
    }
}

















void RenderableGlobe::calculateEclipseShadows(const Chunk& chunk,
    ghoul::opengl::ProgramObject* programObject,
    const RenderData& data)
{
    // Shadow calculations..
    if (_ellipsoid.hasEclipseShadows()) {
        std::vector<RenderableGlobe::ShadowRenderingStruct> shadowDataArray;
        std::vector<Ellipsoid::ShadowConfiguration> shadowConfArray =
            _ellipsoid.shadowConfigurationArray();
        shadowDataArray.reserve(shadowConfArray.size());
        double lt;
        for (const auto & shadowConf : shadowConfArray) {
            // TO REMEMBER: all distances and lengths in world coordinates are in
            // meters!!! We need to move this to view space...
            // Getting source and caster:
            glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(
                shadowConf.source.first,
                "SUN",
                "GALACTIC",
                {},
                data.time.j2000Seconds(),
                lt
            );
            sourcePos *= KM_TO_M; // converting to meters
            glm::dvec3 casterPos = SpiceManager::ref().targetPosition(
                shadowConf.caster.first,
                "SUN",
                "GALACTIC",
                {},
                data.time.j2000Seconds(),
                lt
            );
            casterPos *= KM_TO_M; // converting to meters
            // psc caster_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(
            //     casterPos.x,
            //     casterPos.y,
            //     casterPos.z
            // );


            // First we determine if the caster is shadowing the current planet (all
            // calculations in World Coordinates):
            const glm::dvec3 planetCasterVec = casterPos - data.position.dvec3();
            const glm::dvec3 sourceCasterVec = casterPos - sourcePos;
            const double sc_length = glm::length(sourceCasterVec);
            const glm::dvec3 planetCaster_proj =
                (glm::dot(planetCasterVec, sourceCasterVec) / (sc_length*sc_length)) *
                sourceCasterVec;
            const double d_test = glm::length(planetCasterVec - planetCaster_proj);
            const double xp_test = shadowConf.caster.second * sc_length /
                (shadowConf.source.second + shadowConf.caster.second);
            const double rp_test = shadowConf.caster.second *
                (glm::length(planetCaster_proj) + xp_test) / xp_test;

            const glm::dvec3 sunPos = SpiceManager::ref().targetPosition(
                "SUN",
                "SUN",
                "GALACTIC",
                {},
                data.time.j2000Seconds(),
                lt
            );
            const double casterDistSun = glm::length(casterPos - sunPos);
            const double planetDistSun = glm::length(data.position.dvec3() - sunPos);

            RenderableGlobe::ShadowRenderingStruct shadowData;
            shadowData.isShadowing = false;

            // Eclipse shadows considers planets and moons as spheres
            if (((d_test - rp_test) < (_ellipsoid.radii().x * KM_TO_M)) &&
                (casterDistSun < planetDistSun))
            {
                // The current caster is shadowing the current planet
                shadowData.isShadowing = true;
                shadowData.rs = shadowConf.source.second;
                shadowData.rc = shadowConf.caster.second;
                shadowData.sourceCasterVec = glm::normalize(sourceCasterVec);
                shadowData.xp = xp_test;
                shadowData.xu = shadowData.rc * sc_length /
                    (shadowData.rs - shadowData.rc);
                shadowData.casterPositionVec = casterPos;
            }
            shadowDataArray.push_back(shadowData);
        }

        const std::string uniformVarName("shadowDataArray[");
        unsigned int counter = 0;
        for (const RenderableGlobe::ShadowRenderingStruct& sd : shadowDataArray) {
            constexpr const char* NameIsShadowing = "shadowDataArray[{}].isShadowing";
            constexpr const char* NameXp = "shadowDataArray[{}].xp";
            constexpr const char* NameXu = "shadowDataArray[{}].xu";
            constexpr const char* NameRc = "shadowDataArray[{}].rc";
            constexpr const char* NameSource = "shadowDataArray[{}].sourceCasterVec";
            constexpr const char* NamePos = "shadowDataArray[{}].casterPositionVec";

            programObject->setUniform(
                fmt::format(NameIsShadowing, counter), sd.isShadowing
            );
            if (sd.isShadowing) {
                programObject->setUniform(fmt::format(NameXp, counter), sd.xp);
                programObject->setUniform(fmt::format(NameXu, counter), sd.xu);
                programObject->setUniform(fmt::format(NameRc, counter), sd.rc);
                programObject->setUniform(
                    fmt::format(NameSource, counter), sd.sourceCasterVec
                );
                programObject->setUniform(
                    fmt::format(NamePos, counter), sd.casterPositionVec
                );
            }
            counter++;
        }

        programObject->setUniform(
            "inverseViewTransform",
            glm::inverse(data.camera.combinedViewMatrix())
        );
        programObject->setUniform("modelTransform", _cachedModelTransform);
        programObject->setUniform(
            "hardShadows",
            _generalProperties.eclipseHardShadows
        );
        programObject->setUniform("calculateEclipseShadows", true);
    }
}

void RenderableGlobe::setCommonUniforms(ghoul::opengl::ProgramObject& programObject,
    const Chunk& chunk, const RenderData& data)
{
    const glm::dmat4 viewTransform = data.camera.combinedViewMatrix();
    const glm::dmat4 modelViewTransform = viewTransform * _cachedModelTransform;

    const bool nightLayersActive =
        !_layerManager.layerGroup(layergroupid::NightLayers).activeLayers().empty();
    const bool waterLayersActive =
        !_layerManager.layerGroup(layergroupid::WaterMasks).activeLayers().empty();

    if (nightLayersActive || waterLayersActive || _generalProperties.performShading) {
        const glm::dvec3 directionToSunWorldSpace =
            length(data.modelTransform.translation) > 0.0 ?
            glm::normalize(-data.modelTransform.translation) :
            glm::dvec3(0.0);

        const glm::vec3 directionToSunCameraSpace = glm::vec3(viewTransform *
            glm::dvec4(directionToSunWorldSpace, 0));
        programObject.setUniform(
            "lightDirectionCameraSpace",
            -glm::normalize(directionToSunCameraSpace)
        );
    }

    if (_generalProperties.performShading) {
        programObject.setUniform(
            "orenNayarRoughness",
            _generalProperties.orenNayarRoughness);
    }

    if (_generalProperties.useAccurateNormals &&
        !_layerManager.layerGroup(layergroupid::HeightLayers).activeLayers().empty())
    {
        const glm::dvec3 corner00 = _ellipsoid.cartesianSurfacePosition(
            chunk.surfacePatch().corner(Quad::SOUTH_WEST)
        );
        const glm::dvec3 corner10 = _ellipsoid.cartesianSurfacePosition(
            chunk.surfacePatch().corner(Quad::SOUTH_EAST)
        );
        const glm::dvec3 corner01 = _ellipsoid.cartesianSurfacePosition(
            chunk.surfacePatch().corner(Quad::NORTH_WEST)
        );
        const glm::dvec3 corner11 = _ellipsoid.cartesianSurfacePosition(
            chunk.surfacePatch().corner(Quad::NORTH_EAST)
        );

        // This is an assumption that the height tile has a resolution of 64 * 64
        // If it does not it will still produce "correct" normals. If the resolution is
        // higher the shadows will be softer, if it is lower, pixels will be visible.
        // Since default is 64 this will most likely work fine.

        const glm::mat3& modelViewTransformMat3 = glm::mat3(modelViewTransform);

        constexpr const float TileDelta = 1.f / 64.f;
        const glm::vec3 deltaTheta0 = modelViewTransformMat3 *
            (glm::vec3(corner10 - corner00) * TileDelta);
        const glm::vec3 deltaTheta1 = modelViewTransformMat3 *
            (glm::vec3(corner11 - corner01) * TileDelta);
        const glm::vec3 deltaPhi0 = modelViewTransformMat3 *
            (glm::vec3(corner01 - corner00) * TileDelta);
        const glm::vec3 deltaPhi1 = modelViewTransformMat3 *
            (glm::vec3(corner11 - corner10) * TileDelta);

        // Upload uniforms
        programObject.setUniform("deltaTheta0", glm::length(deltaTheta0));
        programObject.setUniform("deltaTheta1", glm::length(deltaTheta1));
        programObject.setUniform("deltaPhi0", glm::length(deltaPhi0));
        programObject.setUniform("deltaPhi1", glm::length(deltaPhi1));
        programObject.setUniform("tileDelta", TileDelta);
        // This should not be needed once the light calculations for the atmosphere
        // is performed in view space..
        programObject.setUniform(
            "invViewModelTransform",
            glm::inverse(
                glm::mat4(data.camera.combinedViewMatrix()) *
                glm::mat4(_cachedModelTransform)
            )
        );
    }
}

int RenderableGlobe::desiredLevelByDistance(const Chunk& chunk, const RenderData& data) const {
    // Calculations are done in the reference frame of the globe
    // (model space). Hence, the camera position needs to be transformed
    // with the inverse model matrix
    const glm::dvec3 cameraPosition = glm::dvec3(_cachedInverseModelTransform *
        glm::dvec4(data.camera.positionVec3(), 1.0));

    const Geodetic2 pointOnPatch = chunk.surfacePatch().closestPoint(
        _ellipsoid.cartesianToGeodetic2(cameraPosition)
    );
    const glm::dvec3 patchNormal = _ellipsoid.geodeticSurfaceNormal(pointOnPatch);
    glm::dvec3 patchPosition = _ellipsoid.cartesianSurfacePosition(pointOnPatch);

    const Chunk::BoundingHeights heights = chunk.boundingHeights();
    const double heightToChunk = heights.min;

    // Offset position according to height
    patchPosition += patchNormal * heightToChunk;

    const glm::dvec3 cameraToChunk = patchPosition - cameraPosition;

    // Calculate desired level based on distance
    const double distanceToPatch = glm::length(cameraToChunk);
    const double distance = distanceToPatch;

    const double scaleFactor = _generalProperties.lodScaleFactor *
        _ellipsoid.minimumRadius();
    const double projectedScaleFactor = scaleFactor / distance;
    const int desiredLevel = static_cast<int>(ceil(log2(projectedScaleFactor)));
    return desiredLevel;
}

int RenderableGlobe::desiredLevelByProjectedArea(const Chunk& chunk, const RenderData& data) const {
    // Calculations are done in the reference frame of the globe
    // (model space). Hence, the camera position needs to be transformed
    // with the inverse model matrix
    const glm::dvec4 cameraPositionModelSpace = glm::dvec4(data.camera.positionVec3(), 1);
    const glm::dvec3 cameraPosition = glm::dvec3(
        _cachedInverseModelTransform * cameraPositionModelSpace
    );
    const glm::dvec3 cameraToEllipsoidCenter = -cameraPosition;

    const Geodetic2 cameraGeodeticPos = _ellipsoid.cartesianToGeodetic2(cameraPosition);

    // Approach:
    // The projected area of the chunk will be calculated based on a small area that
    // is close to the camera, and the scaled up to represent the full area.
    // The advantage of doing this is that it will better handle the cases where the
    // full patch is very curved (e.g. stretches from latitude 0 to 90 deg).

    const Geodetic2 center = chunk.surfacePatch().center();
    const Geodetic2 closestCorner = chunk.surfacePatch().closestCorner(cameraGeodeticPos);

    //  Camera
    //  |
    //  V
    //
    //  oo
    // [  ]<
    //                     *geodetic space*
    //
    //   closestCorner
    //    +-----------------+  <-- north east corner
    //    |                 |
    //    |      center     |
    //    |                 |
    //    +-----------------+  <-- south east corner

    const Chunk::BoundingHeights heights = chunk.boundingHeights();
    const Geodetic3 c = { center, heights.min };
    const Geodetic3 c1 = { Geodetic2(center.lat, closestCorner.lon), heights.min };
    const Geodetic3 c2 = { Geodetic2(closestCorner.lat, center.lon), heights.min };

    //  Camera
    //  |
    //  V
    //
    //  oo
    // [  ]<
    //                     *geodetic space*
    //
    //    +--------c2-------+  <-- north east corner
    //    |                 |
    //    c1       c        |
    //    |                 |
    //    +-----------------+  <-- south east corner


    // Go from geodetic to cartesian space and project onto unit sphere
    const glm::dvec3 A = glm::normalize(
        cameraToEllipsoidCenter + _ellipsoid.cartesianPosition(c)
    );
    const glm::dvec3 B = glm::normalize(
        cameraToEllipsoidCenter + _ellipsoid.cartesianPosition(c1)
    );
    const glm::dvec3 C = glm::normalize(
        cameraToEllipsoidCenter + _ellipsoid.cartesianPosition(c2)
    );

    // Camera                      *cartesian space*
    // |                    +--------+---+
    // V             __--''   __--''    /
    //              C-------A--------- +
    // oo          /       /          /
    //[  ]<       +-------B----------+
    //

    // If the geodetic patch is small (i.e. has small width), that means the patch in
    // cartesian space will be almost flat, and in turn, the triangle ABC will roughly
    // correspond to 1/8 of the full area
    const glm::dvec3 AB = B - A;
    const glm::dvec3 AC = C - A;
    const double areaABC = 0.5 * glm::length(glm::cross(AC, AB));
    const double projectedChunkAreaApprox = 8 * areaABC;

    const double scaledArea = _generalProperties.lodScaleFactor *
        projectedChunkAreaApprox;
    return chunk.tileIndex().level + static_cast<int>(round(scaledArea - 1));
}

int RenderableGlobe::desiredLevelByAvailableTileData(const Chunk& chunk, const RenderData&) const {
    const int currLevel = chunk.tileIndex().level;

    for (size_t i = 0; i < layergroupid::NUM_LAYER_GROUPS; ++i) {
        for (const std::shared_ptr<Layer>& layer :
            _layerManager.layerGroup(i).activeLayers())
        {
            Tile::Status status = layer->tileStatus(chunk.tileIndex());
            if (status == Tile::Status::OK) {
                return UnknownDesiredLevel;
            }
        }
    }

    return currLevel - 1;
}

bool RenderableGlobe::isCullableByFrustum(const Chunk& chunk, const RenderData& renderData) const {
    // Calculate the MVP matrix
    const glm::dmat4 viewTransform = glm::dmat4(renderData.camera.combinedViewMatrix());
    const glm::dmat4 modelViewProjectionTransform = glm::dmat4(
        renderData.camera.sgctInternal.projectionMatrix()
    ) * viewTransform * _cachedModelTransform;

    const std::vector<glm::dvec4>& corners = chunk.boundingPolyhedronCorners();

    // Create a bounding box that fits the patch corners
    AABB3 bounds; // in screen space
    for (size_t i = 0; i < 8; ++i) {
        const glm::dvec4 cornerClippingSpace = modelViewProjectionTransform * corners[i];
        const glm::dvec3 ndc = glm::dvec3(
            (1.f / glm::abs(cornerClippingSpace.w)) * cornerClippingSpace
        );
        bounds.expand(ndc);
    }

    return !(CullingFrustum.intersects(bounds));
}

const LayerManager& RenderableGlobe::layerManager() const {
    return _layerManager;
}

LayerManager& RenderableGlobe::layerManager() {
    return _layerManager;
}

const Ellipsoid& RenderableGlobe::ellipsoid() const {
    return _ellipsoid;
}

const glm::dmat4& RenderableGlobe::modelTransform() const {
    return _cachedModelTransform;
}

Camera* RenderableGlobe::savedCamera() const {
    return _savedCamera.get();
}



bool RenderableGlobe::isCullableByHorizon(const Chunk& chunk, const RenderData& renderData) const {
    // Calculations are done in the reference frame of the globe. Hence, the camera
    // position needs to be transformed with the inverse model matrix
    const GeodeticPatch& patch = chunk.surfacePatch();
    const float maxHeight = chunk.boundingHeights().max;
    const glm::dvec3 globePos = glm::dvec3(0, 0, 0); // In model space it is 0
    const double minimumGlobeRadius = _ellipsoid.minimumRadius();

    const glm::dvec3 cameraPos = glm::dvec3(
        _cachedInverseModelTransform * glm::dvec4(renderData.camera.positionVec3(), 1)
    );

    const glm::dvec3 globeToCamera = cameraPos;

    const Geodetic2 cameraPositionOnGlobe = _ellipsoid.cartesianToGeodetic2(globeToCamera);
    const Geodetic2 closestPatchPoint = patch.closestPoint(cameraPositionOnGlobe);
    glm::dvec3 objectPos = _ellipsoid.cartesianSurfacePosition(closestPatchPoint);

    // objectPosition is closest in latlon space but not guaranteed to be closest in
    // castesian coordinates. Therefore we compare it to the corners and pick the
    // real closest point,
    std::array<glm::dvec3, 4> corners = {
        _ellipsoid.cartesianSurfacePosition(chunk.surfacePatch().corner(NORTH_WEST)),
        _ellipsoid.cartesianSurfacePosition(chunk.surfacePatch().corner(NORTH_EAST)),
        _ellipsoid.cartesianSurfacePosition(chunk.surfacePatch().corner(SOUTH_WEST)),
        _ellipsoid.cartesianSurfacePosition(chunk.surfacePatch().corner(SOUTH_EAST))
    };

    for (int i = 0; i < 4; ++i) {
        const double distance = glm::length(cameraPos - corners[i]);
        if (distance < glm::length(cameraPos - objectPos)) {
            objectPos = corners[i];
        }
    }


    const double objectP = pow(length(objectPos - globePos), 2);
    const double horizonP = pow(minimumGlobeRadius - maxHeight, 2);
    if (objectP < horizonP) {
        return false;
    }

    const double cameraP = pow(length(cameraPos - globePos), 2);
    const double minR = pow(minimumGlobeRadius, 2);
    if (cameraP < minR) {
        return false;
    }

    const double minimumAllowedDistanceToObjectFromHorizon = sqrt(objectP - horizonP);
    const double distanceToHorizon = sqrt(cameraP - minR);

    // Minimum allowed for the object to be occluded
    const double minimumAllowedDistanceToObjectSquared =
        pow(distanceToHorizon + minimumAllowedDistanceToObjectFromHorizon, 2) +
        pow(maxHeight, 2);

    const double distanceToObjectSquared = pow(
        length(objectPos - cameraPos),
        2
    );
    return distanceToObjectSquared > minimumAllowedDistanceToObjectSquared;
}

} // namespace openspace::globebrowsing
