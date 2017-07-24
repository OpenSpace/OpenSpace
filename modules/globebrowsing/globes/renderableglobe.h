/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__

#include <openspace/rendering/renderable.h>

#include <modules/globebrowsing/geometry/ellipsoid.h>
#include <modules/globebrowsing/other/distanceswitch.h>

#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/boolproperty.h>

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
namespace openspace {
    class AtmosphereDeferredcaster;
}
#endif

namespace openspace::globebrowsing {

class ChunkedLodGlobe;
class PointGlobe;
class LayerManager;

/**
 * A <code>RenderableGlobe</code> is a globe modeled as an ellipsoid using a chunked LOD
 * algorithm for rendering.

 * The renderable uses a <code>DistanceSwitch</code> to determine if the renderable
 * should be rendered. 
 */
class RenderableGlobe : public Renderable {
public:
    /**
     * These properties are specific for <code>ChunkedLodGlobe</code> and separated from
     * the general properties of <code>RenderableGlobe</code>.
     */
    struct DebugProperties {
        properties::BoolProperty saveOrThrowCamera;
        properties::BoolProperty showChunkEdges;
        properties::BoolProperty showChunkBounds;
        properties::BoolProperty showChunkAABB;
        properties::BoolProperty showHeightResolution;
        properties::BoolProperty showHeightIntensities;
        properties::BoolProperty performFrustumCulling;
        properties::BoolProperty performHorizonCulling;
        properties::BoolProperty levelByProjectedAreaElseDistance;
        properties::BoolProperty resetTileProviders;
        properties::BoolProperty toggleEnabledEveryFrame;
        properties::BoolProperty collectStats;
        properties::BoolProperty limitLevelByAvailableData;
        properties::IntProperty modelSpaceRenderingCutoffLevel;
    };
    
    struct GeneralProperties {
        properties::BoolProperty isEnabled;
        properties::BoolProperty performShading;
        properties::BoolProperty atmosphereEnabled;
        properties::BoolProperty useAccurateNormals;
        properties::FloatProperty lodScaleFactor;
        properties::FloatProperty cameraMinHeight;        
        properties::FloatProperty orenNayarRoughness;
    };

    // Shadow structure
    typedef struct {
        std::pair<std::string, float> source;
        std::pair<std::string, float> caster;
    } ShadowConf;

    struct ShadowRenderingStruct {
        float xu, xp;
        float rs, rc;
        glm::vec3 sourceCasterVec;
        glm::vec3 casterPositionVec;
        bool isShadowing;
    };
    
    RenderableGlobe(const ghoul::Dictionary& dictionary);
    ~RenderableGlobe() = default;

    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    // Getters that perform calculations
    glm::dvec3 projectOnEllipsoid(glm::dvec3 position);
    float getHeight(glm::dvec3 position);
    
    // Getters
    std::shared_ptr<ChunkedLodGlobe> chunkedLodGlobe() const;
    LayerManager* layerManager() const;
    const Ellipsoid& ellipsoid() const;
    const glm::dmat4& modelTransform() const;
    const glm::dmat4& inverseModelTransform() const;
    const DebugProperties& debugProperties() const;
    const GeneralProperties& generalProperties() const;
    const std::shared_ptr<const Camera> savedCamera() const;
    double interactionDepthBelowEllipsoid();

    // Setters
    void setSaveCamera(std::shared_ptr<Camera> camera);

    virtual SurfacePositionHandle calculateSurfacePositionHandle(
                                             const glm::dvec3& targetModelSpace) override; 

private:
    // Globes. These are renderables inserted in a distance switch so that the heavier
    // <code>ChunkedLodGlobe</code> does not have to be rendered at far distances.
    std::shared_ptr<ChunkedLodGlobe> _chunkedLodGlobe;
    //std::shared_ptr<PointGlobe> _pointGlobe;

    Ellipsoid _ellipsoid;
    std::shared_ptr<LayerManager> _layerManager;
    DistanceSwitch _distanceSwitch;
    std::shared_ptr<Camera> _savedCamera;
    
    std::string _frame;
    double _time;

    glm::dmat4 _cachedModelTransform;
    glm::dmat4 _cachedInverseModelTransform;

    // Properties
    DebugProperties _debugProperties;
    GeneralProperties _generalProperties;
    properties::PropertyOwner _debugPropertyOwner;
    properties::PropertyOwner _texturePropertyOwner;

    // Shadow
    bool _shadowEnabled;
    std::vector< ShadowConf > _shadowConfArray;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__
