/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#ifndef __RENDERABLEGLOBE_H__
#define __RENDERABLEGLOBE_H__

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/threadpool.h>

#include <openspace/rendering/renderable.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/util/updatestructures.h>

#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/globes/pointglobe.h>
#include <modules/globebrowsing/meshes/trianglesoup.h>
#include <modules/globebrowsing/geometry/ellipsoid.h>
#include <modules/globebrowsing/layered_rendering/layeredtextures.h>
#include <modules/globebrowsing/other/distanceswitch.h>

#include <unordered_map>

namespace openspace {

class ChunkedLodGlobe;
class TileProviderManager;

class SingleTexturePropertyOwner : public properties::PropertyOwner
{
public:
    SingleTexturePropertyOwner(std::string name);
    ~SingleTexturePropertyOwner();

    properties::BoolProperty isEnabled;
};

class LayeredCategoryPropertyOwner : public properties::PropertyOwner
{
public:
    LayeredCategoryPropertyOwner(
        LayeredTextures::TextureCategory category,
        TileProviderManager& tileProviderManager);
    ~LayeredCategoryPropertyOwner();
private:
    TileProviderManager& _tileProviderManager;
    std::vector<std::unique_ptr<SingleTexturePropertyOwner> >
        _texturePropertyOwners;
    properties::BoolProperty _levelBlendingEnabled;
};

class RenderableGlobe : public Renderable {
public:
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
    };
    
    struct GeneralProperties {
        properties::BoolProperty isEnabled;
        properties::BoolProperty performShading;
        properties::BoolProperty atmosphereEnabled;
        properties::FloatProperty lodScaleFactor;
        properties::FloatProperty cameraMinHeight;
    };
    
    RenderableGlobe(const ghoul::Dictionary& dictionary);
    ~RenderableGlobe();

    bool initialize() override;
    bool deinitialize() override;
    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

    // Getters that perform calculations
    glm::dvec3 projectOnEllipsoid(glm::dvec3 position);
    float getHeight(glm::dvec3 position);
    
    // Getters
    std::shared_ptr<ChunkedLodGlobe> chunkedLodGlobe() const;
    const Ellipsoid& ellipsoid() const;
    const glm::dmat4& modelTransform() const;
    const glm::dmat4& inverseModelTransform() const;
    const DebugProperties& debugProperties() const;
    const GeneralProperties& generalProperties() const;
    const std::shared_ptr<const Camera> savedCamera() const;

    double interactionDepthBelowEllipsoid();

    // Setters
    void setSaveCamera(std::shared_ptr<Camera> camera);    
private:
    std::shared_ptr<ChunkedLodGlobe> _chunkedLodGlobe;
    std::shared_ptr<PointGlobe> _pointGlobe;

    Ellipsoid _ellipsoid;
    std::shared_ptr<TileProviderManager> _tileProviderManager;
    DistanceSwitch _distanceSwitch;
    std::shared_ptr<Camera> _savedCamera;
    
    double _interactionDepthBelowEllipsoid;
    std::string _frame;
    double _time;

    glm::dmat4 _cachedModelTransform;
    glm::dmat4 _cachedInverseModelTransform;

    // Properties
    DebugProperties _debugProperties;
    GeneralProperties _generalProperties;

    properties::PropertyOwner _debugPropertyOwner;
    properties::PropertyOwner _texturePropertyOwner;
    
    std::vector<std::unique_ptr<LayeredCategoryPropertyOwner> > _textureProperties;
};

}  // namespace openspace

#endif  // __RENDERABLEGLOBE_H__
