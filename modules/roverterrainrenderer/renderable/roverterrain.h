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

#ifndef __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___ROVER_TERRAIN___H__
#define __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___ROVER_TERRAIN___H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>

#include <modules/roverterrainrenderer/filehandler/subsite.h>
#include <modules/roverterrainrenderer/filehandler/roverpathfilereader.h>
#include <modules/roverterrainrenderer/renderable/sitemanager.h>
#include <modules/roverterrainrenderer/renderable/cachingsurfacemodelprovider.h>
#include <modules/roverterrainrenderer/renderable/renderableexplorationpath.h>
#include <modules/roverterrainrenderer/renderable/lodmodelswitch.h>

struct RenderData;
struct UpdateData;
struct Subsite;

namespace Documentation { struct Documentation; }

namespace openspace {

class RoverTerrain : public Renderable {
public:
    struct GeneralProperties {
        properties::BoolProperty isEnabled;
        properties::BoolProperty enablePath;
        properties::BoolProperty lockSubsite;
        properties::BoolProperty useMastCam;
        properties::BoolProperty enableDepth;
        properties::BoolProperty enableCulling;
        properties::FloatProperty heightProp;
        properties::IntProperty maxLod;
    };

    RoverTerrain(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;
    void initialize() override;
    void deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;
private:
    std::vector<std::shared_ptr<SubsiteModels>> calculateSurfacePosition(std::vector<std::shared_ptr<SubsiteModels>> vector);
    void lockSubsite(std::vector<std::shared_ptr<Subsite>> subsites);

    std::string _modelPath;
    std::string _texturePath;
    std::string _roverLocationPath;

    std::vector<std::shared_ptr<Subsite>> _subsites;
    std::vector<std::shared_ptr<Subsite>> _prevSubsites;
    std::vector<std::shared_ptr<Subsite>> _subsitesWithModels;

    std::shared_ptr<SiteManager> _siteManager;

    std::shared_ptr<CachingSurfaceModelProvider> _cachingModelProvider;

    std::shared_ptr<RenderableExplorationPath> _renderableExplorationPath;
    
    openspace::SceneGraphNode* _parent;

    globebrowsing::RenderableGlobe* _globe;

    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;

    LodModelSwitch _modelSwitch;
    int _prevLevel;

    GeneralProperties _generalProperties;

    std::vector<Geodetic2> _coordinatesModels;

    properties::Vec3Property _debugModelRotation;

    bool _isFirst;
    bool _isFirstLow;
    bool _isFirstHigh;

    bool _pressedOnce = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ROVER_TERRAIN_RENDERER___ROVER_TERRAIN___H__
