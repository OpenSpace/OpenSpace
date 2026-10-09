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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLEDENSITYPOINTCLOUD___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLEDENSITYPOINTCLOUD___H__

#include <openspace/rendering/renderable.h>

#include <openspace/opengl/uniformcache.h>
#include <openspace/properties/list/intlistproperty.h>
#include <openspace/properties/misc/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <filesystem>
#include <memory>
#include <optional>
#include <string>

namespace openspace::opengl {
    class ProgramObject;
    class Texture;
} // namespace openspace::opengl

namespace openspace::exoplanets {

class DensityVolume;

/**
 * Renders the particles of a CSV dataset as GL points colored by their density, as
 * estimated by a DensityVolume.
 */
class RenderableDensityPointCloud : public Renderable {
public:
    explicit RenderableDensityPointCloud(const Dictionary& dictionary);
    ~RenderableDensityPointCloud() override;

    void initializeCmap();

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    void update(const UpdateData& data) override;
    void render(const RenderData& data, RendererTasks& tasks) override;

    const DensityVolume* densityVolume() const;

    /**
     * The transform from volume coordinates to clip space from the most recent render
     * call, or std::nullopt if the renderable has not been rendered yet.
     */
    std::optional<glm::dmat4> modelToClipTransform() const;

    static openspace::Documentation Documentation();

private:
    void uploadPointData();
    void uploadSelection();
    void uploadBoundingBox();

    std::filesystem::path _file;
    std::string _xColumn;
    std::string _yColumn;
    std::string _zColumn;
    glm::uvec3 _gridDimensions;
    double _unitToMeter;
    bool _hasUserDensityRange = false;

    std::unique_ptr<DensityVolume> _volume;
    std::unique_ptr<opengl::Texture> _colorMapTexture;
    bool _colorMapIsDirty = false;

    FloatProperty _pointSize;

    struct {
        PropertyOwner owner;

        Vec3Property fixedColor;

        BoolProperty useColorMap;
        StringProperty colorMapFile;
        Vec2Property densityRange;
        BoolProperty useLogScale;

        Vec3Property selectionColor;
        FloatProperty unselectedOpacity;
    } _color;

    FloatProperty _densityThreshold;
    IntListProperty _selectedIndices;
    TriggerProperty _clearSelection;

    struct {
        PropertyOwner owner;

        BoolProperty enabled;
        Vec4Property color;
    } _boundingBox;

    std::unique_ptr<opengl::ProgramObject> _pointsProgram;
    UniformCache(modelViewTransform, projectionTransform, unitToMeter, pointSize,
        fixedColor, useColorMap, colorMapTexture, densityRange, useLogScale,
        densityThreshold, hasSelection, selectionColor, unselectedOpacity,
        opacity) _pointsUniformCache;

    std::unique_ptr<opengl::ProgramObject> _boxProgram;
    UniformCache(modelViewTransform, projectionTransform, unitToMeter,
        color) _boxUniformCache;

    GLuint _pointsVao = 0;
    GLuint _pointsVbo = 0;
    GLuint _selectionVbo = 0;
    GLuint _boxVao = 0;
    GLuint _boxVbo = 0;

    bool _selectionIsDirty = true;

    std::optional<glm::dmat4> _modelToClip;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLEDENSITYPOINTCLOUD___H__
