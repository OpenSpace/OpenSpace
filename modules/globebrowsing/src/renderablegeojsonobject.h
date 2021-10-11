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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGEOJSONFEATURE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGEOJSONFEATURE___H__

#include <openspace/rendering/renderable.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }
namespace openspace::documentation { struct Documentation; }

namespace openspace::globebrowsing {

class RenderableGlobe;

/**
 * TODO: short documentation
 */
class RenderableGeoJsonObject : public Renderable {
public:
    RenderableGeoJsonObject(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

    enum class GeometryType {
        LineString = 0,
        Point,
        MultiPoint
    };

    struct Geometry {
        GeometryType type;
        std::vector<Geodetic2> coordinates; // radians. TODO: support Geodetic3 as well

        GLuint vaoId = 0;
        GLuint vboId = 0;
        std::vector<GLfloat> vertices;
    };

private:
    void renderGeometry(const RenderData&, const glm::dmat4& modelViewMatrix,
        const glm::dmat4& projectionMatrix);

    void findGlobe();
    void readFile();
    void initializeGeometry();

    glm::vec3 calculateModelCoordinate(const Geodetic2& geodetic);

    std::unique_ptr<ghoul::opengl::ProgramObject> _program = nullptr;

    std::vector<Geometry> _geometryFeatures;

    properties::StringProperty _globe;
    properties::StringProperty _geoJsonFile;
    properties::FloatProperty _heightOffset;
    properties::Vec2Property _latLongOffset;
    properties::BoolProperty _useHeightmap;
    properties::Vec3Property _color;
    properties::FloatProperty _pointSize;
    properties::FloatProperty _lineWidth;

    // Temporary property to get around height map update issues
    properties::TriggerProperty _forceUpdateData;
    bool _shouldForceUpdateData = false;

    RenderableGlobe* _globeNode = nullptr;

    bool _dataIsDirty = true;
    bool _dataIsInitialized = false;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGEOJSONFEATURE___H__
