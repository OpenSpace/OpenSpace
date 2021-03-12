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

#ifndef __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEDENSITYMAP___H__
#define __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEDENSITYMAP___H__

#include <iomanip>
#include <openspace/rendering/renderable.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/opengl/uniformcache.h>
#include <openspace/util/time.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/optionproperty.h>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }


class RenderableDensityMap : public Renderable {
public:
    explicit RenderableDensityMap(const ghoul::Dictionary& dictionary);
    virtual ~RenderableDensityMap() = default;

    void initialize() override;
    void deinitialize() override;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    bool fetchData(); 

    bool updateBuffers();
    
    static documentation::Documentation Documentation();

private:
    static const int _THRESHOLD = -9999;

    struct AircraftVBOLayout {
        float latitude = static_cast<float>(_THRESHOLD);
        float longitude = static_cast<float>(_THRESHOLD);
        int firstSeen = 0;
        int lastSeen = 0;
        int identifier = 0; // 0 for aircrafts, 1 for box
    };

    properties::Vec3Property _color;
    properties::FloatProperty _opacity;
    properties::Vec2Property _latitudeThreshold; 
    properties::Vec2Property _longitudeThreshold;
    properties::IntProperty _nTotalFlights;

    // Backend storage for vertex buffer object containing all points
    std::vector<AircraftVBOLayout>  _vertexBufferData;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader = nullptr;
    std::string _currentDate = "";

    UniformCache(modelViewProjection, color, opacity, latitudeThreshold, longitudeThreshold) _uniformCache;
    std::vector<std::vector<std::string>> _data;
    GLuint _vertexArray = 0;
    GLuint _vertexBuffer = 0;
    const std::string _PATH = "${MODULE_AIRTRAFFIC}/data/";
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEDENSITYMAP___H__
