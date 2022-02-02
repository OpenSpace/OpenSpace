/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICLIVE___H__
#define __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICLIVE___H__

#include <openspace/rendering/renderable.h>

#include <openspace/json.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/time.h>
#include <ghoul/opengl/uniformcache.h>
#include <future>
#include <list>

namespace ghoul::filesystem { class File; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableAirTrafficLive : public Renderable {
public:
    explicit RenderableAirTrafficLive(const ghoul::Dictionary& dictionary);
    virtual ~RenderableAirTrafficLive() = default;

    void initializeGL() override;
    void deinitializeGL() override;

    nlohmann::json fetchData();


    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;

    void updateBuffers();
    
    static documentation::Documentation Documentation();

private:
    nlohmann::json parseData(std::string_view data);
    
    static const int _TRAILSIZE = 10;
    static const int _THRESHOLD = -9999;
    properties::FloatProperty _lineWidth;
    properties::Vec3Property _color;
    properties::FloatProperty _opacity;
    properties::IntProperty _nRenderedAircraft;

    struct AircraftVBOLayout {
        float latitude = static_cast<float>(_THRESHOLD);
        float longitude = static_cast<float>(_THRESHOLD);
        float barometricAltitude = 0.f;
    };

    // Backend storage for vertex buffer object containing all points
    std::vector<AircraftVBOLayout>  _vertexBufferData;
   
    std::unique_ptr<ghoul::opengl::ProgramObject> _shader = nullptr;

    UniformCache(modelViewProjection, trailSize, resolution, lineWidth, color, opacity,
        latitudeThreshold, longitudeThreshold) _uniformCache;

    GLuint _vertexArray = 0;
    GLuint _vertexBuffer = 0;
    
    std::future<nlohmann::json> _future;
    bool _isDataLoading = false;
    nlohmann::json _data = nlohmann::json();

    // For storing the trails of each aircraft
    std::unordered_map<std::string, std::list<AircraftVBOLayout>> _aircraftMap;
    
    // Fix secure way to handle credentials
    const std::string _url = "https://opensky-network.org/api/states/all";
    double _deltaTime = Time::now().j2000Seconds();
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICLIVE___H__
