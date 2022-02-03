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

#ifndef __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICHISTORICAL___H__
#define __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICHISTORICAL___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/intproperty.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <future>

namespace ghoul::filesystem { class File; }

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableAirTrafficHistorical : public Renderable {
public:
    explicit RenderableAirTrafficHistorical(const ghoul::Dictionary& dictionary);
    virtual ~RenderableAirTrafficHistorical() = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    
    static documentation::Documentation Documentation();

private:

    // Defines a date with only year, month, and day.
    //  Also defines operator==, operator!= and operator=
    struct Date {
        int year = 0;
        int month = 0;
        int day = 0;

        Date() = default;
        Date(double timeNow);
        Date& operator=(const Date& d);

        Date getTomorrow();
        Date getYesterday();

        bool operator==(const Date& d) const;
        bool operator!=(const Date& d) const;
    };


    bool fetchData(const Date& date);
    void updateBuffers(const Date& date, bool async = false);
    void updateBuffersReverse(const Date& date, bool async = false);

    // Stores some number outside the latitude and longitude 
    // interval used for validation 
    static const int _THRESHOLD = -9999;

    struct AircraftVBOLayout {
        float latitude = static_cast<float>(_THRESHOLD);
        float longitude = static_cast<float>(_THRESHOLD);
        int firstSeen = 0;
        int lastSeen = 0;
    };

    // Stores the backend storage in vertexBufferData
    // also contains the vertexArray, vertexBuffer,
    // and a Date struct for the day the data is valid
    struct Buffer {
        std::vector<AircraftVBOLayout>  vertexBufferData;
        Date date;
        GLuint vertexArray;
        GLuint vertexBuffer;

        Buffer(int idx) : vertexArray(idx), vertexBuffer(idx) {}
    };

    void fillBuffer(Buffer& buffer);
    void sendToGLBuffer(Buffer& buffer);
    
    // GUI properties
    properties::IntProperty _nRenderedFlights;

    // Initializing backend storage for 
    // vertex buffer object containing all points
    Buffer _bufferA = Buffer(0);
    Buffer _bufferB = Buffer(1); 
    Buffer _bufferC = Buffer(2);

    // Initilize shader program an set Uniforms 
    std::unique_ptr<ghoul::opengl::ProgramObject> _shader = nullptr;
    UniformCache(modelViewProjection, opacity, latitudeThreshold, longitudeThreshold,
        time, cameraPosition, modelTransform, clipping) _uniformCache;
    
    // Date structs,
    // these will correspond to "today", "tomorrow",
    // and day after "tomorrow" for the date set in OpenSpace
    // and reverse
    Date _currentDate;
    Date _nextDate;
    Date _nextNextDate;

    // SSBO
    GLuint _ssbo = 1;
    int _nFilteredAircraft = 0;
    int* _ptr;
    
    // Stuff related to the data loading
    std::future<void> _future;
    bool _isDataLoading = false;
    std::vector<std::vector<std::string>> _data;
    const std::string _PATH = "${MODULE_AIRTRAFFIC}/data/";
   
    // Used to keep track of time direction
    double _lastUpdate = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICHISTORICAL___H__
