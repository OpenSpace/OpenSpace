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

#ifndef __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICHISTORICAL___H__
#define __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICHISTORICAL___H__

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
#include <future>


namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

// Defines a date
// with only year, month, and day.
// Also defines operator==, operator!=
// and operator=
struct Date {
    int year = 0;
    int month = 0;
    int day = 0;

    Date getTomorrow() {
        const int days[12] = {
        31,28,31,30,31,30,31,31,30,31,30,31
        };

        // Just blindly add a day with no checks.
        Date tomorrow;
        tomorrow.year = year;
        tomorrow.month = month;
        tomorrow.day = day + 1;

        // Allow Feb 29 in leap year if needed.
        if (tomorrow.month == 2 && tomorrow.day == 29) {
            if (tomorrow.year % 400 == 0)
                return tomorrow;
            if ((tomorrow.year % 4 == 0) && (tomorrow.year % 100 != 0))
                return tomorrow;
        }

        // Catch rolling into new month.
        if (tomorrow.day > days[tomorrow.month - 1]) {
            tomorrow.day = 1;
            tomorrow.month++;

            // Catch rolling into new year.
            if (tomorrow.month == 13) {
                tomorrow.month = 1;
                tomorrow.year++;
            }
        }

        return tomorrow;
    }

    Date getYesterday() {
        const int days[12] = {
        31,28,31,30,31,30,31,31,30,31,30,31
        };

        // Just blindly add a day with no checks
        Date yesterday;
        yesterday.year = year;
        yesterday.month = month;
        yesterday.day = day - 1;

        // Catch rolling into new month
        if (yesterday.day == 0) {
            yesterday.month--;
            yesterday.day = days[yesterday.month];

            // Allow Feb 29 in leap year if needed
            if (yesterday.month == 2) {
                if (yesterday.year % 400 == 0) {
                    yesterday.day = 29;
                    return yesterday;
                }
                if ((yesterday.year % 4 == 0) && (yesterday.year % 100 != 0)) {
                    yesterday.day = 29;
                    return yesterday;
                }
            }

            // Catch rolling into new year
            if (yesterday.month == 0) {
                yesterday.month = 12;
                yesterday.year--;
            }
        }

        return yesterday;
    }

    bool operator==(const Date& d) const {
        return (year == d.year && month == d.month && day == d.day);
    }

    bool operator!=(const Date& d) const {
        return !(*this == d);
    }

    Date& operator=(const Date& d) {
        year = d.year;
        month = d.month;
        day = d.day;
        return *this;
    }
};

class RenderableAirTrafficHistorical : public Renderable {
public:
    explicit RenderableAirTrafficHistorical(const ghoul::Dictionary& dictionary);
    virtual ~RenderableAirTrafficHistorical() = default;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;

    bool fetchData(const Date& date); 
    
    void updateBuffers(const Date& date, const bool async = false);
    void updateBuffersReverse(const Date& date, const bool async = false);

    static documentation::Documentation Documentation();

private:
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

        Buffer(int idx)
        :vertexArray(idx), vertexBuffer(idx)
        {}
    };

    void fillBuffer(Buffer& buffer);
    void sendToGLBuffer(Buffer& buffer);
    Date convertDate(double timeNow); 
    
    // GUI properties
    properties::FloatProperty _opacity;
    properties::IntProperty _nDailyFlights;

    // Initilising backend storage for 
    // vertex buffer object containing all points
    Buffer _bufferA = Buffer(0);
    Buffer _bufferB = Buffer(1); 
    Buffer _bufferC = Buffer(2);

    // Initilize shader program an set Uniforms 
    std::unique_ptr<ghoul::opengl::ProgramObject> _shader = nullptr;
    UniformCache(modelViewProjection, opacity, latitudeThreshold, longitudeThreshold, time, cameraPosition, modelTransform) _uniformCache;
    
    // Date structs,
    // these will correspond to "today", "tomorrow",
    // and day after "tomorrow" for the date set in OpenSpace
    // and reverse
    Date _currentDate;
    Date _nextDate;
    Date _nextNextDate;
    
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
