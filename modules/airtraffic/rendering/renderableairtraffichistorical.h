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

    void initialize() override;
    void deinitialize() override;

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    bool fetchData(const Date& date); 

    bool updateBuffers(const Date& date);
    
    static documentation::Documentation Documentation();

private:
    static const int _THRESHOLD = -9999;

    struct AircraftVBOLayout {
        float latitude = static_cast<float>(_THRESHOLD);
        float longitude = static_cast<float>(_THRESHOLD);
        int firstSeen = 0;
        int lastSeen = 0;
    };

    struct Buffer {
        std::vector<AircraftVBOLayout>  vertexBufferData;
        Date date;
    };

    properties::Vec3Property _maximumColor;
    properties::Vec3Property _minimumColor;
    properties::FloatProperty _opacity;
    properties::IntProperty _nDailyFlights;

    // Backend storage for vertex buffer object containing all points
    //std::vector<AircraftVBOLayout>  _vertexBufferData;
    Buffer _bufferA;
    Buffer _bufferB;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader = nullptr;
    //std::string _currentDate = "";
    //std::string _nextDate = "";
    Date _currentDate;
    Date _nextDate;
    std::future<bool> _future;
    bool _isDataLoading = false;

    UniformCache(modelViewProjection, maximumColor, minimumColor, opacity, latitudeThreshold, longitudeThreshold, dailyFlights, time) _uniformCache;
    std::vector<std::vector<std::string>> _data;
    GLuint _vertexArrayA = 0;
    GLuint _vertexArrayB = 1;
    GLuint _vertexBufferA = 0;
    GLuint _vertexBufferB = 1;
    const std::string _PATH = "${MODULE_AIRTRAFFIC}/data/";

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_AIRTRAFFIC___RENDERABLEAIRTRAFFICHISTORICAL___H__
