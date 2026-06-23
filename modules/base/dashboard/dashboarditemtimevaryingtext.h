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

#ifndef __OPENSPACE_MODULE_BASE___DASHBOARDITEMTIMEVARYINGTEXT___H__
#define __OPENSPACE_MODULE_BASE___DASHBOARDITEMTIMEVARYINGTEXT___H__

#include <openspace/rendering/dashboardtextitem.h>

#include <glm/glm.hpp>
#include <openspace/json.h>
#include <openspace/properties/misc/stringproperty.h>
#include <vector>

namespace openspace {

class DashboardItemTimeVaryingText : public DashboardTextItem {
public:
    enum class FallbackMode {
        HoldLastValue,
        Clear,
        MissingText
    };

    explicit DashboardItemTimeVaryingText(const ghoul::Dictionary& dictionary);
    ~DashboardItemTimeVaryingText() override = default;

    void update() override;
    void render(glm::vec2& penPosition) override;

    static openspace::Documentation Documentation();

private:
    void loadDataFromJson(const std::string& filePath);
    void updateBufferForValue(const nlohmann::json& value);
    void applyFallback();
    int valueIndex(double currentTime) const;
    glm::vec4 colorForValue(const nlohmann::json& value) const;

    StringProperty _formatString;
    StringProperty _dataFile;

    std::vector<double> _timestamps;
    std::vector<nlohmann::json> _values;

    int _activeValueIndex = -1;
    double _averageInterval = 0.0;
    bool _timestampsAreIntervalEnd = false;
    bool _useKpColoring = false;
    FallbackMode _fallbackMode = FallbackMode::Clear;
    std::string _missingText;
    glm::vec4 _textColor = glm::vec4(1.f);
    glm::vec4 _lastResolvedColor = glm::vec4(1.f);
    bool _hasLastResolvedValue = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___DASHBOARDITEMTIMEVARYINGTEXT___H__
