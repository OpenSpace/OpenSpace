#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETIMEDTEXTUREDGLOBEPOINTCLOUD___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETIMEDTEXTUREDGLOBEPOINTCLOUD___H__

#include <modules/base/rendering/pointcloud/renderablepointcloud.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/util/updatestructures.h>

#include <filesystem>
#include <limits>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ghoul { class Dictionary; }

namespace openspace {

class RenderableTimedTexturedGlobePointCloud : public RenderablePointCloud {
public:
    explicit RenderableTimedTexturedGlobePointCloud(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void update(const UpdateData& data) override;

    static openspace::Documentation Documentation();

protected:
    struct TimedPoint {
        glm::vec3 position = glm::vec3(0.f);
        float textureIndex = 0.f;
        double startTime = 0.0;
        double endTime = 0.0;
    };

    using HeaderMap = std::unordered_map<std::string, size_t>;

    void updateBufferData() override;

    virtual void validateSourceColumns(const HeaderMap& headerMap) const;
    virtual std::optional<TimedPoint> pointFromRow(const std::vector<std::string>& row,
        const HeaderMap& headerMap) const;

    static size_t requiredColumnIndex(const HeaderMap& headerMap, std::string_view name);
    static std::optional<size_t> optionalColumnIndex(const HeaderMap& headerMap,
        std::string_view name);

    static std::string normalizeTimeString(std::string value);
    static std::string lowered(std::string value);
    static glm::dvec3 cartesianPositionFromDegrees(double latitude, double longitude,
        double altitude, double globeRadius);

    int textureIndexForKey(std::string_view key) const;

    StringProperty _sourceFile;
    BoolProperty _ignoreTimeFiltering;
    StringProperty _latitudeColumn;
    StringProperty _longitudeColumn;
    StringProperty _startTimeColumn;
    StringProperty _endTimeColumn;
    StringProperty _altitudeColumn;
    FloatProperty _fixedAltitude;
    StringProperty _textureIndexColumn;
    StringProperty _textureKeyColumn;
    FloatProperty _globeRadius;
    UIntProperty _loadedPointCount;
    UIntProperty _activePointCount;

    std::filesystem::path _dataPath;
    std::vector<std::string> _textureFiles;
    std::vector<std::string> _textureKeys;

    std::vector<TimedPoint> _points;
    std::vector<size_t> _activePointIndices;
    std::vector<int> _activePointSlots;
    std::vector<std::pair<double, size_t>> _startTimes;
    std::vector<std::pair<double, size_t>> _endTimes;

    size_t _nextStartIndex = 0;
    size_t _nextEndIndex = 0;
    double _lastUpdateTime = std::numeric_limits<double>::quiet_NaN();
    double _maxPointRadius = 0.0;

private:
    static ghoul::Dictionary sanitizedDictionary(const ghoul::Dictionary& dictionary);

    void loadPoints();
    void rebuildActiveSet(double currentTime);
    void advanceActiveSet(double currentTime);
    void activatePoint(size_t index);
    void deactivatePoint(size_t index);
    void rebuildDatasetEntries();
    std::vector<dataloader::Dataset::Texture> textureMapping() const;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETIMEDTEXTUREDGLOBEPOINTCLOUD___H__
