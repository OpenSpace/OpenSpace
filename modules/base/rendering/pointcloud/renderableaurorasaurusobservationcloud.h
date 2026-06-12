#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEAURORASAURUSOBSERVATIONCLOUD___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEAURORASAURUSOBSERVATIONCLOUD___H__

#include <modules/base/rendering/pointcloud/renderablepointcloud.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/util/updatestructures.h>

#include <filesystem>
#include <limits>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ghoul { class Dictionary; }

namespace openspace {

class RenderableAurorasaurusObservationCloud : public RenderablePointCloud {
public:
    explicit RenderableAurorasaurusObservationCloud(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void update(const UpdateData& data) override;

    static openspace::Documentation Documentation();

protected:
    void updateBufferData() override;

private:
    struct Observation {
        glm::vec3 position = glm::vec3(0.f);
        float textureIndex = 0.f;
        double startTime = 0.0;
        double endTime = 0.0;
    };

    static ghoul::Dictionary sanitizedDictionary(const ghoul::Dictionary& dictionary);

    void loadObservations();
    void rebuildActiveSet(double currentTime);
    void advanceActiveSet(double currentTime);
    void activateObservation(size_t index);
    void deactivateObservation(size_t index);
    void rebuildDatasetEntries();

    static std::string normalizeTimeString(std::string value);
    static int textureIndexForObservation(bool seeAurora, std::string_view colors);
    static std::vector<dataloader::Dataset::Texture> textureMapping();

    StringProperty _observationFile;
    BoolProperty _ignoreTimeFiltering;
    FloatProperty _observedAltitude;
    FloatProperty _notObservedAltitude;
    UIntProperty _loadedObservationCount;
    UIntProperty _activeObservationCount;

    std::filesystem::path _dataPath;
    std::vector<Observation> _observations;
    std::vector<size_t> _activeObservationIndices;
    std::vector<int> _activeObservationSlots;
    std::vector<std::pair<double, size_t>> _startTimes;
    std::vector<std::pair<double, size_t>> _endTimes;

    size_t _nextStartIndex = 0;
    size_t _nextEndIndex = 0;
    double _lastUpdateTime = std::numeric_limits<double>::quiet_NaN();
    double _maxObservationRadius = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEAURORASAURUSOBSERVATIONCLOUD___H__
