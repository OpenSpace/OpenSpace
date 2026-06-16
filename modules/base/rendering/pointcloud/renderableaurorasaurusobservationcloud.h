#ifndef __OPENSPACE_MODULE_BASE___RENDERABLEAURORASAURUSOBSERVATIONCLOUD___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLEAURORASAURUSOBSERVATIONCLOUD___H__

#include <modules/base/rendering/pointcloud/renderabletimedglobepointcloud.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/floatproperty.h>

#include <optional>
#include <string_view>
#include <vector>

namespace ghoul { class Dictionary; }

namespace openspace {

class RenderableAurorasaurusObservationCloud : public RenderableTimedGlobePointCloud {
public:
    explicit RenderableAurorasaurusObservationCloud(const ghoul::Dictionary& dictionary);

    static openspace::Documentation Documentation();

protected:
    void validateSourceColumns(const HeaderMap& headerMap) const override;
    std::optional<TimedPoint> pointFromRow(const std::vector<std::string>& row,
        const HeaderMap& headerMap) const override;

private:
    static ghoul::Dictionary aurorasaurusDictionary(const ghoul::Dictionary& dictionary);
    static std::vector<std::string> textureFiles();
    static int textureIndexForObservation(bool seeAurora, std::string_view colors);

    FloatProperty _observedAltitude;
    FloatProperty _notObservedAltitude;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLEAURORASAURUSOBSERVATIONCLOUD___H__
