#include <modules/base/rendering/pointcloud/renderableaurorasaurusobservationcloud.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>

#include <algorithm>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo ObservedAltitudeInfo = {
        "ObservedAltitude",
        "Observed altitude",
        "The altitude in meters used for aurora observations.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo NotObservedAltitudeInfo = {
        "NotObservedAltitude",
        "Not observed altitude",
        "The altitude in meters used for reports where aurora was not seen.",
        Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

openspace::Documentation RenderableAurorasaurusObservationCloud::Documentation() {
    return {
        "RenderableAurorasaurusObservationCloud",
        "base_renderable_aurorasaurus_observation_cloud",
        "A timed textured globe point cloud specialized for Aurorasaurus observations.",
        {
            { "ObservationFile", new StringVerifier, Optional::No },
            { "IgnoreTimeFiltering", new BoolVerifier, Optional::Yes },
            { "ObservedAltitude", new DoubleVerifier, Optional::Yes },
            { "NotObservedAltitude", new DoubleVerifier, Optional::Yes }
        }
    };
}

ghoul::Dictionary RenderableAurorasaurusObservationCloud::aurorasaurusDictionary(
                                                   const ghoul::Dictionary& dictionary)
{
    ghoul::Dictionary result = dictionary;

    if (dictionary.hasValue<std::filesystem::path>("ObservationFile")) {
        result.setValue(
            "SourceFile",
            absPath(dictionary.value<std::filesystem::path>("ObservationFile"))
        );
    }
    else if (dictionary.hasValue<std::string>("ObservationFile")) {
        result.setValue(
            "SourceFile",
            absPath(dictionary.value<std::string>("ObservationFile"))
        );
    }
    else {
        throw ghoul::RuntimeError("Missing required key 'ObservationFile'");
    }

    result.setValue("LatitudeColumn", std::string("st_y"));
    result.setValue("LongitudeColumn", std::string("st_x"));
    result.setValue("StartTimeColumn", std::string("time_start"));
    result.setValue("EndTimeColumn", std::string("time_end"));
    result.setValue("FixedAltitude", 0.0);
    result.setValue("GlobeRadius", 6378137.0);
    result.setValue("TextureFiles", textureFiles());

    result.removeValue("ObservationFile");
    result.removeValue("ObservedAltitude");
    result.removeValue("NotObservedAltitude");

    return result;
}

RenderableAurorasaurusObservationCloud::RenderableAurorasaurusObservationCloud(
                                                  const ghoul::Dictionary& dictionary)
    : RenderableTimedGlobePointCloud(aurorasaurusDictionary(dictionary))
    , _observedAltitude(ObservedAltitudeInfo, 10000.f, 0.f, 1000000.f)
    , _notObservedAltitude(NotObservedAltitudeInfo, 9500.f, 0.f, 1000000.f)
{
    if (dictionary.hasValue<double>("ObservedAltitude")) {
        _observedAltitude = static_cast<float>(dictionary.value<double>("ObservedAltitude"));
    }
    else if (dictionary.hasValue<int>("ObservedAltitude")) {
        _observedAltitude = static_cast<float>(dictionary.value<int>("ObservedAltitude"));
    }

    if (dictionary.hasValue<double>("NotObservedAltitude")) {
        _notObservedAltitude = static_cast<float>(
            dictionary.value<double>("NotObservedAltitude")
        );
    }
    else if (dictionary.hasValue<int>("NotObservedAltitude")) {
        _notObservedAltitude = static_cast<float>(
            dictionary.value<int>("NotObservedAltitude")
        );
    }

    addProperty(_observedAltitude);
    addProperty(_notObservedAltitude);
}

void RenderableAurorasaurusObservationCloud::validateSourceColumns(
                                                   const HeaderMap& headerMap) const
{
    requiredColumnIndex(headerMap, "st_y");
    requiredColumnIndex(headerMap, "st_x");
    requiredColumnIndex(headerMap, "time_start");
    requiredColumnIndex(headerMap, "time_end");
    requiredColumnIndex(headerMap, "see_aurora");
    requiredColumnIndex(headerMap, "colors");
}

std::optional<RenderableAurorasaurusObservationCloud::TimedPoint>
RenderableAurorasaurusObservationCloud::pointFromRow(const std::vector<std::string>& row,
                                                     const HeaderMap& headerMap) const
{
    const size_t latitudeColumn = requiredColumnIndex(headerMap, "st_y");
    const size_t longitudeColumn = requiredColumnIndex(headerMap, "st_x");
    const size_t startTimeColumn = requiredColumnIndex(headerMap, "time_start");
    const size_t endTimeColumn = requiredColumnIndex(headerMap, "time_end");
    const size_t seeAuroraColumn = requiredColumnIndex(headerMap, "see_aurora");
    const size_t colorsColumn = requiredColumnIndex(headerMap, "colors");

    const std::string start = normalizeTimeString(row[startTimeColumn]);
    if (start.empty()) {
        return std::nullopt;
    }

    std::string end = normalizeTimeString(row[endTimeColumn]);
    if (end.empty()) {
        end = start;
    }

    const double latitude = std::stod(row[latitudeColumn]);
    const double longitude = std::stod(row[longitudeColumn]);
    const bool sawAurora = lowered(row[seeAuroraColumn]) == "true";
    const double altitude = sawAurora ? _observedAltitude.value() : _notObservedAltitude.value();

    TimedPoint point;
    point.position = glm::vec3(cartesianPositionFromDegrees(
        latitude,
        longitude,
        altitude,
        _globeRadius.value()
    ));
    point.textureIndex = static_cast<float>(
        textureIndexForObservation(sawAurora, row[colorsColumn])
    );
    point.startTime = Time::convertTime(start);
    point.endTime = Time::convertTime(end);

    if (point.endTime < point.startTime) {
        std::swap(point.startTime, point.endTime);
    }

    return point;
}

std::vector<std::string> RenderableAurorasaurusObservationCloud::textureFiles() {
    return {
        "grayIcon.png",
        "green2.png",
        "red2.png",
        "white2.png",
        "pink2.png",
        "redWhite2.png",
        "greenRed2.png",
        "redPink2.png",
        "greenWhite2.png",
        "whitePink2.png",
        "greenPink2.png",
        "greenRedWhite2.png",
        "redWhitePink2.png",
        "greenRedPink2.png",
        "greenWhitePink2.png",
        "greenRedWhitePink2.png"
    };
}

int RenderableAurorasaurusObservationCloud::textureIndexForObservation(
                                                 bool seeAurora, std::string_view colors)
{
    constexpr int Red = 1;
    constexpr int White = 2;
    constexpr int Green = 4;
    constexpr int Pink = 8;

    if (!seeAurora) {
        return 0;
    }

    int mask = 0;
    std::vector<std::string> tokens = ghoul::tokenizeString(std::string(colors), ',');
    for (std::string& token : tokens) {
        token = lowered(token);
        if (token.empty()) {
            continue;
        }
        if (token.starts_with("red")) {
            mask |= Red;
        }
        else if (token.starts_with("whit") || token.starts_with("white")) {
            mask |= White;
        }
        else if (token.starts_with("gree") || token.starts_with("green")) {
            mask |= Green;
        }
        else if (token.starts_with("pink")) {
            mask |= Pink;
        }
    }

    switch (mask) {
        case 0: return 1;
        case Red: return 2;
        case White: return 3;
        case Green: return 1;
        case Pink: return 4;
        case Red | White: return 5;
        case Red | Green: return 6;
        case Red | Pink: return 7;
        case White | Green: return 8;
        case White | Pink: return 9;
        case Green | Pink: return 10;
        case Red | White | Green: return 11;
        case Red | White | Pink: return 12;
        case Red | Green | Pink: return 13;
        case White | Green | Pink: return 14;
        case Red | White | Green | Pink: return 15;
        default: return 1;
    }
}

} // namespace openspace
