#include <modules/skybrowser/include/renderableskybrowser.h>

namespace {
    constexpr const char* _loggerCat = "RenderableSkyBrowser";

    constexpr const openspace::properties::Property::PropertyInfo BrowserDimensionInfo =
    {
        "BrowserDimensions",
        "Browser Dimensions Info",
        "Set the dimensions of the SkyTarget according to the SkyBrowser ratio "
    };


    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {

        // [[codegen::verbatim(BrowserDimensionInfo.description)]]
        std::optional<glm::vec2> browserDimensions;
    };

#include "renderableskybrowser_codegen.cpp"
} // namespace

namespace openspace {

    RenderableSkyBrowser::RenderableSkyBrowser(const ghoul::Dictionary& dictionary)
        : RenderablePlane(dictionary)
    {
    }

} // namespace
