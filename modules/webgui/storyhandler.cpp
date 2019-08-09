#include <openspace/engine/globals.h>
#include "storyhandler.h"
#include <openspace/scripting/scriptengine.h>
#include <openspace/openspace.h>
#include <openspace/engine/openspaceengine.h>

namespace {
    const std::string _loggerCat = "StoryHandler";

    static const openspace::properties::Property::PropertyInfo OverviewLimitInfo = {
        "OverviewLimit",
        "OverviewLimit",
        "Determines the overview limit for the overview button."
    };
    static const openspace::properties::Property::PropertyInfo ZoomInLimitInfo = {
        "ZoomInLimit",
        "ZoomInLimit",
        "Determines the limit for zooming in."
    };
    static const openspace::properties::Property::PropertyInfo StoryIdentifierInfo = {
        "StoryIdentifier",
        "StoryIdentifier",
        "Contains the name for the current story."
    };
    static const openspace::properties::Property::PropertyInfo FocusNodesListInfo = {
        "FocusNodesList",
        "FocusNodesList",
        "Contains a string with all the wanted focus nodes listed for the current story."
    };
}

namespace openspace::webgui {
    StoryHandler::StoryHandler() : properties::PropertyOwner({ "StoryHandler" })
        , _overviewLimit(OverviewLimitInfo, 0.0)
        , _zoomInLimit(ZoomInLimitInfo, 0.0)
        , _storyIdentifier(StoryIdentifierInfo, "story_default")
        , _focusNodesList(FocusNodesListInfo, std::string(""))

    {
        addProperty(_overviewLimit);
        addProperty(_zoomInLimit);
        addProperty(_storyIdentifier);
        addProperty(_focusNodesList);

    };

    float StoryHandler::overviewLimit() {
        return _overviewLimit;
    };

    float StoryHandler::zoomInLimit() {
        return _zoomInLimit;
    };

};// namespace

