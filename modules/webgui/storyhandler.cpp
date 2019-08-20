/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/webgui/storyhandler.h>

#include <openspace/openspace.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>

namespace {
    constexpr const openspace::properties::Property::PropertyInfo OverviewLimitInfo = {
        "OverviewLimit",
        "OverviewLimit",
        "Determines the overview limit for the overview button."
    };
    constexpr const openspace::properties::Property::PropertyInfo ZoomInLimitInfo = {
        "ZoomInLimit",
        "ZoomInLimit",
        "Determines the limit for zooming in."
    };
    constexpr const openspace::properties::Property::PropertyInfo StoryIdentifierInfo = {
        "StoryIdentifier",
        "StoryIdentifier",
        "Contains the name for the current story."
    };
    constexpr const openspace::properties::Property::PropertyInfo FocusNodesListInfo = {
        "FocusNodesList",
        "FocusNodesList",
        "Contains a string with all the wanted focus nodes listed for the current story."
    };
}

namespace openspace::webgui {

StoryHandler::StoryHandler()
    : properties::PropertyOwner({ "StoryHandler" })
    , _overviewLimit(OverviewLimitInfo, std::numeric_limits<float>::max())
    , _zoomInLimit(ZoomInLimitInfo, 0.0)
    , _storyIdentifier(StoryIdentifierInfo, "story_default")
    , _focusNodesList(FocusNodesListInfo, std::string(""))

{
    addProperty(_overviewLimit);
    addProperty(_zoomInLimit);
    addProperty(_storyIdentifier);
    addProperty(_focusNodesList);
};

float StoryHandler::overviewLimit() const {
    return _overviewLimit;
};

float StoryHandler::zoomInLimit() const {
    return _zoomInLimit;
};

};// namespace

