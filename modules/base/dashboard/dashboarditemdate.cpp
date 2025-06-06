/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/base/dashboard/dashboarditemdate.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo FormatStringInfo = {
        "FormatString",
        "Format String",
        "The format text describing how this dashboard item renders its text. This text "
        "must contain exactly one {} which is a placeholder that will contain the date "
        "in the format as specified by `TimeFormat`.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TimeFormatInfo = {
        "TimeFormat",
        "Time Format",
        "The format string used for formatting the date/time before being passed to the "
        "string in FormatString. See "
        "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/timout_c.html for full "
        "information about how to structure this format.",
        openspace::properties::Property::Visibility::User
    };

    // This `DashboardItem` shows the current in-game simulation time. The `FormatString`
    // and the `TimeFormat` options provide the ability to customize the output that is
    // printed. See these two parameters for more information on how to structure the
    // inputs.
    struct [[codegen::Dictionary(DashboardItemDate)]] Parameters {
        // [[codegen::verbatim(FormatStringInfo.description)]]
        std::optional<std::string> formatString;

        // [[codegen::verbatim(TimeFormatInfo.description)]]
        std::optional<std::string> timeFormat;
    };
#include "dashboarditemdate_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemDate::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_date",
        DashboardTextItem::Documentation()
    );
}

DashboardItemDate::DashboardItemDate(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary, 15.f)
    , _formatString(FormatStringInfo, "Date: {}")
    , _timeFormat(TimeFormatInfo, "YYYY MON DD HR:MN:SC.### UTC ::RND")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _formatString = p.formatString.value_or(_formatString);
    addProperty(_formatString);

    _timeFormat = p.timeFormat.value_or(_timeFormat);
    addProperty(_timeFormat);
}

void DashboardItemDate::update() {
    ZoneScoped;

    std::string time = SpiceManager::ref().dateFromEphemerisTime(
        global::timeManager->time().j2000Seconds(),
        _timeFormat.value().c_str()
    );

    try {
        // @CPP26(abock): This can be replaced with std::runtime_format
        _buffer = std::vformat(_formatString.value(), std::make_format_args(time));
    }
    catch (const std::format_error&) {
        LERRORC("DashboardItemDate", "Illegal format string");
    }
}

} // namespace openspace
