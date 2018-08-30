/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/base/dashboard/dashboarditemparallelconnection.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/network/parallelconnection.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    constexpr const char* KeyFontMono = "Mono";
    constexpr const float DefaultFontSize = 10.f;

    constexpr openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the date."
    };
} // namespace

namespace openspace {

documentation::Documentation DashboardItemParallelConnection::Documentation() {
    using namespace documentation;
    return {
        "DashboardItem Parallel Connection",
        "base_dashboarditem_parallelconnection",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemParallelConnection"),
                Optional::No
            },
            {
                FontNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                FontNameInfo.description
            },
            {
                FontSizeInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                FontSizeInfo.description
            }
        }
    };
}

DashboardItemParallelConnection::DashboardItemParallelConnection(
                                                      const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemParallelConnection"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    _fontName.onChange([this](){
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(
            dictionary.value<double>(FontSizeInfo.identifier)
        );
    }
    _fontSize.onChange([this](){
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _font = global::fontManager.font(_fontName, _fontSize);
}

void DashboardItemParallelConnection::render(glm::vec2& penPosition) {
    const ParallelConnection::Status status = global::parallelPeer.status();
    const size_t nConnections = global::parallelPeer.nConnections();
    const std::string& hostName = global::parallelPeer.hostName();

    std::string connectionInfo;
    int nClients = static_cast<int>(nConnections);
    if (status == ParallelConnection::Status::Host) {
        nClients--;
        constexpr const char* Singular = "Hosting session with {} client";
        constexpr const char* Plural = "Hosting session with {} clients";

        connectionInfo =
            (nClients == 1) ?
            fmt::format(Singular, nClients) :
            fmt::format(Plural, nClients);
    }
    else if (status == ParallelConnection::Status::ClientWithHost) {
        nClients--;
        connectionInfo = "Session hosted by '" + hostName + "'";
    }
    else if (status == ParallelConnection::Status::ClientWithoutHost) {
        connectionInfo = "Host is disconnected";
    }

    if (status == ParallelConnection::Status::ClientWithHost ||
        status == ParallelConnection::Status::ClientWithoutHost)
    {
        constexpr const char* Singular = "You and {} more client are tuned in";
        constexpr const char* Plural = "You and {} more clients are tuned in";

        connectionInfo += "\n";

        if (nClients > 2) {
            connectionInfo += fmt::format(Plural, nClients - 1);
        }
        else if (nClients == 2) {
            connectionInfo += fmt::format(Singular, nClients - 1);
        }
        else if (nClients == 1) {
            connectionInfo += "You are the only client";
        }
    }

    if (!connectionInfo.empty()) {
        penPosition.y -= _font->height();
        RenderFont(*_font, penPosition, connectionInfo);
    }
}

glm::vec2 DashboardItemParallelConnection::size() const {
    ParallelConnection::Status status = global::parallelPeer.status();
    size_t nConnections = global::parallelPeer.nConnections();
    const std::string& hostName = global::parallelPeer.hostName();

    std::string connectionInfo;
    int nClients = static_cast<int>(nConnections);
    if (status == ParallelConnection::Status::Host) {
        nClients--;
        if (nClients == 1) {
            connectionInfo = "Hosting session with 1 client";
        }
        else {
            connectionInfo = fmt::format("Hosting session with {} clients", nClients);
        }
    }
    else if (status == ParallelConnection::Status::ClientWithHost) {
        nClients--;
        connectionInfo = "Session hosted by '" + hostName + "'";
    }
    else if (status == ParallelConnection::Status::ClientWithoutHost) {
        connectionInfo = "Host is disconnected";
    }

    if (status == ParallelConnection::Status::ClientWithHost ||
        status == ParallelConnection::Status::ClientWithoutHost)
    {
        constexpr const char* Singular = "You and {} more client are tuned in";
        constexpr const char* Plural = "You and {} more clients are tuned in";

        connectionInfo += "\n";
        if (nClients > 2) {
            std::string c = std::to_string(nClients - 1);
            connectionInfo += fmt::format(Plural, nClients);
        }
        else if (nClients == 2) {
            std::string c = std::to_string(nClients - 1);
            connectionInfo += fmt::format(Singular, nClients - 1);
        }
        else if (nClients == 1) {
            connectionInfo += "You are the only client";
        }
    }

    if (!connectionInfo.empty()) {
        return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
            *_font,
            connectionInfo
        ).boundingBox;
    }
    else {
        return { 0.f, 0.f };
    }
}

} // namespace openspace
