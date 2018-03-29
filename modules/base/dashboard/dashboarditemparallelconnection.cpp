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
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <openspace/util/distanceconversion.h>

#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    const char* KeyFontMono = "Mono";
    const float DefaultFontSize = 10.f;

    static const openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    static const openspace::properties::Property::PropertyInfo FontSizeInfo = {
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
                                                             ghoul::Dictionary dictionary)
    : DashboardItem("ParallelConnection", "Parallel Connection")
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemDate"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    _fontName.onChange([this](){
        _font = OsEng.fontManager().font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(
            dictionary.value<double>(FontSizeInfo.identifier)
        );
    }
    _fontSize.onChange([this](){
        _font = OsEng.fontManager().font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _font = OsEng.fontManager().font(_fontName, _fontSize);
}

void DashboardItemParallelConnection::render(glm::vec2& penPosition) {
    ParallelConnection::Status status = OsEng.parallelPeer().status();
    size_t nConnections = OsEng.parallelPeer().nConnections();
    const std::string& hostName = OsEng.parallelPeer().hostName();

    std::string connectionInfo = "";
    int nClients = static_cast<int>(nConnections);
    if (status == ParallelConnection::Status::Host) {
        nClients--;
        if (nClients == 1) {
            connectionInfo = "Hosting session with 1 client";
        }
        else {
            connectionInfo =
                "Hosting session with " + std::to_string(nClients) + " clients";
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
        status == ParallelConnection::Status::ClientWithoutHost) {
        connectionInfo += "\n";
        if (nClients > 2) {
            std::string c = std::to_string(nClients - 1);
            connectionInfo += "You and " + c + " more clients are tuned in";
        }
        else if (nClients == 2) {
            std::string c = std::to_string(nClients - 1);
            connectionInfo += "You and " + c + " more client are tuned in";
        }
        else if (nClients == 1) {
            connectionInfo += "You are the only client";
        }
    }

    if (!connectionInfo.empty()) {
        penPosition.y -= _font->height();
        RenderFont(
            *_font,
            penPosition,
            connectionInfo.c_str()
        );
    }
}

glm::vec2 DashboardItemParallelConnection::size() const {
    ParallelConnection::Status status = OsEng.parallelPeer().status();
    size_t nConnections = OsEng.parallelPeer().nConnections();
    const std::string& hostName = OsEng.parallelPeer().hostName();

    std::string connectionInfo = "";
    int nClients = static_cast<int>(nConnections);
    if (status == ParallelConnection::Status::Host) {
        nClients--;
        if (nClients == 1) {
            connectionInfo = "Hosting session with 1 client";
        }
        else {
            connectionInfo =
                "Hosting session with " + std::to_string(nClients) + " clients";
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
        status == ParallelConnection::Status::ClientWithoutHost) {
        connectionInfo += "\n";
        if (nClients > 2) {
            std::string c = std::to_string(nClients - 1);
            connectionInfo += "You and " + c + " more clients are tuned in";
        }
        else if (nClients == 2) {
            std::string c = std::to_string(nClients - 1);
            connectionInfo += "You and " + c + " more client are tuned in";
        }
        else if (nClients == 1) {
            connectionInfo += "You are the only client";
        }
    }

    if (!connectionInfo.empty()) {
        return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
            *_font,
            connectionInfo.c_str()
        ).boundingBox;
    }
    else {
        return { 0.f, 0.f };
    }
}

} // namespace openspace
