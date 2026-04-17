/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace openspace {

enum class ImageryAvailabilityState {
    NoData,
    WaitingForListing,
    WaitingForDownload,
    DecodePending,
    ShowingImage,
    Error
};

enum class FrameKnowledgeState {
    Unknown,
    KnownRemote,
    KnownLocal,
    KnownEmpty,
    RetryBackoff
};

struct RuntimeImageryStatus {
    std::string activeInstrument;
    int sourceId = -1;
    int64_t currentRequestKey = 0;
    double requestedSimTimeJ2000 = 0.0;
    std::optional<double> displayedFrameTimeJ2000;
    ImageryAvailabilityState availabilityState = ImageryAvailabilityState::NoData;
    std::string statusText;
    size_t queuedDownloads = 0;
    size_t activeDownloads = 0;
    size_t decodeQueueSize = 0;
};

inline std::string_view toString(ImageryAvailabilityState state) {
    switch (state) {
    case ImageryAvailabilityState::NoData: return "NoData";
    case ImageryAvailabilityState::WaitingForListing: return "WaitingForListing";
    case ImageryAvailabilityState::WaitingForDownload: return "WaitingForDownload";
    case ImageryAvailabilityState::DecodePending: return "DecodePending";
    case ImageryAvailabilityState::ShowingImage: return "ShowingImage";
    case ImageryAvailabilityState::Error: return "Error";
    }
    return "Unknown";
}

} // namespace openspace
