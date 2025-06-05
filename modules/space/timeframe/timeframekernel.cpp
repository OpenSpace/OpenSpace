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

#include <modules/space/timeframe/timeframekernel.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/logging/logmanager.h>
#include "SpiceUsr.h"

namespace {
    constexpr std::string_view _loggerCat = "TimeFrameKernel";
    constexpr unsigned SpiceErrorBufferSize = 1841;

    std::vector<openspace::TimeRange> extractTimeFramesSPK(
                                        const std::vector<std::filesystem::path>& kernels,
                                             const std::variant<std::string, int>& object)
    {
        using namespace openspace;
        std::vector<TimeRange> res;

        // Load the kernel to be able to resolve the provided object name
        const std::filesystem::path currentDirectory = std::filesystem::current_path();
        for (const std::filesystem::path& kernel : kernels) {
            const std::filesystem::path p = kernel.parent_path();
            std::filesystem::current_path(p);
            const std::string k = kernel.string();
            furnsh_c(k.c_str());
        }

        // Convert the provided object name into a SpiceInt
        SpiceBoolean success = SPICEFALSE;
        SpiceInt id = 0;
        if (std::holds_alternative<std::string>(object)) {
            std::string s = std::get<std::string>(object);
            bods2c_c(s.c_str(), &id, &success);
            if (!success) {
                throw ghoul::RuntimeError(std::format("Error finding object '{}'", s));
            }
        }
        else {
            ghoul_assert(std::holds_alternative<int>(object), "Additional variant type");
            id = std::get<int>(object);
        }


        // Set up variables
        constexpr unsigned int MaxObj = 1024;
        SPICEINT_CELL(ids, MaxObj);
        constexpr unsigned int WinSiz = 16384;
        SPICEDOUBLE_CELL(cover, WinSiz);
        scard_c(0, &cover);

        // Get all objects in the provided kernels
        for (const std::filesystem::path& kernel : kernels) {
            const std::string k = kernel.string();

            constexpr int ArchitectureSize = 128;
            std::array<char, ArchitectureSize> architecture;
            std::memset(architecture.data(), ArchitectureSize, 0);

            constexpr int TypeSize = 16;
            std::array<char, TypeSize> type;
            std::memset(type.data(), TypeSize, 0);

            getfat_c(
                k.c_str(),
                ArchitectureSize,
                TypeSize,
                architecture.data(),
                type.data()
            );

            if (std::string_view(type.data()) != "SPK") {
                // Only SPK kernels are allowed, but we want the user to be able to pass
                // the full list of kernels to this class, which includes other types
                continue;
            }

            spkobj_c(k.c_str(), &ids);
            if (failed_c()) {
                std::string buffer;
                buffer.resize(SpiceErrorBufferSize);
                getmsg_c("LONG", SpiceErrorBufferSize, buffer.data());
                reset_c();
                throw ghoul::RuntimeError(std::format(
                    "Error loading kernel {}. {}", kernel, buffer
                ));
            }
            for (SpiceInt i = 0; i < card_c(&ids); i++) {
                const SpiceInt obj = SPICE_CELL_ELEM_I(&ids, i);

                if (obj != id) {
                    // We only want to find the coverage for the specific identifier
                    continue;
                }

                // Get coverage for the object
                spkcov_c(k.c_str(), obj, &cover);
                if (failed_c()) {
                    std::string buffer;
                    buffer.resize(SpiceErrorBufferSize);
                    getmsg_c("LONG", SpiceErrorBufferSize, buffer.data());
                    reset_c();
                    throw ghoul::RuntimeError(std::format(
                        "Error finding SPK coverage '{}'. {}", kernel, buffer
                    ));
                }

                // Access all of the windows
                const SpiceInt numberOfIntervals = wncard_c(&cover);
                for (SpiceInt j = 0; j < numberOfIntervals; j++) {
                    // Get the endpoints of the jth interval
                    SpiceDouble b = 0.0;
                    SpiceDouble e = 0.0;
                    wnfetd_c(&cover, j, &b, &e);
                    if (failed_c()) {
                        std::string buffer;
                        buffer.resize(SpiceErrorBufferSize);
                        getmsg_c("LONG", SpiceErrorBufferSize, buffer.data());
                        reset_c();
                        throw ghoul::RuntimeError(std::format(
                            "Error finding window {} in SPK '{}'. {}", j, kernel, buffer
                        ));
                    }

                    res.emplace_back(b, e);
                }
            }
        }


        // We no longer need to have need for the kernel being loaded
        for (const std::filesystem::path& kernel : kernels) {
            const std::filesystem::path p = kernel.parent_path();
            std::filesystem::current_path(p);
            const std::string k = kernel.string();
            unload_c(k.c_str());
        }
        std::filesystem::current_path(currentDirectory);


        return res;
    }

    std::vector<openspace::TimeRange> extractTimeFramesCK(
                                        const std::vector<std::filesystem::path>& kernels,
                                             const std::variant<std::string, int>& object)
    {
        using namespace openspace;
        std::vector<TimeRange> res;

        std::filesystem::path lsk = SpiceManager::leapSecondKernel();
        const std::string l = lsk.string();
        furnsh_c(l.c_str());

        // Load the kernel to be able to resolve the provided object name
        const std::filesystem::path currentDirectory = std::filesystem::current_path();

        for (const std::filesystem::path& kernel : kernels) {
            const std::filesystem::path p = kernel.parent_path();
            std::filesystem::current_path(p);
            const std::string k = kernel.string();
            furnsh_c(k.c_str());
        }

        // Convert the provided reference name into a SpiceInt
        SpiceBoolean success = SPICEFALSE;
        SpiceInt id = 0;
        if (std::holds_alternative<std::string>(object)) {
            std::string s = std::get<std::string>(object);
            bods2c_c(s.c_str(), &id, &success);
            if (!success) {
                throw ghoul::RuntimeError(std::format("Error finding object '{}'", s));
            }
        }
        else {
            ghoul_assert(std::holds_alternative<int>(object), "Additional variant type");
            id = std::get<int>(object);
        }


        // Set up variables
        constexpr unsigned int MaxObj = 1024;
        SPICEINT_CELL(ids, MaxObj);
        constexpr unsigned int WinSiz = 16384;
        SPICEDOUBLE_CELL(cover, WinSiz);
        scard_c(0, &cover);

        // Get all objects in the provided kernel
        for (const std::filesystem::path& kernel : kernels) {
            const std::string k = kernel.string();

            constexpr int ArchitectureSize = 128;
            std::array<char, ArchitectureSize> architecture;
            std::memset(architecture.data(), ArchitectureSize, 0);

            constexpr int TypeSize = 16;
            std::array<char, TypeSize> type;
            std::memset(type.data(), TypeSize, 0);

            getfat_c(
                k.c_str(),
                ArchitectureSize,
                TypeSize,
                architecture.data(),
                type.data()
            );

            if (std::string_view(type.data()) != "CK") {
                // Since SCLK kernels are allowed as well we can't throw an exception
                // here. We can't even warn about it since the tested spacecraft clock
                // kernels report a type and architecture of '?' which is not helpful.
                continue;
            }

            ckobj_c(k.c_str(), &ids);
            if (failed_c()) {
                std::string buffer;
                buffer.resize(SpiceErrorBufferSize);
                getmsg_c("LONG", SpiceErrorBufferSize, buffer.data());
                reset_c();
                throw ghoul::RuntimeError(std::format(
                    "Error loading kernel {}. {}", kernel, buffer
                ));
            }
            for (SpiceInt i = 0; i < card_c(&ids); i++) {
                const SpiceInt frame = SPICE_CELL_ELEM_I(&ids, i);

                if (frame != id) {
                    // We only want to find the coverage for the specific identifier
                    continue;
                }

                // Get coverage for the object
                ckcov_c(k.c_str(), frame, SPICEFALSE, "SEGMENT", 0.0, "TDB", &cover);
                if (failed_c()) {
                    std::string buffer;
                    buffer.resize(SpiceErrorBufferSize);
                    getmsg_c("LONG", SpiceErrorBufferSize, buffer.data());
                    reset_c();
                    throw ghoul::RuntimeError(std::format(
                        "Error finding CK coverage '{}'. {}", kernel, buffer
                    ));
                }

                // Access all of the windows
                const SpiceInt numberOfIntervals = wncard_c(&cover);
                for (SpiceInt j = 0; j < numberOfIntervals; j++) {
                    // Get the endpoints of the jth interval
                    SpiceDouble b = 0.0;
                    SpiceDouble e = 0.0;
                    wnfetd_c(&cover, j, &b, &e);
                    if (failed_c()) {
                        std::string buffer;
                        buffer.resize(SpiceErrorBufferSize);
                        getmsg_c("LONG", SpiceErrorBufferSize, buffer.data());
                        reset_c();
                        throw ghoul::RuntimeError(std::format(
                            "Error finding window {} in SPK '{}'. {}", j, kernel, buffer
                        ));
                    }

                    res.emplace_back(b, e);
                }
            }
        }

        // We no longer need to have need for the kernel being loaded
        for (const std::filesystem::path& kernel : kernels) {
            const std::filesystem::path p = kernel.parent_path();
            std::filesystem::current_path(p);
            const std::string k = kernel.string();
            unload_c(k.c_str());
        }
        unload_c(l.c_str());
        std::filesystem::current_path(currentDirectory);


        return res;
    }

    void normalizeTimeRanges(std::vector<openspace::TimeRange>& ranges) {
        using namespace openspace;

        if (ranges.size() <= 1) {
            // Nothing to do here if there is 0 or 1 elements in the vector
            return;
        }

        // 1. Sort time frames based on their beginning time. If the beginning times are
        // the same, sort by the end date instead
        std::sort(
            ranges.begin(),
            ranges.end(),
            [](const TimeRange& lhs, const TimeRange& rhs) {
                return lhs.start == rhs.start ? lhs.end < rhs.end : lhs.start < lhs.start;
            }
        );

        // 2. If `i`'s end time is after `i+1`'s begin time, we can merge these two
        ghoul_assert(ranges.size() > 1, "Too few items. Possible underflow");
        for (size_t i = 0; i < ranges.size() - 1; i++) {
            TimeRange& curr = ranges[i];
            TimeRange& next = ranges[i + 1];

            if (curr.end >= next.start) {
                // Include the next with the current
                curr.include(next);

                // Remove the next as we have subsumed it
                ranges.erase(ranges.begin() + i + 1);
            }
        }
    }

    // This `TimeFrame` class determines its time ranges based on the set of provided
    // SPICE kernels. Any number of SPK (for position information) or CK (for orientation
    // information) kernels can be specified together with a SPICE object name (for
    // position information) or the name of a valid reference frame (for orientation
    // information). For more information about Spice kernels, windows, or IDs, see the
    // required reading documentation from NAIF:
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/kernel.html
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/spk.html
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/ck.html
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/time.html
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/windows.html
    //
    // Note that for the CK kernels, a valid CK kernel as well as the corresponding SCLK
    // kernel must be provided as the latter is required to be able to interpret the time
    // codes of the former. For this reason it is not possible to provide only a single
    // kernel to the CK struct in this class.
    //
    // The resulting validity of the time frame is based on the following conditions:
    //
    //   1. If either SPK or CK (but not both) are specified, the time frame depends on
    //      the union of all windows within all kernels that were provided. This means
    //      that if the simulation time is within any time where the kernel has data for
    //      the provided object, the TimeFrame will be valid.
    //   2. If SPK and CK kernels are both specified, the time range validity for SPK and
    //      CK kernels are calculated separately, but both results must be valid to result
    //      in a valid time frame. This means that if only position data is available but
    //      not orientation data, the time frame is invalid. Only if positional and
    //      orientation data is available, then the TimeFrame will be valid.
    //   3. If neither SPK nor CK kernels are specified, the creation of the `TimeFrame`
    //      will fail.
    struct [[codegen::Dictionary(TimeFrameKernel)]] Parameters {
        // Specifies information about the kernels and object name used to extract the
        // times when positional information for the provided object is available.
        struct SPK {
            // The path to the kernel or list of kernels that should be loaded to extract
            // the positional information. At least one kernel must be a SPK-type kernel.
            // Any other kernel type is ignored.
            std::variant<
                std::filesystem::path, std::vector<std::filesystem::path>
            > kernels;

            // The NAIF name of the object for which the positional information should be
            // extracted
            std::variant<std::string, int> object;
        };
        std::optional<SPK> spk [[codegen::key("SPK")]];

        // Specifies information about the kernels and refrence frame name used to extract
        // the times when positional information for the provided object is available.
        struct CK {
            // The path to the list of kernels that should be loaded to extract
            // orientation information. At least one kernel must be a CK-type kernel and
            // if needed, a SCLK (spacecraft clock) kernel musat be provided. Any other
            // kernel type is ignored.
            std::vector<std::filesystem::path> kernels;

            // The NAIF name of the reference frame for which the times are extacted at
            // which this reference frame has data in the provided kernels
            std::variant<std::string, int> reference;
        };
        std::optional<CK> ck [[codegen::key("CK")]];
    };
#include "timeframekernel_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation TimeFrameKernel::Documentation() {
    return codegen::doc<Parameters>("space_time_frame_kernel");
}

TimeFrameKernel::TimeFrameKernel(const ghoul::Dictionary& dictionary)
    : _initialization(dictionary)
{
    // Baking the dictionary here to detect any error
    codegen::bake<Parameters>(dictionary);
}

bool TimeFrameKernel::initialize() {
    const Parameters p = codegen::bake<Parameters>(_initialization);

    // Either the SPK or the CK variable must be specified
    if (!p.spk.has_value() && !p.ck.has_value()) {
        throw ghoul::RuntimeError(
            "Either the SPK or the CK (or both) values must be specified for the "
            "TimeFrameKernel. Neither was specified."
        );
    }


    // Extract the SPK file/files if they were specified
    if (p.spk.has_value()) {
        std::vector<std::filesystem::path> kernels;
        if (std::holds_alternative<std::filesystem::path>(p.spk->kernels)) {
            kernels = { std::get<std::filesystem::path>(p.spk->kernels) };
        }
        else {
            kernels = std::get<std::vector<std::filesystem::path>>(p.spk->kernels);
        }

        _timeRangesSPK = extractTimeFramesSPK(kernels, p.spk->object);
        LDEBUG(std::format("Extracted {} SPK time ranges", _timeRangesSPK.size()));
    }

    // Extract the CK file/files if they were specified
    if (p.ck.has_value()) {
        _timeRangesCK = extractTimeFramesCK(p.ck->kernels, p.ck->reference);
        LDEBUG(std::format("Extracted {} CK time ranges", _timeRangesCK.size()));
    }

    //
    // Normalize the timeframes to simplify them as much as possible to reduce the length
    // of the vector and improve performance in the `update` lookup
    normalizeTimeRanges(_timeRangesSPK);
    normalizeTimeRanges(_timeRangesCK);

    return true;
}

void TimeFrameKernel::update(const Time& time) {
    // We don't set _isInTimeFrame directly here as that would trigger an invalidation of
    // the property and cause a data transmission every frame. This way, the data is only
    // sent if the value actually changes, which should be rare
    const double t = time.j2000Seconds();
    bool isInTimeFrameSPK = false;
    if (_timeRangesSPK.empty()) {
        isInTimeFrameSPK = true;
    }
    for (const TimeRange& range : _timeRangesSPK) {
        if (range.includes(t)) {
            isInTimeFrameSPK = true;
            break;
        }
    }
    bool isInTimeFrameCK = false;
    if (_timeRangesCK.empty()) {
        isInTimeFrameCK = true;
    }
    for (const TimeRange& range : _timeRangesCK) {
        if (range.includes(t)) {
            isInTimeFrameCK = true;
            break;
        }
    }
    _isInTimeFrame = isInTimeFrameSPK && isInTimeFrameCK;
}

} // namespace
