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

#include <modules/neoviz/tasks/findimpactstask.h>

#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <format>
#include <numbers>
#include <fstream>
#include "SpiceUsr.h"
#include "SpiceZpr.h"

namespace {
    constexpr std::string_view _loggerCat = "FindImpactsTask";
    constexpr int MaxEarthRadius = 6378;
    constexpr unsigned int WindowSize = 200;
    constexpr std::string_view TimeStringFormat = "YYYY MON DD HR:MN:SC.###";
    constexpr unsigned int TimeStringLength = 41;
    constexpr double ToDegrees = 180.0 / std::numbers::pi;

    // Offset Spice Id to not collide with any existing NAIF id. This is the first NAIF ID
    // for the variants
    constexpr int IdOffset = 1000000;

    constexpr int DebugMaxVariants = 100;
    constexpr bool DebugMode = false;

    struct [[codegen::Dictionary(FindImpactsTask)]] Parameters {
        // The name of the asteroid to find possible impacts for.
        std::string asteroidName;

        // The name of the tagert to search for impacts against. If not given, then Earth
        // is used as the target. The name must be a valid SPICE id/name.
        std::optional<std::string> targetName;

        // The name of the tagert reference frame to calculate the impact position in. If
        // not given, then IAU_EARTH is used as the reference frame. The name must be a
        // valid SPICE id/name.
        std::optional<std::string> targetFrame;

        // Path to directory with kernels for the variants of the asteroid to test.
        std::string kernelDirectory;

        // Path to the output file that will contain a list of all impactors and their
        // impact information.
        std::string outputFilename;

        // The start of the time interval to search for impacts
        std::string timeIntervalStart;

        // The end of the time interval to search for impacts
        std::string timeIntervalEnd;

        // The distance from the center of the target that is consided as an impact. If
        // not given, then the maximum radius of Earth is used. And variant that comes
        // closer to Earth than this distance, is considered to have impacted.
        std::optional<int> impactDistance;

        // The step size, in seconds, used to search for impacts. If not given, then the
        // number of seconds in a day is used.
        std::optional<double> stepSize;
    };
#include "findimpactstask_codegen.cpp"
} // namespace

namespace openspace::neoviz {

documentation::Documentation FindImpactsTask::documentation() {
    return codegen::doc<Parameters>("neoviz_documentation_findimpactstask");
}

FindImpactsTask::FindImpactsTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _asteroidName = p.asteroidName;
    _targetName = p.targetName.value_or("EARTH");
    _targetFrame = p.targetFrame.value_or("IAU_EARTH");
    _kernelDirectory = absPath(p.kernelDirectory);
    _outputFilename = absPath(p.outputFilename);
    _timeIntervalStart = p.timeIntervalStart;
    _timeIntervalEnd = p.timeIntervalEnd;
    _impactDistance = p.impactDistance.value_or(MaxEarthRadius);
    _stepSize = p.stepSize.value_or(spd_c());
}

std::string FindImpactsTask::description() {
    return "Find possible impact locations for the given input and save them to file";
}

void FindImpactsTask::perform(const Task::ProgressCallback& progressCallback) {
    // Count the number of variants to process in the kernel folder
    int nVariants = static_cast<int>(std::distance(
        std::filesystem::directory_iterator(_kernelDirectory),
        std::filesystem::directory_iterator()
    ));
    ghoul_assert(nVariants > 0, "No variants found in kernel directory");
    LINFO(std::format("Number of variants: {}", nVariants));

    // Load the general kernels for the major bodies in the solar system
    // NOTE (malej 2025-02-20): This requires that the general kernels already have been
    // synced to the local machine. And if the kernels have been updated, then this needs
    // to be updated too.
    SpiceManager::ref().loadKernel(absPath("${SYNC}/http/general_spk/2/de430.bsp"));
    SpiceManager::ref().loadKernel(absPath("${SYNC}/http/general_pck/1/pck00011.tpc"));

    // Find impacts if there are any
    LINFO(std::format("Finding impacts for asteroid {}...", _asteroidName));
    findImpacts(progressCallback, nVariants);
    progressCallback(0.0);

    // Estimate the impact probability, to compare with the expected value
    LINFO(std::format(
        "Impact probability for {}: {:.4f}% ({}/{})",
        _asteroidName,
        (_impactCoordinates.size() / static_cast<float>(nVariants)) * 100.0,
        _impactCoordinates.size(), nVariants
    ));

    // Save the impact coordinates to file
    LINFO(std::format("Saving impact coordinates to file: {}...", _outputFilename));
    writeImpactCoordinates(progressCallback);
}

void FindImpactsTask::findImpacts(const Task::ProgressCallback& progressCallback,
                                     int nVariants)
{
    // Specify the time range to search in
    SPICEDOUBLE_CELL(timerange, WindowSize);
    SpiceDouble startTime;
    SpiceDouble endTime;
    str2et_c(_timeIntervalStart.c_str(), &startTime);
    str2et_c(_timeIntervalEnd.c_str(), &endTime);
    wninsd_c(startTime, endTime, &timerange);
    SpiceDouble impactStart;
    SpiceDouble impactEnd;
    SpiceChar impactTime[TimeStringLength];

    // Variables to calculate and store the impact position
    glm::dvec3 impactPosition = glm::dvec3(0.0);
    SpiceDouble lightTime;
    double impactLatitude = 0.0;
    double impactLongitude = 0.0;
    double impactAltitude = 0.0;
    double impactLatitudeDegree = 0.0;
    double impactLongitudeDegree = 0.0;

    // Loop over all variants of the asteroid
    int counterVariants = 0;
    int variantId = IdOffset;
    std::string variantNAIFName;
    int managerKernelId = 0;
    SPICEDOUBLE_CELL(result, WindowSize);
    for (const auto& variantKernel :
         std::filesystem::directory_iterator(_kernelDirectory))
    {
        if (DebugMode && counterVariants >= DebugMaxVariants) {
            break;
        }

        variantNAIFName = std::to_string(variantId);

        // Make sure it is a kernel file and not anything else
        if (!variantKernel.exists() || variantKernel.is_directory()) {
            LWARNING(std::format(
                "{} is not a file or could not be found", variantKernel.path()
            ));

            ++variantId;
            ++counterVariants;
            SpiceManager::ref().unloadKernel(managerKernelId);
            progressCallback(counterVariants / static_cast<float>(nVariants));
            continue;
        }

        // Load the kernel file for this variant
        managerKernelId = SpiceManager::ref().loadKernel(variantKernel);

        // Get times within the searching timerange when the variant is closer than the
        // impact distance to the target. This is then considered as an impact.
        gfdist_c(
            variantNAIFName.c_str(), // Name of the target body
            "NONE",                  // Aberration correction flag
            _targetName.c_str(),     // Name of the observing body
            "<",                     // Relational operator (example <, =, or >)
            _impactDistance,         // Reference value in km
            0.0,                     // Adjustment value for absolute extrema searches
            _stepSize,               // Step size used for locating extrema and roots
            100,                     // Workspace window interval count
            &timerange,              // Time range for which the search is confined
            &result                  // SPICE window containing results
        );

        // Check if the current kernel file and the current variant ID match
        if (failed_c()) {
            // If not, then there is probably a missing kernel file and we need to skip to
            // the next valid variant
            LWARNING(std::format("Missing kernel file {:07}", variantId - IdOffset + 1));
            variantId += 2;

            SpiceManager::ref().unloadKernel(managerKernelId);
            progressCallback(counterVariants / static_cast<float>(nVariants));
            reset_c();
            continue;
        }

        // Check if the variant impacts or misses the target
        if (wncard_c(&result) == 0) {
            // No impact
            // LDEBUG(std::format("Variant {} does not impact the target", variantNAIFName));
            ++variantId;
            ++counterVariants;
            SpiceManager::ref().unloadKernel(managerKernelId);
            progressCallback(counterVariants / static_cast<float>(nVariants));
            continue;
        }

        // Get the time range for when the variant is located within the impact distance
        wnfetd_c(&result, 0, &impactStart, &impactEnd);

        // Then take the start of that timerange and convert it to a string, this gives
        // the time of impact
        timout_c(
            impactStart,             // Time in J2000 seconds
            TimeStringFormat.data(), // Format for the output string
            TimeStringLength,        // Length of the output string plus 1
            impactTime               // Result
        );

        // Get the cartesian X, Y, Z position of the variant at the impact time
        spkpos_c(
            variantNAIFName.c_str(),        // Target body name to check position for
            impactStart,                    // Time in J2000 seconds
            _targetFrame.c_str(),           // Reference frame of output position vector
            "NONE",                         // Aberration correction flag
            _targetName.c_str(),            // Observing body name, frame of output
            glm::value_ptr(impactPosition), // Output position vector
            &lightTime                      // Resulting light time between the positions
        );

        // Convert the position to a lat, long, alt coordinate
        reclat_c(
            glm::value_ptr(impactPosition), // Cartesian position
            &impactAltitude,                // Altitude
            &impactLongitude,               // Longitude
            &impactLatitude                 // Latitude
        );
        impactLatitudeDegree = impactLatitude * ToDegrees;
        impactLongitudeDegree = impactLongitude * ToDegrees;

        // Flip the latitude, north is up
        impactLatitudeDegree *= -1;

        // Add the result to the list of impact coordinates, this can be used to create
        // an impact corridor image map later
        _impactCoordinates.push_back({
            .id = variantId,
            .time = impactTime,
            .latitude = impactLatitudeDegree,
            .longitude = impactLongitudeDegree
        });

        // Reset
        SpiceManager::ref().unloadKernel(managerKernelId);
        ++variantId;
        ++counterVariants;
        progressCallback(counterVariants / static_cast<float>(nVariants));
    }
}

void FindImpactsTask::writeImpactCoordinates(
                                           const Task::ProgressCallback& progressCallback)
{
    // Create the output file or overwrite it if it already exists
    std::ofstream file(_outputFilename);
    if (!file) {
        LERROR(std::format("Could not open file {} to store impacts", _outputFilename));
        return;
    }

    // Header
    file << std::format("{}\n", _asteroidName);
    file << std::format(
        "{:<10}{:>12}{:>12}{:>30}\n",
        "Varient ID", "Latitude", "Longitude", "Time"
    );

    int impactCounter = 0;
    for (const ImpactCoordinate& impact : _impactCoordinates) {
        // Use 6 decimal points for the coordinates. This will give a precision of
        // approximately 11.1 cm
        file << std::format(
            "{:<10}{:>12.6f}{:>12.6f}{:>30}\n",
            impact.id, impact.latitude, impact.longitude, impact.time
        );

        ++impactCounter;
        progressCallback(impactCounter / static_cast<float>(_impactCoordinates.size()));
    }

    file.close();
}

} // namespace openspace::neoviz
