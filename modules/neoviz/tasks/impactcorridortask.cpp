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

#include <modules/neoviz/tasks/impactcorridortask.h>

#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <filesystem>
#include <format>
#include <numbers>
#include <stb_image_write.h>
#include "SpiceUsr.h"
#include "SpiceZpr.h"

namespace {
    constexpr std::string_view _loggerCat = "ImpactCorridorTask";
    constexpr int MaxEarthRadius = 6378;
    constexpr unsigned int WinSiz = 200;
    constexpr std::string_view TimeStringFormat = "YYYY MON DD HR:MN:SC.###";
    constexpr unsigned int TimeStringLength = 41;
    constexpr double ToDegrees = 180.0 / std::numbers::pi;
    constexpr int DebugMaxVariants = 100;
    constexpr bool DebugMode = false;

    // Offset spice Id to not collide with any existing NAIF id. This is the first NAIF ID
    // for the variants
    constexpr int IdOffset = 1000000;

    std::vector<std::vector<double>> gaussianFilter(int kernelSize, double filterStrength) {
        ghoul_assert(kernelSize % 2 == 1, "Kernel size must be odd");

        double sigma = filterStrength;
        double s = 2.0 * sigma * sigma;
        double sum = 0.0;
        std::vector<std::vector<double>> gaussianKernel = std::vector<std::vector<double>>(
            kernelSize,
            std::vector<double>(kernelSize, 0.0)
        );

        // Generating a kernelSize x kernelSize kernel
        int halfSize = kernelSize / 2;
        double r = 0.0;
        for (int x = -halfSize; x <= halfSize; x++) {
            for (int y = -halfSize; y <= halfSize; y++) {
                r = sqrt(x * x + y * y);
                gaussianKernel[x + halfSize][y + halfSize] = (exp(-(r * r) / s)) / (std::numbers::pi * s);
                sum += gaussianKernel[x + halfSize][y + halfSize];
            }
        }

        // normalising the Kernel
        for (int i = 0; i < 5; ++i) {
            for (int j = 0; j < 5; ++j) {
                gaussianKernel[i][j] /= sum;
            }
        }

        return gaussianKernel;
    }

    struct [[codegen::Dictionary(ImpactCorridorTask)]] Parameters {
        // Path to directory with kernels for the variants of the asteroid to test. The
        // directory can only contain valid kernel files, they will all be loaded.
        std::filesystem::path kernelDirectory [[codegen::directory()]];

        // Path to the output impact corridor map image file. Need to include the
        // .png extension.
        std::string outputFilename;

        // The start of the time interval to search for impacts
        std::string timeIntervalStart;

        // The end of the time interval to search for impacts
        std::string timeIntervalEnd;

        // The width in pixels of the resulting impact corridor map image
        int imageWidth [[codegen::greater(0)]];

        // The height in pixels of the resulting impact corridor map image
        int imageHeight [[codegen::greater(0)]];

        // The distance from Earth center to consider as an impact. If not given, then the
        // maximum radius of Earth is used.
        std::optional<int> impactDistance;

        // The step size in number of seconds used to search for impacts. If not given,
        // then the number of seconds in one day is used.
        std::optional<double> stepSize;
    };
#include "impactcorridortask_codegen.cpp"
} // namespace

namespace openspace::neoviz {

documentation::Documentation ImpactCorridorTask::documentation() {
    return codegen::doc<Parameters>("neoviz_documentation_task");
}

ImpactCorridorTask::ImpactCorridorTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _kernelDirectory = absPath(p.kernelDirectory);
    _outputFilename = absPath(p.outputFilename);
    _timeIntervalStart = p.timeIntervalStart;
    _timeIntervalEnd = p.timeIntervalEnd;
    _imageWidth = p.imageWidth;
    _imageHeight = p.imageHeight;
    _impactDistance = p.impactDistance.value_or(MaxEarthRadius);
    _stepSize = p.stepSize.value_or(spd_c());
}

std::string ImpactCorridorTask::description() {
    return "Return the NEOviz impact corridor map image for the given input";
}

void ImpactCorridorTask::perform(const Task::ProgressCallback& progressCallback) {
    int nVariants = static_cast<int>(std::distance(
        std::filesystem::directory_iterator(_kernelDirectory),
        std::filesystem::directory_iterator()
    ));
    LINFO(std::format("Number of variants: {}", nVariants));
    LINFO("Finding impacts...");

    // Load the general kernels for the major bodies in the solar system
    SpiceManager::ref().loadKernel(absPath("${SYNC}/http/general_spk/2/de430.bsp"));
    SpiceManager::ref().loadKernel(absPath("${SYNC}/http/general_pck/1/pck00011.tpc"));

    // Find the impacts if there are any
    findImpacts(progressCallback, nVariants);

    progressCallback(0.0);
    LINFO("Creating image...");

    // Create a transparent image to then plot the impact coordinates on
    const unsigned int Size = _imageWidth * _imageHeight;
    const unsigned int NChannels = 4;
    std::vector<GLubyte> image = std::vector<GLubyte>(Size * NChannels, 0);

    // Tools to plot the impact coordinates on the image
    // @TODO: Make these parameters configurable
    const int brushSize = 15; // Somewhere between 15 and 20 is good
    const int brushSaturation = 100; // Tweak this to get a good result
    const int halfBrush = static_cast<int>(std::round(brushSize / 2.0));

    // Plot all impact coordinates on the image
    int impactCounter = 0;
    for (const ImpactCoordinate& impact : _impactCoordinates) {
        // Find the pixel in the texture data list for the impact
        double latitudeNorm = (impact.latitude + 90.0) / 180.0;
        double longitudeNorm = (impact.longitude + 180.0) / 360.0;
        int pixelH = static_cast<int>(std::round(latitudeNorm * _imageHeight));
        int pixelW = static_cast<int>(std::round(longitudeNorm * _imageWidth));

        // Draw a circle around the impact point
        for (int w = -halfBrush; w < halfBrush; w++) {
            for (int h = -halfBrush; h < halfBrush; h++) {
                int index = pixelIndex(
                    pixelW + w,
                    pixelH + h,
                    NChannels,
                    _imageWidth,
                    _imageHeight
                );

                // Get the eucledian distance from the center of the brush and calculate
                // the total saturation for this pixel
                double distance = std::sqrt(w * w + h * h);
                int saturation = static_cast<int>(std::round(
                    std::max(brushSaturation * (1.0 - distance / halfBrush), 0.0)
                ));

                for (int c = 0; c < NChannels; ++c) {
                    // Check for overflow
                    if (static_cast<int>(image[index + c]) + saturation > 255) {
                        // Clamp the color in case it will get too saturated
                        image[index + c] = 255;
                        continue;
                    }

                    // Add the saturation to the pixel
                    image[index + c] += saturation;
                }
            }
        }

        ++impactCounter;
        progressCallback(impactCounter / static_cast<float>(_impactCoordinates.size()));
    }

    // @TODO: Apply gaussian filter to the image
    const int kernelSize = (halfBrush % 2 == 1) ? halfBrush : halfBrush + 1;
    const double filterStrength = 1.0;
    const std::vector<std::vector<double>> filter =
        gaussianFilter(kernelSize, filterStrength);

    // @TODO: Apply a transfer function to bring color to the image

    // @TODO: Use the night layer image to estimate impact risk due to population density

    // Create and save the resulting impact map image
    stbi_write_png(
        _outputFilename.string().c_str(),
        _imageWidth,
        _imageHeight,
        NChannels,
        reinterpret_cast<void*>(image.data()),
        0
    );

    // Estimate the impact probability, to compare with the expected value
    LINFO(std::format(
        "Impact probability: {} ({}/{})",
        _impactCoordinates.size() / static_cast<float>(nVariants),
        _impactCoordinates.size(), nVariants
    ));
}

void ImpactCorridorTask::findImpacts(const Task::ProgressCallback& progressCallback,
                                     int nVariants)
{
    // Specify the time range to search in
    SPICEDOUBLE_CELL(timerange, WinSiz);
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
    SPICEDOUBLE_CELL(result, WinSiz);
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
        // impact distance to Earth. This is then considered as an impact.
        gfdist_c(
            variantNAIFName.c_str(), // Name of the target body
            "NONE",                  // Aberration correction flag
            "EARTH",                 // Name of the observing body
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
            LWARNING(std::format("Missing kernel file {:06}", variantId - IdOffset + 1));
            variantId += 2;
            counterVariants += 2;

            SpiceManager::ref().unloadKernel(managerKernelId);
            progressCallback(counterVariants / static_cast<float>(nVariants));
            reset_c();
            continue;
        }

        // Check if the variant impacts or misses Earth
        if (wncard_c(&result) == 0) {
            // No impact
            // LDEBUG(std::format("Variant {} does not impact Earth", variantNAIFName));
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
            "IAU_EARTH",                    // Reference frame of output position vector
            "NONE",                         // Aberration correction flag
            "EARTH",                        // Observing body name, frame of output
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

        // Flip the y axis for the image, north is up
        impactLatitudeDegree *= -1;

        // Add the result to the list of impact coordinates, this will be used to create
        // the impact corridor image map later
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

int ImpactCorridorTask::pixelIndex(int pixelW, int pixelH, int nChannels, int imageWidth,
                                   int imageHeight)
{
    if (pixelH < 0) {
        // Mirror in the noth pole line
        pixelH = std::abs(pixelH);

        // Mirror in the 0 longitude line (Greenwich)
        int greenwich = static_cast<int>(std::round(imageWidth / 2.0));
        pixelW = greenwich + (greenwich - pixelW);

    }
    else if (pixelH >= imageHeight) {
        // Mirror in the south pole line
        pixelH = imageHeight - (pixelH - imageHeight);

        // Mirror in the 0 longitude line (Greenwich)
        int greenwich = static_cast<int>(std::round(imageWidth / 2.0));
        pixelW = greenwich + (greenwich - pixelW);
    }

    if (pixelW < 0) {
        // Go over to the left side of the international daytime line
        pixelW = imageWidth - std::abs(pixelW);
    }
    else if (pixelW >= imageWidth) {
        // Go over to the right side of the international daytime line
        pixelW = pixelW - imageWidth;
    }

    int pixelIndex = nChannels * pixelW + nChannels * imageWidth * pixelH;
    if (pixelIndex < 0 || pixelIndex >= (imageWidth * imageHeight * nChannels)) {
        throw ghoul::RuntimeError("Pixel index out of bounds");
    }
    return pixelIndex;
}

} // namespace openspace::neoviz
