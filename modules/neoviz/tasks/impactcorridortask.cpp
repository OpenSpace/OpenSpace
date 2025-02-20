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
#include <algorithm>
#include <filesystem>
#include <format>
#include <numbers>
#include <stb_image.h>
#include <stb_image_write.h>
#include "SpiceUsr.h"
#include "SpiceZpr.h"

namespace {
    constexpr std::string_view _loggerCat = "ImpactCorridorTask";
    constexpr int MaxEarthRadius = 6378;
    constexpr unsigned int WindowSize = 200;
    constexpr std::string_view TimeStringFormat = "YYYY MON DD HR:MN:SC.###";
    constexpr unsigned int TimeStringLength = 41;
    constexpr double ToDegrees = 180.0 / std::numbers::pi;
    constexpr unsigned int NChannels = 4;
    constexpr double DefaultBrushSize = 15;
    constexpr double DefaultBrushSaturation = 100;
    constexpr double DefaultFilterStrength = 1;

    // Offset spice Id to not collide with any existing NAIF id. This is the first NAIF ID
    // for the variants
    constexpr int IdOffset = 1000000;

    constexpr int DebugMaxVariants = 100;
    constexpr bool DebugMode = true;

    std::vector<std::vector<double>> gaussianFilter(int kernelSize, double filterStrength) {
        ghoul_assert(kernelSize % 2 == 1, "Kernel size must be odd");

        double standardDeviation = filterStrength;
        double sig = 2.0 * standardDeviation * standardDeviation;
        double sum = 0.0;
        std::vector<std::vector<double>> kernel = std::vector<std::vector<double>>(
            kernelSize,
            std::vector<double>(kernelSize, 0.0)
        );

        // Generating a kernelSize x kernelSize kernel
        int halfSize = kernelSize / 2; // Intentionally rounded down
        int distanceSqared = 0;
        for (int x = -halfSize; x <= halfSize; x++) {
            for (int y = -halfSize; y <= halfSize; y++) {
                distanceSqared = x * x + y * y;
                kernel[x + halfSize][y + halfSize] =
                    (exp(-(distanceSqared) / sig)) / (std::numbers::pi * sig);
                sum += kernel[x + halfSize][y + halfSize];
            }
        }

        // Normalise the kernel
        /*for (int i = 0; i < 5; ++i) {
            for (int j = 0; j < 5; ++j) {
                kernel[i][j] /= sum;
            }
        }*/

        return kernel;
    }

    struct [[codegen::Dictionary(ImpactCorridorTask)]] Parameters {
        // Path to directory with kernels for the variants of the asteroid to test. The
        // directory can only contain valid kernel files, they will all be loaded.
        std::string kernelDirectory;

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

        // The size of the brush used to plot impacts to the image. This is also used as
        // the kernel size for the gaussian filter, which means that this value needs to
        // be odd. If not given, then a default value of 15 is used.
        std::optional<int> brushSize;

        // The saturation of paint on the brush used to plot impacts to the image. If not
        // given, then a default value of 100 is used.
        std::optional<int> brushSaturation;

        // The strength of the gaussian filter used to smooth the impact image. If not
        // given, then a default value of 1.0 is used.
        std::optional<double> filterStrength;

        // Path to the a colormap to use for coloring the impact corridor map.
        std::optional<std::string> colorMap;
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
    _brushSize = p.brushSize.value_or(DefaultBrushSize);
    _brushSaturation = p.brushSaturation.value_or(DefaultBrushSaturation);
    _filterStrength = p.filterStrength.value_or(DefaultFilterStrength);

    if (p.colorMap.has_value()) {
        _hasColorMapFile = true;
        _colorMap = absPath(*p.colorMap);
        ghoul_assert(std::filesystem::exists(_colorMap), "Cannot find color map file");
    }

    ghoul_assert(_brushSize % 2 == 1, "Brush size must be an odd number");
}

std::string ImpactCorridorTask::description() {
    return "Return the NEOviz impact corridor map image for the given input";
}

void ImpactCorridorTask::perform(const Task::ProgressCallback& progressCallback) {
    // Count the number of variants to process in the kernel folder
    int nVariants = static_cast<int>(std::distance(
        std::filesystem::directory_iterator(_kernelDirectory),
        std::filesystem::directory_iterator()
    ));
    ghoul_assert(nVariants > 0, "No variants found in kernel directory");
    LINFO(std::format("Number of variants: {}", nVariants));

    // Load the general kernels for the major bodies in the solar system
    SpiceManager::ref().loadKernel(absPath("${SYNC}/http/general_spk/2/de430.bsp"));
    SpiceManager::ref().loadKernel(absPath("${SYNC}/http/general_pck/1/pck00011.tpc"));

    // Find the impacts if there are any
    LINFO("Finding impacts...");
    findImpacts(progressCallback, nVariants);
    progressCallback(0.0);

    // Settings for the images
    const unsigned int NumPixels = _imageWidth * _imageHeight;
    const unsigned int Size = NumPixels * NChannels;

    // Create a raw impact image that only have information about impact saturation
    LINFO("Creating raw image...");
    std::vector<double> rawData = rawImpactData(progressCallback, NumPixels);
    auto it = std::max_element(rawData.begin(), rawData.end());
    if (it == rawData.end()) {
        LDEBUG("Could not find max element in raw image");
    }
    double maxRawSaturation = *it;
    progressCallback(0.0);

    // Apply a transfer function to bring color to the image
    if (_hasColorMapFile) {
        LINFO("Processing image...");
        int colorMapWidth = 0;
        int colorMapHeight = 0;
        int colorMapChannels = 0;
        unsigned char* colorMapData = stbi_load(
            _colorMap.string().c_str(),
            &colorMapWidth,
            &colorMapHeight,
            &colorMapChannels,
            0
        );
        ghoul_assert(colorMapHeight != 1, "Color map needs to be 1D");

        progressCallback(0.0);
    }

    // @TODO: Use the night layer image to estimate impact risk due to population density

    // Convert the raw image to a normalized image that can be written to file
    LINFO("Finalizing image...");
    std::vector<GLubyte> image = std::vector<GLubyte>(Size, 0);
    for (unsigned int i, p = 0; p < NumPixels && i < Size; ++p, i += NChannels) {
        // Note that this normalizes all channels equally
        double saturation = rawData[p] / maxRawSaturation;

        // Watch out for overflow
        double clampedSaturation = std::min(std::round(saturation * 255.0), 255.0);

        // Save the normalized saturation to all color channels
        for (int c = 0; c < NChannels; ++c) {
            image[i + c] = static_cast<GLubyte>(clampedSaturation);
        }
        progressCallback(p / static_cast<float>(NumPixels));
    }

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
        "Impact probability: {:.4f}% ({}/{})",
        (_impactCoordinates.size() / static_cast<float>(nVariants)) * 100.0,
        _impactCoordinates.size(), nVariants
    ));
}

void ImpactCorridorTask::findImpacts(const Task::ProgressCallback& progressCallback,
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

int ImpactCorridorTask::pixelIndex(int pixelW, int pixelH, int numChannels, int imageWidth,
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

    int pixelIndex = numChannels * pixelW + numChannels * imageWidth * pixelH;
    if (pixelIndex < 0 || pixelIndex >= (imageWidth * imageHeight * numChannels)) {
        throw ghoul::RuntimeError("Pixel index out of bounds");
    }
    return pixelIndex;
}

std::vector<double> ImpactCorridorTask::rawImpactData(
                                           const Task::ProgressCallback& progressCallback,
                                                             const unsigned int numPixels)
{
    // Create a pixel list to then plot the impact coordinates on
    std::vector<double> rawData = std::vector<double>(numPixels, 0.0);

    // Tools to plot the impact coordinates on the pixel list
    const int brushRadius = _brushSize / 2; // Intentionally rounded down

    // Use a gaussian kernel to create a smooth circle around the impact location
    const std::vector<std::vector<double>> filter =
        gaussianFilter(_brushSize, _filterStrength);

    // Plot all impact coordinates on the pixel list
    int impactCounter = 0;
    for (const ImpactCoordinate& impact : _impactCoordinates) {
        // Find the pixel in the texture data list for the impact
        double latitudeNorm = (impact.latitude + 90.0) / 180.0;
        double longitudeNorm = (impact.longitude + 180.0) / 360.0;
        int pixelH = static_cast<int>(std::round(latitudeNorm * _imageHeight));
        int pixelW = static_cast<int>(std::round(longitudeNorm * _imageWidth));

        // Draw a circle around the impact point
        for (int w = -brushRadius; w <= brushRadius; w++) {
            for (int h = -brushRadius; h <= brushRadius; h++) {
                int index = pixelIndex(
                    pixelW + w,
                    pixelH + h,
                    1,
                    _imageWidth,
                    _imageHeight
                );

                // Get the eucledian distance from the center of the brush and calculate
                // the total saturation for this pixel
                /*double distance = std::sqrt(w * w + h * h);
                double saturation =
                    std::max(_brushSaturation * (1.0 - distance / _brushRadius), 0.0);*/

                // Use the gaussian kernel to create a smooth circle around the impact
                double saturation =
                    _brushSaturation * filter[w + brushRadius][h + brushRadius];

                // Add the saturation to the pixel data
                rawData[index] += saturation;
            }
        }

        ++impactCounter;
        progressCallback(impactCounter / static_cast<float>(_impactCoordinates.size()));
    }

    return rawData;
}

} // namespace openspace::neoviz
